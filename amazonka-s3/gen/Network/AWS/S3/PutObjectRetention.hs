{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectRetention
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Retention configuration on an object.
--
--
module Network.AWS.S3.PutObjectRetention
    (
    -- * Creating a Request
      putObjectRetention
    , PutObjectRetention
    -- * Request Lenses
    , porRetention
    , porVersionId
    , porRequestPayer
    , porContentMD5
    , porBypassGovernanceRetention
    , porBucket
    , porKey

    -- * Destructuring the Response
    , putObjectRetentionResponse
    , PutObjectRetentionResponse
    -- * Response Lenses
    , porrsRequestCharged
    , porrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putObjectRetention' smart constructor.
data PutObjectRetention = PutObjectRetention'
  { _porRetention                 :: !(Maybe ObjectLockRetention)
  , _porVersionId                 :: !(Maybe ObjectVersionId)
  , _porRequestPayer              :: !(Maybe RequestPayer)
  , _porContentMD5                :: !(Maybe Text)
  , _porBypassGovernanceRetention :: !(Maybe Bool)
  , _porBucket                    :: !BucketName
  , _porKey                       :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'porRetention' - The container element for the Object Retention configuration.
--
-- * 'porVersionId' - The version ID for the object that you want to apply this Object Retention configuration to.
--
-- * 'porRequestPayer' - Undocumented member.
--
-- * 'porContentMD5' - The MD5 hash for the request body.
--
-- * 'porBypassGovernanceRetention' - Indicates whether this operation should bypass Governance-mode restrictions.j
--
-- * 'porBucket' - The bucket that contains the object you want to apply this Object Retention configuration to.
--
-- * 'porKey' - The key name for the object that you want to apply this Object Retention configuration to.
putObjectRetention
    :: BucketName -- ^ 'porBucket'
    -> ObjectKey -- ^ 'porKey'
    -> PutObjectRetention
putObjectRetention pBucket_ pKey_ =
  PutObjectRetention'
    { _porRetention = Nothing
    , _porVersionId = Nothing
    , _porRequestPayer = Nothing
    , _porContentMD5 = Nothing
    , _porBypassGovernanceRetention = Nothing
    , _porBucket = pBucket_
    , _porKey = pKey_
    }


-- | The container element for the Object Retention configuration.
porRetention :: Lens' PutObjectRetention (Maybe ObjectLockRetention)
porRetention = lens _porRetention (\ s a -> s{_porRetention = a})

-- | The version ID for the object that you want to apply this Object Retention configuration to.
porVersionId :: Lens' PutObjectRetention (Maybe ObjectVersionId)
porVersionId = lens _porVersionId (\ s a -> s{_porVersionId = a})

-- | Undocumented member.
porRequestPayer :: Lens' PutObjectRetention (Maybe RequestPayer)
porRequestPayer = lens _porRequestPayer (\ s a -> s{_porRequestPayer = a})

-- | The MD5 hash for the request body.
porContentMD5 :: Lens' PutObjectRetention (Maybe Text)
porContentMD5 = lens _porContentMD5 (\ s a -> s{_porContentMD5 = a})

-- | Indicates whether this operation should bypass Governance-mode restrictions.j
porBypassGovernanceRetention :: Lens' PutObjectRetention (Maybe Bool)
porBypassGovernanceRetention = lens _porBypassGovernanceRetention (\ s a -> s{_porBypassGovernanceRetention = a})

-- | The bucket that contains the object you want to apply this Object Retention configuration to.
porBucket :: Lens' PutObjectRetention BucketName
porBucket = lens _porBucket (\ s a -> s{_porBucket = a})

-- | The key name for the object that you want to apply this Object Retention configuration to.
porKey :: Lens' PutObjectRetention ObjectKey
porKey = lens _porKey (\ s a -> s{_porKey = a})

instance AWSRequest PutObjectRetention where
        type Rs PutObjectRetention =
             PutObjectRetentionResponse
        request = putXML s3
        response
          = receiveEmpty
              (\ s h x ->
                 PutObjectRetentionResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s)))

instance Hashable PutObjectRetention where

instance NFData PutObjectRetention where

instance ToElement PutObjectRetention where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Retention"
              .
              _porRetention

instance ToHeaders PutObjectRetention where
        toHeaders PutObjectRetention'{..}
          = mconcat
              ["x-amz-request-payer" =# _porRequestPayer,
               "Content-MD5" =# _porContentMD5,
               "x-amz-bypass-governance-retention" =#
                 _porBypassGovernanceRetention]

instance ToPath PutObjectRetention where
        toPath PutObjectRetention'{..}
          = mconcat ["/", toBS _porBucket, "/", toBS _porKey]

instance ToQuery PutObjectRetention where
        toQuery PutObjectRetention'{..}
          = mconcat ["versionId" =: _porVersionId, "retention"]

-- | /See:/ 'putObjectRetentionResponse' smart constructor.
data PutObjectRetentionResponse = PutObjectRetentionResponse'
  { _porrsRequestCharged :: !(Maybe RequestCharged)
  , _porrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectRetentionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'porrsRequestCharged' - Undocumented member.
--
-- * 'porrsResponseStatus' - -- | The response status code.
putObjectRetentionResponse
    :: Int -- ^ 'porrsResponseStatus'
    -> PutObjectRetentionResponse
putObjectRetentionResponse pResponseStatus_ =
  PutObjectRetentionResponse'
    {_porrsRequestCharged = Nothing, _porrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
porrsRequestCharged :: Lens' PutObjectRetentionResponse (Maybe RequestCharged)
porrsRequestCharged = lens _porrsRequestCharged (\ s a -> s{_porrsRequestCharged = a})

-- | -- | The response status code.
porrsResponseStatus :: Lens' PutObjectRetentionResponse Int
porrsResponseStatus = lens _porrsResponseStatus (\ s a -> s{_porrsResponseStatus = a})

instance NFData PutObjectRetentionResponse where
