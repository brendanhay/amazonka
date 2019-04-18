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
-- Module      : Network.AWS.S3.GetObjectRetention
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an object's retention settings.
--
--
module Network.AWS.S3.GetObjectRetention
    (
    -- * Creating a Request
      getObjectRetention
    , GetObjectRetention
    -- * Request Lenses
    , gorVersionId
    , gorRequestPayer
    , gorBucket
    , gorKey

    -- * Destructuring the Response
    , getObjectRetentionResponse
    , GetObjectRetentionResponse
    -- * Response Lenses
    , gorrsRetention
    , gorrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getObjectRetention' smart constructor.
data GetObjectRetention = GetObjectRetention'
  { _gorVersionId    :: !(Maybe ObjectVersionId)
  , _gorRequestPayer :: !(Maybe RequestPayer)
  , _gorBucket       :: !BucketName
  , _gorKey          :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorVersionId' - The version ID for the object whose retention settings you want to retrieve.
--
-- * 'gorRequestPayer' - Undocumented member.
--
-- * 'gorBucket' - The bucket containing the object whose retention settings you want to retrieve.
--
-- * 'gorKey' - The key name for the object whose retention settings you want to retrieve.
getObjectRetention
    :: BucketName -- ^ 'gorBucket'
    -> ObjectKey -- ^ 'gorKey'
    -> GetObjectRetention
getObjectRetention pBucket_ pKey_ =
  GetObjectRetention'
    { _gorVersionId = Nothing
    , _gorRequestPayer = Nothing
    , _gorBucket = pBucket_
    , _gorKey = pKey_
    }


-- | The version ID for the object whose retention settings you want to retrieve.
gorVersionId :: Lens' GetObjectRetention (Maybe ObjectVersionId)
gorVersionId = lens _gorVersionId (\ s a -> s{_gorVersionId = a})

-- | Undocumented member.
gorRequestPayer :: Lens' GetObjectRetention (Maybe RequestPayer)
gorRequestPayer = lens _gorRequestPayer (\ s a -> s{_gorRequestPayer = a})

-- | The bucket containing the object whose retention settings you want to retrieve.
gorBucket :: Lens' GetObjectRetention BucketName
gorBucket = lens _gorBucket (\ s a -> s{_gorBucket = a})

-- | The key name for the object whose retention settings you want to retrieve.
gorKey :: Lens' GetObjectRetention ObjectKey
gorKey = lens _gorKey (\ s a -> s{_gorKey = a})

instance AWSRequest GetObjectRetention where
        type Rs GetObjectRetention =
             GetObjectRetentionResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetObjectRetentionResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetObjectRetention where

instance NFData GetObjectRetention where

instance ToHeaders GetObjectRetention where
        toHeaders GetObjectRetention'{..}
          = mconcat ["x-amz-request-payer" =# _gorRequestPayer]

instance ToPath GetObjectRetention where
        toPath GetObjectRetention'{..}
          = mconcat ["/", toBS _gorBucket, "/", toBS _gorKey]

instance ToQuery GetObjectRetention where
        toQuery GetObjectRetention'{..}
          = mconcat ["versionId" =: _gorVersionId, "retention"]

-- | /See:/ 'getObjectRetentionResponse' smart constructor.
data GetObjectRetentionResponse = GetObjectRetentionResponse'
  { _gorrsRetention      :: !(Maybe ObjectLockRetention)
  , _gorrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectRetentionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorrsRetention' - The container element for an object's retention settings.
--
-- * 'gorrsResponseStatus' - -- | The response status code.
getObjectRetentionResponse
    :: Int -- ^ 'gorrsResponseStatus'
    -> GetObjectRetentionResponse
getObjectRetentionResponse pResponseStatus_ =
  GetObjectRetentionResponse'
    {_gorrsRetention = Nothing, _gorrsResponseStatus = pResponseStatus_}


-- | The container element for an object's retention settings.
gorrsRetention :: Lens' GetObjectRetentionResponse (Maybe ObjectLockRetention)
gorrsRetention = lens _gorrsRetention (\ s a -> s{_gorrsRetention = a})

-- | -- | The response status code.
gorrsResponseStatus :: Lens' GetObjectRetentionResponse Int
gorrsResponseStatus = lens _gorrsResponseStatus (\ s a -> s{_gorrsResponseStatus = a})

instance NFData GetObjectRetentionResponse where
