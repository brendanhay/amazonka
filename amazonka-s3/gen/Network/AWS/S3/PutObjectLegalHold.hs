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
-- Module      : Network.AWS.S3.PutObjectLegalHold
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a Legal Hold configuration to the specified object.
--
--
module Network.AWS.S3.PutObjectLegalHold
    (
    -- * Creating a Request
      putObjectLegalHold
    , PutObjectLegalHold
    -- * Request Lenses
    , polhLegalHold
    , polhVersionId
    , polhRequestPayer
    , polhContentMD5
    , polhBucket
    , polhKey

    -- * Destructuring the Response
    , putObjectLegalHoldResponse
    , PutObjectLegalHoldResponse
    -- * Response Lenses
    , polhrsRequestCharged
    , polhrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putObjectLegalHold' smart constructor.
data PutObjectLegalHold = PutObjectLegalHold'
  { _polhLegalHold    :: !(Maybe ObjectLockLegalHold)
  , _polhVersionId    :: !(Maybe ObjectVersionId)
  , _polhRequestPayer :: !(Maybe RequestPayer)
  , _polhContentMD5   :: !(Maybe Text)
  , _polhBucket       :: !BucketName
  , _polhKey          :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectLegalHold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'polhLegalHold' - Container element for the Legal Hold configuration you want to apply to the specified object.
--
-- * 'polhVersionId' - The version ID of the object that you want to place a Legal Hold on.
--
-- * 'polhRequestPayer' - Undocumented member.
--
-- * 'polhContentMD5' - The MD5 hash for the request body.
--
-- * 'polhBucket' - The bucket containing the object that you want to place a Legal Hold on.
--
-- * 'polhKey' - The key name for the object that you want to place a Legal Hold on.
putObjectLegalHold
    :: BucketName -- ^ 'polhBucket'
    -> ObjectKey -- ^ 'polhKey'
    -> PutObjectLegalHold
putObjectLegalHold pBucket_ pKey_ =
  PutObjectLegalHold'
    { _polhLegalHold = Nothing
    , _polhVersionId = Nothing
    , _polhRequestPayer = Nothing
    , _polhContentMD5 = Nothing
    , _polhBucket = pBucket_
    , _polhKey = pKey_
    }


-- | Container element for the Legal Hold configuration you want to apply to the specified object.
polhLegalHold :: Lens' PutObjectLegalHold (Maybe ObjectLockLegalHold)
polhLegalHold = lens _polhLegalHold (\ s a -> s{_polhLegalHold = a})

-- | The version ID of the object that you want to place a Legal Hold on.
polhVersionId :: Lens' PutObjectLegalHold (Maybe ObjectVersionId)
polhVersionId = lens _polhVersionId (\ s a -> s{_polhVersionId = a})

-- | Undocumented member.
polhRequestPayer :: Lens' PutObjectLegalHold (Maybe RequestPayer)
polhRequestPayer = lens _polhRequestPayer (\ s a -> s{_polhRequestPayer = a})

-- | The MD5 hash for the request body.
polhContentMD5 :: Lens' PutObjectLegalHold (Maybe Text)
polhContentMD5 = lens _polhContentMD5 (\ s a -> s{_polhContentMD5 = a})

-- | The bucket containing the object that you want to place a Legal Hold on.
polhBucket :: Lens' PutObjectLegalHold BucketName
polhBucket = lens _polhBucket (\ s a -> s{_polhBucket = a})

-- | The key name for the object that you want to place a Legal Hold on.
polhKey :: Lens' PutObjectLegalHold ObjectKey
polhKey = lens _polhKey (\ s a -> s{_polhKey = a})

instance AWSRequest PutObjectLegalHold where
        type Rs PutObjectLegalHold =
             PutObjectLegalHoldResponse
        request = putXML s3
        response
          = receiveEmpty
              (\ s h x ->
                 PutObjectLegalHoldResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s)))

instance Hashable PutObjectLegalHold where

instance NFData PutObjectLegalHold where

instance ToElement PutObjectLegalHold where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}LegalHold"
              .
              _polhLegalHold

instance ToHeaders PutObjectLegalHold where
        toHeaders PutObjectLegalHold'{..}
          = mconcat
              ["x-amz-request-payer" =# _polhRequestPayer,
               "Content-MD5" =# _polhContentMD5]

instance ToPath PutObjectLegalHold where
        toPath PutObjectLegalHold'{..}
          = mconcat ["/", toBS _polhBucket, "/", toBS _polhKey]

instance ToQuery PutObjectLegalHold where
        toQuery PutObjectLegalHold'{..}
          = mconcat
              ["versionId" =: _polhVersionId, "legal-hold"]

-- | /See:/ 'putObjectLegalHoldResponse' smart constructor.
data PutObjectLegalHoldResponse = PutObjectLegalHoldResponse'
  { _polhrsRequestCharged :: !(Maybe RequestCharged)
  , _polhrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectLegalHoldResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'polhrsRequestCharged' - Undocumented member.
--
-- * 'polhrsResponseStatus' - -- | The response status code.
putObjectLegalHoldResponse
    :: Int -- ^ 'polhrsResponseStatus'
    -> PutObjectLegalHoldResponse
putObjectLegalHoldResponse pResponseStatus_ =
  PutObjectLegalHoldResponse'
    {_polhrsRequestCharged = Nothing, _polhrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
polhrsRequestCharged :: Lens' PutObjectLegalHoldResponse (Maybe RequestCharged)
polhrsRequestCharged = lens _polhrsRequestCharged (\ s a -> s{_polhrsRequestCharged = a})

-- | -- | The response status code.
polhrsResponseStatus :: Lens' PutObjectLegalHoldResponse Int
polhrsResponseStatus = lens _polhrsResponseStatus (\ s a -> s{_polhrsResponseStatus = a})

instance NFData PutObjectLegalHoldResponse where
