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
-- Module      : Network.AWS.S3.GetBucketEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the server-side encryption configuration of a bucket.
module Network.AWS.S3.GetBucketEncryption
    (
    -- * Creating a Request
      getBucketEncryption
    , GetBucketEncryption
    -- * Request Lenses
    , gbeBucket

    -- * Destructuring the Response
    , getBucketEncryptionResponse
    , GetBucketEncryptionResponse
    -- * Response Lenses
    , gbersServerSideEncryptionConfiguration
    , gbersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketEncryption' smart constructor.
newtype GetBucketEncryption = GetBucketEncryption'
  { _gbeBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbeBucket' - The name of the bucket from which the server-side encryption configuration is retrieved.
getBucketEncryption
    :: BucketName -- ^ 'gbeBucket'
    -> GetBucketEncryption
getBucketEncryption pBucket_ = GetBucketEncryption' {_gbeBucket = pBucket_}


-- | The name of the bucket from which the server-side encryption configuration is retrieved.
gbeBucket :: Lens' GetBucketEncryption BucketName
gbeBucket = lens _gbeBucket (\ s a -> s{_gbeBucket = a})

instance AWSRequest GetBucketEncryption where
        type Rs GetBucketEncryption =
             GetBucketEncryptionResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketEncryptionResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetBucketEncryption where

instance NFData GetBucketEncryption where

instance ToHeaders GetBucketEncryption where
        toHeaders = const mempty

instance ToPath GetBucketEncryption where
        toPath GetBucketEncryption'{..}
          = mconcat ["/", toBS _gbeBucket]

instance ToQuery GetBucketEncryption where
        toQuery = const (mconcat ["encryption"])

-- | /See:/ 'getBucketEncryptionResponse' smart constructor.
data GetBucketEncryptionResponse = GetBucketEncryptionResponse'
  { _gbersServerSideEncryptionConfiguration :: !(Maybe ServerSideEncryptionConfiguration)
  , _gbersResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketEncryptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbersServerSideEncryptionConfiguration' - Undocumented member.
--
-- * 'gbersResponseStatus' - -- | The response status code.
getBucketEncryptionResponse
    :: Int -- ^ 'gbersResponseStatus'
    -> GetBucketEncryptionResponse
getBucketEncryptionResponse pResponseStatus_ =
  GetBucketEncryptionResponse'
    { _gbersServerSideEncryptionConfiguration = Nothing
    , _gbersResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gbersServerSideEncryptionConfiguration :: Lens' GetBucketEncryptionResponse (Maybe ServerSideEncryptionConfiguration)
gbersServerSideEncryptionConfiguration = lens _gbersServerSideEncryptionConfiguration (\ s a -> s{_gbersServerSideEncryptionConfiguration = a})

-- | -- | The response status code.
gbersResponseStatus :: Lens' GetBucketEncryptionResponse Int
gbersResponseStatus = lens _gbersResponseStatus (\ s a -> s{_gbersResponseStatus = a})

instance NFData GetBucketEncryptionResponse where
