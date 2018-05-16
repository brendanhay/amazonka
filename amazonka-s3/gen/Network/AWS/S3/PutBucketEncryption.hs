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
-- Module      : Network.AWS.S3.PutBucketEncryption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new server-side encryption configuration (or replaces an existing one, if present).
module Network.AWS.S3.PutBucketEncryption
    (
    -- * Creating a Request
      putBucketEncryption
    , PutBucketEncryption
    -- * Request Lenses
    , pbeContentMD5
    , pbeBucket
    , pbeServerSideEncryptionConfiguration

    -- * Destructuring the Response
    , putBucketEncryptionResponse
    , PutBucketEncryptionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketEncryption' smart constructor.
data PutBucketEncryption = PutBucketEncryption'
  { _pbeContentMD5                        :: !(Maybe Text)
  , _pbeBucket                            :: !BucketName
  , _pbeServerSideEncryptionConfiguration :: !ServerSideEncryptionConfiguration
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbeContentMD5' - The base64-encoded 128-bit MD5 digest of the server-side encryption configuration.
--
-- * 'pbeBucket' - The name of the bucket for which the server-side encryption configuration is set.
--
-- * 'pbeServerSideEncryptionConfiguration' - Undocumented member.
putBucketEncryption
    :: BucketName -- ^ 'pbeBucket'
    -> ServerSideEncryptionConfiguration -- ^ 'pbeServerSideEncryptionConfiguration'
    -> PutBucketEncryption
putBucketEncryption pBucket_ pServerSideEncryptionConfiguration_ =
  PutBucketEncryption'
    { _pbeContentMD5 = Nothing
    , _pbeBucket = pBucket_
    , _pbeServerSideEncryptionConfiguration =
        pServerSideEncryptionConfiguration_
    }


-- | The base64-encoded 128-bit MD5 digest of the server-side encryption configuration.
pbeContentMD5 :: Lens' PutBucketEncryption (Maybe Text)
pbeContentMD5 = lens _pbeContentMD5 (\ s a -> s{_pbeContentMD5 = a})

-- | The name of the bucket for which the server-side encryption configuration is set.
pbeBucket :: Lens' PutBucketEncryption BucketName
pbeBucket = lens _pbeBucket (\ s a -> s{_pbeBucket = a})

-- | Undocumented member.
pbeServerSideEncryptionConfiguration :: Lens' PutBucketEncryption ServerSideEncryptionConfiguration
pbeServerSideEncryptionConfiguration = lens _pbeServerSideEncryptionConfiguration (\ s a -> s{_pbeServerSideEncryptionConfiguration = a})

instance AWSRequest PutBucketEncryption where
        type Rs PutBucketEncryption =
             PutBucketEncryptionResponse
        request = putXML s3
        response = receiveNull PutBucketEncryptionResponse'

instance Hashable PutBucketEncryption where

instance NFData PutBucketEncryption where

instance ToElement PutBucketEncryption where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}ServerSideEncryptionConfiguration"
              .
              _pbeServerSideEncryptionConfiguration

instance ToHeaders PutBucketEncryption where
        toHeaders PutBucketEncryption'{..}
          = mconcat ["Content-MD5" =# _pbeContentMD5]

instance ToPath PutBucketEncryption where
        toPath PutBucketEncryption'{..}
          = mconcat ["/", toBS _pbeBucket]

instance ToQuery PutBucketEncryption where
        toQuery = const (mconcat ["encryption"])

-- | /See:/ 'putBucketEncryptionResponse' smart constructor.
data PutBucketEncryptionResponse =
  PutBucketEncryptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketEncryptionResponse' with the minimum fields required to make a request.
--
putBucketEncryptionResponse
    :: PutBucketEncryptionResponse
putBucketEncryptionResponse = PutBucketEncryptionResponse'


instance NFData PutBucketEncryptionResponse where
