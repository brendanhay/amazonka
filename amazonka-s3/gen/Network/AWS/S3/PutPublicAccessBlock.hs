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
-- Module      : Network.AWS.S3.PutPublicAccessBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies the @PublicAccessBlock@ configuration for an Amazon S3 bucket.
--
--
module Network.AWS.S3.PutPublicAccessBlock
    (
    -- * Creating a Request
      putPublicAccessBlock
    , PutPublicAccessBlock
    -- * Request Lenses
    , ppabContentMD5
    , ppabBucket
    , ppabPublicAccessBlockConfiguration

    -- * Destructuring the Response
    , putPublicAccessBlockResponse
    , PutPublicAccessBlockResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putPublicAccessBlock' smart constructor.
data PutPublicAccessBlock = PutPublicAccessBlock'
  { _ppabContentMD5                     :: !(Maybe Text)
  , _ppabBucket                         :: !BucketName
  , _ppabPublicAccessBlockConfiguration :: !PublicAccessBlockConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPublicAccessBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppabContentMD5' - The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- * 'ppabBucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
--
-- * 'ppabPublicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
putPublicAccessBlock
    :: BucketName -- ^ 'ppabBucket'
    -> PublicAccessBlockConfiguration -- ^ 'ppabPublicAccessBlockConfiguration'
    -> PutPublicAccessBlock
putPublicAccessBlock pBucket_ pPublicAccessBlockConfiguration_ =
  PutPublicAccessBlock'
    { _ppabContentMD5 = Nothing
    , _ppabBucket = pBucket_
    , _ppabPublicAccessBlockConfiguration = pPublicAccessBlockConfiguration_
    }


-- | The MD5 hash of the @PutPublicAccessBlock@ request body.
ppabContentMD5 :: Lens' PutPublicAccessBlock (Maybe Text)
ppabContentMD5 = lens _ppabContentMD5 (\ s a -> s{_ppabContentMD5 = a})

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to set.
ppabBucket :: Lens' PutPublicAccessBlock BucketName
ppabBucket = lens _ppabBucket (\ s a -> s{_ppabBucket = a})

-- | The @PublicAccessBlock@ configuration that you want to apply to this Amazon S3 bucket. You can enable the configuration options in any combination. For more information about when Amazon S3 considers a bucket or object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> in the /Amazon Simple Storage Service Developer Guide/ .
ppabPublicAccessBlockConfiguration :: Lens' PutPublicAccessBlock PublicAccessBlockConfiguration
ppabPublicAccessBlockConfiguration = lens _ppabPublicAccessBlockConfiguration (\ s a -> s{_ppabPublicAccessBlockConfiguration = a})

instance AWSRequest PutPublicAccessBlock where
        type Rs PutPublicAccessBlock =
             PutPublicAccessBlockResponse
        request = putXML s3
        response = receiveNull PutPublicAccessBlockResponse'

instance Hashable PutPublicAccessBlock where

instance NFData PutPublicAccessBlock where

instance ToElement PutPublicAccessBlock where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}PublicAccessBlockConfiguration"
              .
              _ppabPublicAccessBlockConfiguration

instance ToHeaders PutPublicAccessBlock where
        toHeaders PutPublicAccessBlock'{..}
          = mconcat ["Content-MD5" =# _ppabContentMD5]

instance ToPath PutPublicAccessBlock where
        toPath PutPublicAccessBlock'{..}
          = mconcat ["/", toBS _ppabBucket]

instance ToQuery PutPublicAccessBlock where
        toQuery = const (mconcat ["publicAccessBlock"])

-- | /See:/ 'putPublicAccessBlockResponse' smart constructor.
data PutPublicAccessBlockResponse =
  PutPublicAccessBlockResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPublicAccessBlockResponse' with the minimum fields required to make a request.
--
putPublicAccessBlockResponse
    :: PutPublicAccessBlockResponse
putPublicAccessBlockResponse = PutPublicAccessBlockResponse'


instance NFData PutPublicAccessBlockResponse where
