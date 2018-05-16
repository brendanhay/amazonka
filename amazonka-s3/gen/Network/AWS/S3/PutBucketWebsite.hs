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
-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the website configuration for a bucket.
module Network.AWS.S3.PutBucketWebsite
    (
    -- * Creating a Request
      putBucketWebsite
    , PutBucketWebsite
    -- * Request Lenses
    , pbwContentMD5
    , pbwBucket
    , pbwWebsiteConfiguration

    -- * Destructuring the Response
    , putBucketWebsiteResponse
    , PutBucketWebsiteResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketWebsite' smart constructor.
data PutBucketWebsite = PutBucketWebsite'
  { _pbwContentMD5           :: !(Maybe Text)
  , _pbwBucket               :: !BucketName
  , _pbwWebsiteConfiguration :: !WebsiteConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketWebsite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbwContentMD5' - Undocumented member.
--
-- * 'pbwBucket' - Undocumented member.
--
-- * 'pbwWebsiteConfiguration' - Undocumented member.
putBucketWebsite
    :: BucketName -- ^ 'pbwBucket'
    -> WebsiteConfiguration -- ^ 'pbwWebsiteConfiguration'
    -> PutBucketWebsite
putBucketWebsite pBucket_ pWebsiteConfiguration_ =
  PutBucketWebsite'
    { _pbwContentMD5 = Nothing
    , _pbwBucket = pBucket_
    , _pbwWebsiteConfiguration = pWebsiteConfiguration_
    }


-- | Undocumented member.
pbwContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwContentMD5 = lens _pbwContentMD5 (\ s a -> s{_pbwContentMD5 = a})

-- | Undocumented member.
pbwBucket :: Lens' PutBucketWebsite BucketName
pbwBucket = lens _pbwBucket (\ s a -> s{_pbwBucket = a})

-- | Undocumented member.
pbwWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwWebsiteConfiguration = lens _pbwWebsiteConfiguration (\ s a -> s{_pbwWebsiteConfiguration = a})

instance AWSRequest PutBucketWebsite where
        type Rs PutBucketWebsite = PutBucketWebsiteResponse
        request = putXML s3
        response = receiveNull PutBucketWebsiteResponse'

instance Hashable PutBucketWebsite where

instance NFData PutBucketWebsite where

instance ToElement PutBucketWebsite where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}WebsiteConfiguration"
              .
              _pbwWebsiteConfiguration

instance ToHeaders PutBucketWebsite where
        toHeaders PutBucketWebsite'{..}
          = mconcat ["Content-MD5" =# _pbwContentMD5]

instance ToPath PutBucketWebsite where
        toPath PutBucketWebsite'{..}
          = mconcat ["/", toBS _pbwBucket]

instance ToQuery PutBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'putBucketWebsiteResponse' smart constructor.
data PutBucketWebsiteResponse =
  PutBucketWebsiteResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketWebsiteResponse' with the minimum fields required to make a request.
--
putBucketWebsiteResponse
    :: PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse'


instance NFData PutBucketWebsiteResponse where
