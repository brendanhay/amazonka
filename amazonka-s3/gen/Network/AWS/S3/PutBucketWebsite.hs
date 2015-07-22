{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Set the website configuration for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketWebsite.html>
module Network.AWS.S3.PutBucketWebsite
    (
    -- * Request
      PutBucketWebsite
    -- ** Request constructor
    , putBucketWebsite
    -- ** Request lenses
    , pbwrqContentMD5
    , pbwrqBucket
    , pbwrqWebsiteConfiguration

    -- * Response
    , PutBucketWebsiteResponse
    -- ** Response constructor
    , putBucketWebsiteResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketWebsite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbwrqContentMD5'
--
-- * 'pbwrqBucket'
--
-- * 'pbwrqWebsiteConfiguration'
data PutBucketWebsite = PutBucketWebsite'
    { _pbwrqContentMD5           :: !(Maybe Text)
    , _pbwrqBucket               :: !BucketName
    , _pbwrqWebsiteConfiguration :: !WebsiteConfiguration
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketWebsite' smart constructor.
putBucketWebsite :: BucketName -> WebsiteConfiguration -> PutBucketWebsite
putBucketWebsite pBucket pWebsiteConfiguration =
    PutBucketWebsite'
    { _pbwrqContentMD5 = Nothing
    , _pbwrqBucket = pBucket
    , _pbwrqWebsiteConfiguration = pWebsiteConfiguration
    }

-- | FIXME: Undocumented member.
pbwrqContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwrqContentMD5 = lens _pbwrqContentMD5 (\ s a -> s{_pbwrqContentMD5 = a});

-- | FIXME: Undocumented member.
pbwrqBucket :: Lens' PutBucketWebsite BucketName
pbwrqBucket = lens _pbwrqBucket (\ s a -> s{_pbwrqBucket = a});

-- | FIXME: Undocumented member.
pbwrqWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwrqWebsiteConfiguration = lens _pbwrqWebsiteConfiguration (\ s a -> s{_pbwrqWebsiteConfiguration = a});

instance AWSRequest PutBucketWebsite where
        type Sv PutBucketWebsite = S3
        type Rs PutBucketWebsite = PutBucketWebsiteResponse
        request = putXML
        response = receiveNull PutBucketWebsiteResponse'

instance ToElement PutBucketWebsite where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}WebsiteConfiguration"
              .
              _pbwrqWebsiteConfiguration

instance ToHeaders PutBucketWebsite where
        toHeaders PutBucketWebsite'{..}
          = mconcat ["Content-MD5" =# _pbwrqContentMD5]

instance ToPath PutBucketWebsite where
        toPath PutBucketWebsite'{..}
          = mconcat ["/", toText _pbwrqBucket]

instance ToQuery PutBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'putBucketWebsiteResponse' smart constructor.
data PutBucketWebsiteResponse =
    PutBucketWebsiteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketWebsiteResponse' smart constructor.
putBucketWebsiteResponse :: PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse'
