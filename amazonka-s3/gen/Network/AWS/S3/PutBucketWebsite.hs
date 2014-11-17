{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the website configuration for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketWebsite.html>
module Network.AWS.S3.PutBucketWebsite
    (
    -- * Request
      PutBucketWebsite
    -- ** Request constructor
    , putBucketWebsite
    -- ** Request lenses
    , pbwBucket
    , pbwContentMD5
    , pbwWebsiteConfiguration

    -- * Response
    , PutBucketWebsiteResponse
    -- ** Response constructor
    , putBucketWebsiteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketWebsite = PutBucketWebsite
    { _pbwBucket               :: Text
    , _pbwContentMD5           :: Maybe Text
    , _pbwWebsiteConfiguration :: WebsiteConfiguration
    } deriving (Eq, Show, Generic)

-- | 'PutBucketWebsite' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbwBucket' @::@ 'Text'
--
-- * 'pbwContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbwWebsiteConfiguration' @::@ 'WebsiteConfiguration'
--
putBucketWebsite :: Text -- ^ 'pbwBucket'
                 -> WebsiteConfiguration -- ^ 'pbwWebsiteConfiguration'
                 -> PutBucketWebsite
putBucketWebsite p1 p2 = PutBucketWebsite
    { _pbwBucket               = p1
    , _pbwWebsiteConfiguration = p2
    , _pbwContentMD5           = Nothing
    }

pbwBucket :: Lens' PutBucketWebsite Text
pbwBucket = lens _pbwBucket (\s a -> s { _pbwBucket = a })

pbwContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwContentMD5 = lens _pbwContentMD5 (\s a -> s { _pbwContentMD5 = a })

pbwWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwWebsiteConfiguration =
    lens _pbwWebsiteConfiguration (\s a -> s { _pbwWebsiteConfiguration = a })

data PutBucketWebsiteResponse = PutBucketWebsiteResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketWebsiteResponse' constructor.
putBucketWebsiteResponse :: PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse

instance AWSRequest PutBucketWebsite where
    type Sv PutBucketWebsite = S3
    type Rs PutBucketWebsite = PutBucketWebsiteResponse

    request  = put
    response = nullResponse PutBucketWebsiteResponse

instance ToPath PutBucketWebsite where
    toPath PutBucketWebsite{..} = mconcat
        [ "/"
        , toText _pbwBucket
        ]

instance ToHeaders PutBucketWebsite where
    toHeaders PutBucketWebsite{..} = mconcat
        [ "Content-MD5" =: _pbwContentMD5
        ]

instance ToQuery PutBucketWebsite where
    toQuery = const "website"

instance ToXML PutBucketWebsite where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "PutBucketWebsite"
