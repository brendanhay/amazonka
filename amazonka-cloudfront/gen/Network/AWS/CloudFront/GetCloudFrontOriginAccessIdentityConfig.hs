{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentityConfig.html>
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
    (
    -- * Request
      GetCloudFrontOriginAccessIdentityConfig
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentityConfig
    -- ** Request lenses
    , gcfoaicId

    -- * Response
    , GetCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response lenses
    , gcfoaicrCloudFrontOriginAccessIdentityConfig
    , gcfoaicrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig
    { _gcfoaicId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetCloudFrontOriginAccessIdentityConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicId' @::@ 'Text'
--
getCloudFrontOriginAccessIdentityConfig :: Text -- ^ 'gcfoaicId'
                                        -> GetCloudFrontOriginAccessIdentityConfig
getCloudFrontOriginAccessIdentityConfig p1 = GetCloudFrontOriginAccessIdentityConfig
    { _gcfoaicId = p1
    }

-- | The identity's id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig Text
gcfoaicId = lens _gcfoaicId (\s a -> s { _gcfoaicId = a })

data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse
    { _gcfoaicrCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
    , _gcfoaicrETag                                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetCloudFrontOriginAccessIdentityConfigResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicrCloudFrontOriginAccessIdentityConfig' @::@ 'Maybe' 'CloudFrontOriginAccessIdentityConfig'
--
-- * 'gcfoaicrETag' @::@ 'Maybe' 'Text'
--
getCloudFrontOriginAccessIdentityConfigResponse :: GetCloudFrontOriginAccessIdentityConfigResponse
getCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse
    { _gcfoaicrCloudFrontOriginAccessIdentityConfig = Nothing
    , _gcfoaicrETag                                 = Nothing
    }

-- | The origin access identity's configuration information.
gcfoaicrCloudFrontOriginAccessIdentityConfig :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrCloudFrontOriginAccessIdentityConfig =
    lens _gcfoaicrCloudFrontOriginAccessIdentityConfig
        (\s a -> s { _gcfoaicrCloudFrontOriginAccessIdentityConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicrETag :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe Text)
gcfoaicrETag = lens _gcfoaicrETag (\s a -> s { _gcfoaicrETag = a })

instance ToPath GetCloudFrontOriginAccessIdentityConfig where
    toPath GetCloudFrontOriginAccessIdentityConfig{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _gcfoaicId
        , "/config"
        ]

instance ToQuery GetCloudFrontOriginAccessIdentityConfig where
    toQuery = const mempty

instance ToHeaders GetCloudFrontOriginAccessIdentityConfig

instance ToXML GetCloudFrontOriginAccessIdentityConfig where
    toXML = const (node "GetCloudFrontOriginAccessIdentityConfig" [])

instance AWSRequest GetCloudFrontOriginAccessIdentityConfig where
    type Sv GetCloudFrontOriginAccessIdentityConfig = CloudFront
    type Rs GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfigResponse

    request  = get
    response = xmlHeaderResponse $ \h x -> GetCloudFrontOriginAccessIdentityConfigResponse
        <$> x %| "CloudFrontOriginAccessIdentityConfig"
        <*> h ~:? "ETag"
