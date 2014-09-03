{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about an origin access identity.
module Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig
    (
    -- * Request
      GetCloudFrontOriginAccessIdentityConfig
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentityConfig
    -- ** Request lenses
    , gcfoaicrId

    -- * Response
    , GetCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response lenses
    , gcfoaicsCloudFrontOriginAccessIdentityConfig
    , gcfoaicsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetCloudFrontOriginAccessIdentityConfig' request.
getCloudFrontOriginAccessIdentityConfig :: Text -- ^ 'gcfoaicrId'
                                        -> GetCloudFrontOriginAccessIdentityConfig
getCloudFrontOriginAccessIdentityConfig p1 = GetCloudFrontOriginAccessIdentityConfig
    { _gcfoaicrId = p1
    }

data GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig
    { _gcfoaicrId :: Text
      -- ^ The identity's id.
    } deriving (Show, Generic)

-- | The identity's id.
gcfoaicrId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetCloudFrontOriginAccessIdentityConfig
    -> f GetCloudFrontOriginAccessIdentityConfig
gcfoaicrId f x =
    (\y -> x { _gcfoaicrId = y })
       <$> f (_gcfoaicrId x)
{-# INLINE gcfoaicrId #-}

instance ToPath GetCloudFrontOriginAccessIdentityConfig where
    toPath GetCloudFrontOriginAccessIdentityConfig{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toBS _gcfoaicrId
        , "/config"
        ]

instance ToQuery GetCloudFrontOriginAccessIdentityConfig

instance ToHeaders GetCloudFrontOriginAccessIdentityConfig

instance ToXML GetCloudFrontOriginAccessIdentityConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetCloudFrontOriginAccessIdentityConfigRequest"

data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse
    { _gcfoaicsCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
      -- ^ The origin access identity's configuration information.
    , _gcfoaicsETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The origin access identity's configuration information.
gcfoaicsCloudFrontOriginAccessIdentityConfig
    :: Functor f
    => (Maybe CloudFrontOriginAccessIdentityConfig
    -> f (Maybe CloudFrontOriginAccessIdentityConfig))
    -> GetCloudFrontOriginAccessIdentityConfigResponse
    -> f GetCloudFrontOriginAccessIdentityConfigResponse
gcfoaicsCloudFrontOriginAccessIdentityConfig f x =
    (\y -> x { _gcfoaicsCloudFrontOriginAccessIdentityConfig = y })
       <$> f (_gcfoaicsCloudFrontOriginAccessIdentityConfig x)
{-# INLINE gcfoaicsCloudFrontOriginAccessIdentityConfig #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicsETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetCloudFrontOriginAccessIdentityConfigResponse
    -> f GetCloudFrontOriginAccessIdentityConfigResponse
gcfoaicsETag f x =
    (\y -> x { _gcfoaicsETag = y })
       <$> f (_gcfoaicsETag x)
{-# INLINE gcfoaicsETag #-}

instance AWSRequest GetCloudFrontOriginAccessIdentityConfig where
    type Sv GetCloudFrontOriginAccessIdentityConfig = CloudFront
    type Rs GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetCloudFrontOriginAccessIdentityConfigResponse
            <*> xml %|? "CloudFrontOriginAccessIdentityConfig"
            <*> hs ~:? "ETag"
