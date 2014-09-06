{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a distribution.
module Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig
    (
    -- * Request
      GetDistributionConfig
    -- ** Request constructor
    , mkGetDistributionConfig
    -- ** Request lenses
    , gdcId

    -- * Response
    , GetDistributionConfigResponse
    -- ** Response lenses
    , gdcrsDistributionConfig
    , gdcrsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get a distribution configuration.
newtype GetDistributionConfig = GetDistributionConfig
    { _gdcId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistributionConfig' request.
mkGetDistributionConfig :: Text -- ^ 'gdcId'
                        -> GetDistributionConfig
mkGetDistributionConfig p1 = GetDistributionConfig
    { _gdcId = p1
    }
{-# INLINE mkGetDistributionConfig #-}

-- | The distribution's id.
gdcId :: Lens' GetDistributionConfig Text
gdcId = lens _gdcId (\s a -> s { _gdcId = a })
{-# INLINE gdcId #-}

instance ToPath GetDistributionConfig where
    toPath GetDistributionConfig{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _gdcId
        , "/config"
        ]

instance ToQuery GetDistributionConfig

instance ToHeaders GetDistributionConfig

instance ToXML GetDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionConfigRequest"

-- | The returned result of the corresponding request.
data GetDistributionConfigResponse = GetDistributionConfigResponse
    { _gdcrsDistributionConfig :: Maybe DistributionConfig
    , _gdcrsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The distribution's configuration information.
gdcrsDistributionConfig :: Lens' GetDistributionConfigResponse (Maybe DistributionConfig)
gdcrsDistributionConfig =
    lens _gdcrsDistributionConfig
         (\s a -> s { _gdcrsDistributionConfig = a })
{-# INLINE gdcrsDistributionConfig #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcrsETag :: Lens' GetDistributionConfigResponse (Maybe Text)
gdcrsETag = lens _gdcrsETag (\s a -> s { _gdcrsETag = a })
{-# INLINE gdcrsETag #-}

instance AWSRequest GetDistributionConfig where
    type Sv GetDistributionConfig = CloudFront
    type Rs GetDistributionConfig = GetDistributionConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionConfigResponse
            <*> xml %|? "DistributionConfig"
            <*> hs ~:? "ETag"
