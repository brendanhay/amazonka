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
    , mkGetDistributionConfigRequest
    -- ** Request lenses
    , gdcrId

    -- * Response
    , GetDistributionConfigResponse
    -- ** Response lenses
    , gdcsDistributionConfig
    , gdcsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistributionConfig' request.
mkGetDistributionConfigRequest :: Text -- ^ 'gdcrId'
                               -> GetDistributionConfig
mkGetDistributionConfigRequest p1 = GetDistributionConfig
    { _gdcrId = p1
    }
{-# INLINE mkGetDistributionConfigRequest #-}

newtype GetDistributionConfig = GetDistributionConfig
    { _gdcrId :: Text
      -- ^ The distribution's id.
    } deriving (Show, Generic)

-- | The distribution's id.
gdcrId :: Lens' GetDistributionConfig (Text)
gdcrId = lens _gdcrId (\s a -> s { _gdcrId = a })
{-# INLINE gdcrId #-}

instance ToPath GetDistributionConfig where
    toPath GetDistributionConfig{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _gdcrId
        , "/config"
        ]

instance ToQuery GetDistributionConfig

instance ToHeaders GetDistributionConfig

instance ToXML GetDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionConfigRequest"

data GetDistributionConfigResponse = GetDistributionConfigResponse
    { _gdcsDistributionConfig :: Maybe DistributionConfig
      -- ^ The distribution's configuration information.
    , _gdcsETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The distribution's configuration information.
gdcsDistributionConfig :: Lens' GetDistributionConfigResponse (Maybe DistributionConfig)
gdcsDistributionConfig = lens _gdcsDistributionConfig (\s a -> s { _gdcsDistributionConfig = a })
{-# INLINE gdcsDistributionConfig #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcsETag :: Lens' GetDistributionConfigResponse (Maybe Text)
gdcsETag = lens _gdcsETag (\s a -> s { _gdcsETag = a })
{-# INLINE gdcsETag #-}

instance AWSRequest GetDistributionConfig where
    type Sv GetDistributionConfig = CloudFront
    type Rs GetDistributionConfig = GetDistributionConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionConfigResponse
            <*> xml %|? "DistributionConfig"
            <*> hs ~:? "ETag"
