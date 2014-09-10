{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a distribution.
module Network.AWS.CloudFront
    (
    -- * Request
      GetDistributionConfig
    -- ** Request constructor
    , mkGetDistributionConfig
    -- ** Request lenses
    , gdcId

    -- * Response
    , GetDistributionConfigResponse
    -- ** Response constructor
    , mkGetDistributionConfigResponse
    -- ** Response lenses
    , gdcrDistributionConfig
    , gdcrETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get a distribution configuration.
newtype GetDistributionConfig = GetDistributionConfig
    { _gdcId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistributionConfig' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkGetDistributionConfig :: Text -- ^ 'gdcId'
                        -> GetDistributionConfig
mkGetDistributionConfig p1 = GetDistributionConfig
    { _gdcId = p1
    }

-- | The distribution's id.
gdcId :: Lens' GetDistributionConfig Text
gdcId = lens _gdcId (\s a -> s { _gdcId = a })

instance ToPath GetDistributionConfig

instance ToQuery GetDistributionConfig

instance ToHeaders GetDistributionConfig

instance ToXML GetDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionConfigRequest"

-- | The returned result of the corresponding request.
data GetDistributionConfigResponse = GetDistributionConfigResponse
    { _gdcrDistributionConfig :: Maybe DistributionConfig
    , _gdcrETag :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistributionConfigResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DistributionConfig ::@ @Maybe DistributionConfig@
--
-- * @ETag ::@ @Maybe Text@
--
mkGetDistributionConfigResponse :: GetDistributionConfigResponse
mkGetDistributionConfigResponse = GetDistributionConfigResponse
    { _gdcrDistributionConfig = Nothing
    , _gdcrETag = Nothing
    }

-- | The distribution's configuration information.
gdcrDistributionConfig :: Lens' GetDistributionConfigResponse (Maybe DistributionConfig)
gdcrDistributionConfig =
    lens _gdcrDistributionConfig (\s a -> s { _gdcrDistributionConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcrETag :: Lens' GetDistributionConfigResponse (Maybe Text)
gdcrETag = lens _gdcrETag (\s a -> s { _gdcrETag = a })

instance AWSRequest GetDistributionConfig where
    type Sv GetDistributionConfig = CloudFront
    type Rs GetDistributionConfig = GetDistributionConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionConfigResponse
            <*> xml %|? "DistributionConfig"
            <*> hs ~:? "ETag"
