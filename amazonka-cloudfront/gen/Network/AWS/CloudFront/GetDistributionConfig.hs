{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a distribution.
module Network.AWS.CloudFront.GetDistributionConfig
    (
    -- * Request
      GetDistributionConfig
    -- ** Request constructor
    , getDistributionConfig
    -- ** Request lenses
    , gdcId

    -- * Response
    , GetDistributionConfigResponse
    -- ** Response constructor
    , getDistributionConfigResponse
    -- ** Response lenses
    , gdcrDistributionConfig
    , gdcrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetDistributionConfig = GetDistributionConfig
    { _gdcId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetDistributionConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcId' @::@ 'Text'
--
getDistributionConfig :: Text -- ^ 'gdcId'
                      -> GetDistributionConfig
getDistributionConfig p1 = GetDistributionConfig
    { _gdcId = p1
    }

-- | The distribution's id.
gdcId :: Lens' GetDistributionConfig Text
gdcId = lens _gdcId (\s a -> s { _gdcId = a })

instance ToPath GetDistributionConfig where
    toPath GetDistributionConfig{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _gdcId
        , "/config"
        ]

instance ToQuery GetDistributionConfig where
    toQuery = const mempty

instance ToHeaders GetDistributionConfig

data GetDistributionConfigResponse = GetDistributionConfigResponse
    { _gdcrDistributionConfig :: Maybe DistributionConfig
    , _gdcrETag               :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetDistributionConfigResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrDistributionConfig' @::@ 'Maybe' 'DistributionConfig'
--
-- * 'gdcrETag' @::@ 'Maybe' 'Text'
--
getDistributionConfigResponse :: GetDistributionConfigResponse
getDistributionConfigResponse = GetDistributionConfigResponse
    { _gdcrDistributionConfig = Nothing
    , _gdcrETag               = Nothing
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

    request  = get
    response = xmlResponse $ \h x -> GetDistributionConfigResponse
        <$> x %| "DistributionConfig"
        <*> h ~:? "ETag"
