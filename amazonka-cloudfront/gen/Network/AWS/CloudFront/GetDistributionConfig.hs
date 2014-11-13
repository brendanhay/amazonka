{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      GetDistributionConfig2014_05_31
    -- ** Request constructor
    , getDistributionConfig2014_05_31
    -- ** Request lenses
    , gdcId

    -- * Response
    , GetDistributionConfig2014_05_31Response
    -- ** Response constructor
    , getDistributionConfig2014_05_31Response
    -- ** Response lenses
    , gdcrDistributionConfig
    , gdcrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype GetDistributionConfig2014_05_31 = GetDistributionConfig2014_05_31
    { _gdcId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetDistributionConfig2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcId' @::@ 'Text'
--
getDistributionConfig2014_05_31 :: Text -- ^ 'gdcId'
                                -> GetDistributionConfig2014_05_31
getDistributionConfig2014_05_31 p1 = GetDistributionConfig2014_05_31
    { _gdcId = p1
    }

-- | The distribution's id.
gdcId :: Lens' GetDistributionConfig2014_05_31 Text
gdcId = lens _gdcId (\s a -> s { _gdcId = a })

instance ToPath GetDistributionConfig2014_05_31 where
    toPath GetDistributionConfig2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _gdcId
        , "/config"
        ]

instance ToQuery GetDistributionConfig2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetDistributionConfig2014_05_31

data GetDistributionConfig2014_05_31Response = GetDistributionConfig2014_05_31Response
    { _gdcrDistributionConfig :: Maybe DistributionConfig
    , _gdcrETag               :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetDistributionConfig2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrDistributionConfig' @::@ 'Maybe' 'DistributionConfig'
--
-- * 'gdcrETag' @::@ 'Maybe' 'Text'
--
getDistributionConfig2014_05_31Response :: GetDistributionConfig2014_05_31Response
getDistributionConfig2014_05_31Response = GetDistributionConfig2014_05_31Response
    { _gdcrDistributionConfig = Nothing
    , _gdcrETag               = Nothing
    }

-- | The distribution's configuration information.
gdcrDistributionConfig :: Lens' GetDistributionConfig2014_05_31Response (Maybe DistributionConfig)
gdcrDistributionConfig =
    lens _gdcrDistributionConfig (\s a -> s { _gdcrDistributionConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gdcrETag :: Lens' GetDistributionConfig2014_05_31Response (Maybe Text)
gdcrETag = lens _gdcrETag (\s a -> s { _gdcrETag = a })

instance AWSRequest GetDistributionConfig2014_05_31 where
    type Sv GetDistributionConfig2014_05_31 = CloudFront
    type Rs GetDistributionConfig2014_05_31 = GetDistributionConfig2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetDistributionConfig2014_05_31Response
        <$> x %| "DistributionConfig"
        <*> h ~:? "ETag"
