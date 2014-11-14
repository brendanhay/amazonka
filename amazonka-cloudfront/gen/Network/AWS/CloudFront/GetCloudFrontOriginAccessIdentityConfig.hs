{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
    (
    -- * Request
      GetCloudFrontOriginAccessIdentityConfig2014_05_31
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentityConfig2014_05_31
    -- ** Request lenses
    , gcfoaicId

    -- * Response
    , GetCloudFrontOriginAccessIdentityConfig2014_05_31Response
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityConfig2014_05_31Response
    -- ** Response lenses
    , gcfoaicrCloudFrontOriginAccessIdentityConfig
    , gcfoaicrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetCloudFrontOriginAccessIdentityConfig2014_05_31 = GetCloudFrontOriginAccessIdentityConfig2014_05_31
    { _gcfoaicId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetCloudFrontOriginAccessIdentityConfig2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicId' @::@ 'Text'
--
getCloudFrontOriginAccessIdentityConfig2014_05_31 :: Text -- ^ 'gcfoaicId'
                                                  -> GetCloudFrontOriginAccessIdentityConfig2014_05_31
getCloudFrontOriginAccessIdentityConfig2014_05_31 p1 = GetCloudFrontOriginAccessIdentityConfig2014_05_31
    { _gcfoaicId = p1
    }

-- | The identity's id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig2014_05_31 Text
gcfoaicId = lens _gcfoaicId (\s a -> s { _gcfoaicId = a })

instance ToPath GetCloudFrontOriginAccessIdentityConfig2014_05_31 where
    toPath GetCloudFrontOriginAccessIdentityConfig2014_05_31{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _gcfoaicId
        , "/config"
        ]

instance ToQuery GetCloudFrontOriginAccessIdentityConfig2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetCloudFrontOriginAccessIdentityConfig2014_05_31

data GetCloudFrontOriginAccessIdentityConfig2014_05_31Response = GetCloudFrontOriginAccessIdentityConfig2014_05_31Response
    { _gcfoaicrCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
    , _gcfoaicrETag                                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetCloudFrontOriginAccessIdentityConfig2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaicrCloudFrontOriginAccessIdentityConfig' @::@ 'Maybe' 'CloudFrontOriginAccessIdentityConfig'
--
-- * 'gcfoaicrETag' @::@ 'Maybe' 'Text'
--
getCloudFrontOriginAccessIdentityConfig2014_05_31Response :: GetCloudFrontOriginAccessIdentityConfig2014_05_31Response
getCloudFrontOriginAccessIdentityConfig2014_05_31Response = GetCloudFrontOriginAccessIdentityConfig2014_05_31Response
    { _gcfoaicrCloudFrontOriginAccessIdentityConfig = Nothing
    , _gcfoaicrETag                                 = Nothing
    }

-- | The origin access identity's configuration information.
gcfoaicrCloudFrontOriginAccessIdentityConfig :: Lens' GetCloudFrontOriginAccessIdentityConfig2014_05_31Response (Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrCloudFrontOriginAccessIdentityConfig =
    lens _gcfoaicrCloudFrontOriginAccessIdentityConfig
        (\s a -> s { _gcfoaicrCloudFrontOriginAccessIdentityConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicrETag :: Lens' GetCloudFrontOriginAccessIdentityConfig2014_05_31Response (Maybe Text)
gcfoaicrETag = lens _gcfoaicrETag (\s a -> s { _gcfoaicrETag = a })

instance AWSRequest GetCloudFrontOriginAccessIdentityConfig2014_05_31 where
    type Sv GetCloudFrontOriginAccessIdentityConfig2014_05_31 = CloudFront
    type Rs GetCloudFrontOriginAccessIdentityConfig2014_05_31 = GetCloudFrontOriginAccessIdentityConfig2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetCloudFrontOriginAccessIdentityConfig2014_05_31Response
        <$> x %| "CloudFrontOriginAccessIdentityConfig"
        <*> h ~:? "ETag"
