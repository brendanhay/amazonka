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

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
    (
    -- * Request
      GetCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response lenses
    , gcfoairCloudFrontOriginAccessIdentity
    , gcfoairETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype GetCloudFrontOriginAccessIdentity2014_05_31 = GetCloudFrontOriginAccessIdentity2014_05_31
    { _gcfoaiId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetCloudFrontOriginAccessIdentity2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaiId' @::@ 'Text'
--
getCloudFrontOriginAccessIdentity2014_05_31 :: Text -- ^ 'gcfoaiId'
                                            -> GetCloudFrontOriginAccessIdentity2014_05_31
getCloudFrontOriginAccessIdentity2014_05_31 p1 = GetCloudFrontOriginAccessIdentity2014_05_31
    { _gcfoaiId = p1
    }

-- | The identity's id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity2014_05_31 Text
gcfoaiId = lens _gcfoaiId (\s a -> s { _gcfoaiId = a })

instance ToPath GetCloudFrontOriginAccessIdentity2014_05_31 where
    toPath GetCloudFrontOriginAccessIdentity2014_05_31{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _gcfoaiId
        ]

instance ToQuery GetCloudFrontOriginAccessIdentity2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetCloudFrontOriginAccessIdentity2014_05_31

data GetCloudFrontOriginAccessIdentity2014_05_31Response = GetCloudFrontOriginAccessIdentity2014_05_31Response
    { _gcfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _gcfoairETag                           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetCloudFrontOriginAccessIdentity2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'gcfoairETag' @::@ 'Maybe' 'Text'
--
getCloudFrontOriginAccessIdentity2014_05_31Response :: GetCloudFrontOriginAccessIdentity2014_05_31Response
getCloudFrontOriginAccessIdentity2014_05_31Response = GetCloudFrontOriginAccessIdentity2014_05_31Response
    { _gcfoairCloudFrontOriginAccessIdentity = Nothing
    , _gcfoairETag                           = Nothing
    }

-- | The origin access identity's information.
gcfoairCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentity2014_05_31Response (Maybe CloudFrontOriginAccessIdentity)
gcfoairCloudFrontOriginAccessIdentity =
    lens _gcfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _gcfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity's information. For
-- example: E2QWRUHAPOMQZL.
gcfoairETag :: Lens' GetCloudFrontOriginAccessIdentity2014_05_31Response (Maybe Text)
gcfoairETag = lens _gcfoairETag (\s a -> s { _gcfoairETag = a })

instance AWSRequest GetCloudFrontOriginAccessIdentity2014_05_31 where
    type Sv GetCloudFrontOriginAccessIdentity2014_05_31 = CloudFront
    type Rs GetCloudFrontOriginAccessIdentity2014_05_31 = GetCloudFrontOriginAccessIdentity2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetCloudFrontOriginAccessIdentity2014_05_31Response
        <$> x %| "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"
