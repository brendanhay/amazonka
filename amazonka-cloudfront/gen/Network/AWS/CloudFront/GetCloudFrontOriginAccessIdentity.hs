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
      GetCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentityResult
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response lenses
    , gcfoairCloudFrontOriginAccessIdentity
    , gcfoairETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetCloudFrontOriginAccessIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaiId' @::@ 'Text'
--
getCloudFrontOriginAccessIdentity2014_05_31 :: Text -- ^ 'gcfoaiId'
                                            -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity2014_05_31 p1 = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId = p1
    }

-- | The identity's id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\s a -> s { _gcfoaiId = a })

instance ToPath GetCloudFrontOriginAccessIdentity where
    toPath GetCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _gcfoaiId
        ]

instance ToQuery GetCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders GetCloudFrontOriginAccessIdentity

data GetCloudFrontOriginAccessIdentityResult = GetCloudFrontOriginAccessIdentityResult
    { _gcfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _gcfoairETag                           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetCloudFrontOriginAccessIdentityResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'gcfoairETag' @::@ 'Maybe' 'Text'
--
getCloudFrontOriginAccessIdentity2014_05_31Response :: GetCloudFrontOriginAccessIdentityResult
getCloudFrontOriginAccessIdentity2014_05_31Response = GetCloudFrontOriginAccessIdentityResult
    { _gcfoairCloudFrontOriginAccessIdentity = Nothing
    , _gcfoairETag                           = Nothing
    }

-- | The origin access identity's information.
gcfoairCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentityResult (Maybe CloudFrontOriginAccessIdentity)
gcfoairCloudFrontOriginAccessIdentity =
    lens _gcfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _gcfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity's information. For
-- example: E2QWRUHAPOMQZL.
gcfoairETag :: Lens' GetCloudFrontOriginAccessIdentityResult (Maybe Text)
gcfoairETag = lens _gcfoairETag (\s a -> s { _gcfoairETag = a })

instance FromXML GetCloudFrontOriginAccessIdentityResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetCloudFrontOriginAccessIdentityResult"
instance AWSRequest GetCloudFrontOriginAccessIdentity where
    type Sv GetCloudFrontOriginAccessIdentity = CloudFront
    type Rs GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentityResult

    request  = get
    response = xmlResponse $ \h x -> GetCloudFrontOriginAccessIdentityResult
        <$> x %| "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"
