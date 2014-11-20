{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
    (
    -- * Request
      GetCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , gcfoairCloudFrontOriginAccessIdentity
    , gcfoairETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetCloudFrontOriginAccessIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoaiId' @::@ 'Text'
--
getCloudFrontOriginAccessIdentity :: Text -- ^ 'gcfoaiId'
                                  -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity p1 = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId = p1
    }

-- | The identity's id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\s a -> s { _gcfoaiId = a })

data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse
    { _gcfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _gcfoairETag                           :: Maybe Text
    } deriving (Eq, Show)

-- | 'GetCloudFrontOriginAccessIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'gcfoairETag' @::@ 'Maybe' 'Text'
--
getCloudFrontOriginAccessIdentityResponse :: GetCloudFrontOriginAccessIdentityResponse
getCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse
    { _gcfoairCloudFrontOriginAccessIdentity = Nothing
    , _gcfoairETag                           = Nothing
    }

-- | The origin access identity's information.
gcfoairCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
gcfoairCloudFrontOriginAccessIdentity =
    lens _gcfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _gcfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity's information. For
-- example: E2QWRUHAPOMQZL.
gcfoairETag :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe Text)
gcfoairETag = lens _gcfoairETag (\s a -> s { _gcfoairETag = a })

instance ToPath GetCloudFrontOriginAccessIdentity where
    toPath GetCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _gcfoaiId
        ]

instance ToQuery GetCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders GetCloudFrontOriginAccessIdentity

instance ToXMLRoot GetCloudFrontOriginAccessIdentity where
    toXMLRoot = const (element "GetCloudFrontOriginAccessIdentity" [])

instance ToXML GetCloudFrontOriginAccessIdentity

instance AWSRequest GetCloudFrontOriginAccessIdentity where
    type Sv GetCloudFrontOriginAccessIdentity = CloudFront
    type Rs GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentityResponse

    request  = get
    response = xmlHeaderResponse $ \h x -> GetCloudFrontOriginAccessIdentityResponse
        <$> x .@? "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"


Some kind of operator / class to check the types whether to continue?
