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

-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , createCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ccfoairCloudFrontOriginAccessIdentity
    , ccfoairETag
    , ccfoairLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Eq, Show)

-- | 'CreateCloudFrontOriginAccessIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoaiCloudFrontOriginAccessIdentityConfig' @::@ 'CloudFrontOriginAccessIdentityConfig'
--
createCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
                                     -> CreateCloudFrontOriginAccessIdentity
createCloudFrontOriginAccessIdentity p1 = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig = p1
    }

-- | The origin access identity's configuration information.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig =
    lens _ccfoaiCloudFrontOriginAccessIdentityConfig
        (\s a -> s { _ccfoaiCloudFrontOriginAccessIdentityConfig = a })

data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ccfoairETag                           :: Maybe Text
    , _ccfoairLocation                       :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateCloudFrontOriginAccessIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'ccfoairETag' @::@ 'Maybe' 'Text'
--
-- * 'ccfoairLocation' @::@ 'Maybe' 'Text'
--
createCloudFrontOriginAccessIdentityResponse :: CreateCloudFrontOriginAccessIdentityResponse
createCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoairCloudFrontOriginAccessIdentity = Nothing
    , _ccfoairLocation                       = Nothing
    , _ccfoairETag                           = Nothing
    }

-- | The origin access identity's information.
ccfoairCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ccfoairCloudFrontOriginAccessIdentity =
    lens _ccfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _ccfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity created.
ccfoairETag :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairETag = lens _ccfoairETag (\s a -> s { _ccfoairETag = a })

-- | The fully qualified URI of the new origin access identity just created.
-- For example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoairLocation :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairLocation = lens _ccfoairLocation (\s a -> s { _ccfoairLocation = a })

instance ToPath CreateCloudFrontOriginAccessIdentity where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToXMLRoot CreateCloudFrontOriginAccessIdentity where
    toXMLRoot CreateCloudFrontOriginAccessIdentity{..} = namespace ns "CreateCloudFrontOriginAccessIdentity"
        [ "CloudFrontOriginAccessIdentityConfig" =@ _ccfoaiCloudFrontOriginAccessIdentityConfig
        ]

instance ToXML CreateCloudFrontOriginAccessIdentity

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Sv CreateCloudFrontOriginAccessIdentity = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateCloudFrontOriginAccessIdentityResponse
        <$> x .@? "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"
        <*> h ~:? "Location"
