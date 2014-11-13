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
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response constructor
    , createCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response lenses
    , ccfoairCloudFrontOriginAccessIdentity
    , ccfoairETag
    , ccfoairLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype CreateCloudFrontOriginAccessIdentity2014_05_31 = CreateCloudFrontOriginAccessIdentity2014_05_31
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Eq, Show, Generic)

-- | 'CreateCloudFrontOriginAccessIdentity2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoaiCloudFrontOriginAccessIdentityConfig' @::@ 'CloudFrontOriginAccessIdentityConfig'
--
createCloudFrontOriginAccessIdentity2014_05_31 :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
                                               -> CreateCloudFrontOriginAccessIdentity2014_05_31
createCloudFrontOriginAccessIdentity2014_05_31 p1 = CreateCloudFrontOriginAccessIdentity2014_05_31
    { _ccfoaiCloudFrontOriginAccessIdentityConfig = p1
    }

-- | The origin access identity's configuration information.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity2014_05_31 CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig =
    lens _ccfoaiCloudFrontOriginAccessIdentityConfig
        (\s a -> s { _ccfoaiCloudFrontOriginAccessIdentityConfig = a })

instance ToPath CreateCloudFrontOriginAccessIdentity2014_05_31 where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity2014_05_31 where
    toQuery = const mempty

instance ToHeaders CreateCloudFrontOriginAccessIdentity2014_05_31

instance ToBody CreateCloudFrontOriginAccessIdentity2014_05_31 where
    toBody = toBody . encodeXML . _ccfoaiCloudFrontOriginAccessIdentityConfig

data CreateCloudFrontOriginAccessIdentity2014_05_31Response = CreateCloudFrontOriginAccessIdentity2014_05_31Response
    { _ccfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ccfoairETag                           :: Maybe Text
    , _ccfoairLocation                       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateCloudFrontOriginAccessIdentity2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'ccfoairETag' @::@ 'Maybe' 'Text'
--
-- * 'ccfoairLocation' @::@ 'Maybe' 'Text'
--
createCloudFrontOriginAccessIdentity2014_05_31Response :: CreateCloudFrontOriginAccessIdentity2014_05_31Response
createCloudFrontOriginAccessIdentity2014_05_31Response = CreateCloudFrontOriginAccessIdentity2014_05_31Response
    { _ccfoairCloudFrontOriginAccessIdentity = Nothing
    , _ccfoairLocation                       = Nothing
    , _ccfoairETag                           = Nothing
    }

-- | The origin access identity's information.
ccfoairCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentity2014_05_31Response (Maybe CloudFrontOriginAccessIdentity)
ccfoairCloudFrontOriginAccessIdentity =
    lens _ccfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _ccfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity created.
ccfoairETag :: Lens' CreateCloudFrontOriginAccessIdentity2014_05_31Response (Maybe Text)
ccfoairETag = lens _ccfoairETag (\s a -> s { _ccfoairETag = a })

-- | The fully qualified URI of the new origin access identity just created.
-- For example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoairLocation :: Lens' CreateCloudFrontOriginAccessIdentity2014_05_31Response (Maybe Text)
ccfoairLocation = lens _ccfoairLocation (\s a -> s { _ccfoairLocation = a })

instance AWSRequest CreateCloudFrontOriginAccessIdentity2014_05_31 where
    type Sv CreateCloudFrontOriginAccessIdentity2014_05_31 = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity2014_05_31 = CreateCloudFrontOriginAccessIdentity2014_05_31Response

    request  = post
    response = xmlResponse $ \h x -> CreateCloudFrontOriginAccessIdentity2014_05_31Response
        <$> x %| "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"
        <*> h ~:? "Location"
