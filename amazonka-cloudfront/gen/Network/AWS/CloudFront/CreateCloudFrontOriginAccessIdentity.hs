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
      CreateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResult
    -- ** Response constructor
    , createCloudFrontOriginAccessIdentityResult
    -- ** Response lenses
    , ccfoairCloudFrontOriginAccessIdentity
    , ccfoairETag
    , ccfoairLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Eq, Show, Generic)

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

instance ToPath CreateCloudFrontOriginAccessIdentity where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToBody CreateCloudFrontOriginAccessIdentity where
    toBody = toBody . encodeXML . _ccfoaiCloudFrontOriginAccessIdentityConfig

data CreateCloudFrontOriginAccessIdentityResult = CreateCloudFrontOriginAccessIdentityResult
    { _ccfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ccfoairETag                           :: Maybe Text
    , _ccfoairLocation                       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateCloudFrontOriginAccessIdentityResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'ccfoairETag' @::@ 'Maybe' 'Text'
--
-- * 'ccfoairLocation' @::@ 'Maybe' 'Text'
--
createCloudFrontOriginAccessIdentityResult :: CreateCloudFrontOriginAccessIdentityResult
createCloudFrontOriginAccessIdentityResult = CreateCloudFrontOriginAccessIdentityResult
    { _ccfoairCloudFrontOriginAccessIdentity = Nothing
    , _ccfoairLocation                       = Nothing
    , _ccfoairETag                           = Nothing
    }

-- | The origin access identity's information.
ccfoairCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResult (Maybe CloudFrontOriginAccessIdentity)
ccfoairCloudFrontOriginAccessIdentity =
    lens _ccfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _ccfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity created.
ccfoairETag :: Lens' CreateCloudFrontOriginAccessIdentityResult (Maybe Text)
ccfoairETag = lens _ccfoairETag (\s a -> s { _ccfoairETag = a })

-- | The fully qualified URI of the new origin access identity just created.
-- For example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoairLocation :: Lens' CreateCloudFrontOriginAccessIdentityResult (Maybe Text)
ccfoairLocation = lens _ccfoairLocation (\s a -> s { _ccfoairLocation = a })

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Sv CreateCloudFrontOriginAccessIdentity = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResult

    request  = post
    response = const . xmlResponse $ \h x -> CreateCloudFrontOriginAccessIdentityResult
record
