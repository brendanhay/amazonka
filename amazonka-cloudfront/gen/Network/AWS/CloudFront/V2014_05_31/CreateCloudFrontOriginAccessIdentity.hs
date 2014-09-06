{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new origin access identity.
module Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , mkCreateCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ccfoairsCloudFrontOriginAccessIdentity
    , ccfoairsLocation
    , ccfoairsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create a new origin access identity.
newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCloudFrontOriginAccessIdentity' request.
mkCreateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
                                       -> CreateCloudFrontOriginAccessIdentity
mkCreateCloudFrontOriginAccessIdentity p1 = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig = p1
    }
{-# INLINE mkCreateCloudFrontOriginAccessIdentity #-}

-- | The origin access identity's configuration information.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig =
    lens _ccfoaiCloudFrontOriginAccessIdentityConfig
         (\s a -> s { _ccfoaiCloudFrontOriginAccessIdentityConfig = a })
{-# INLINE ccfoaiCloudFrontOriginAccessIdentityConfig #-}

instance ToPath CreateCloudFrontOriginAccessIdentity where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToXML CreateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateCloudFrontOriginAccessIdentityRequest"

-- | The returned result of the corresponding request.
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoairsCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ccfoairsLocation :: Maybe Text
    , _ccfoairsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The origin access identity's information.
ccfoairsCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ccfoairsCloudFrontOriginAccessIdentity =
    lens _ccfoairsCloudFrontOriginAccessIdentity
         (\s a -> s { _ccfoairsCloudFrontOriginAccessIdentity = a })
{-# INLINE ccfoairsCloudFrontOriginAccessIdentity #-}

-- | The fully qualified URI of the new origin access identity just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoairsLocation :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairsLocation =
    lens _ccfoairsLocation (\s a -> s { _ccfoairsLocation = a })
{-# INLINE ccfoairsLocation #-}

-- | The current version of the origin access identity created.
ccfoairsETag :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairsETag = lens _ccfoairsETag (\s a -> s { _ccfoairsETag = a })
{-# INLINE ccfoairsETag #-}

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Sv CreateCloudFrontOriginAccessIdentity = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
