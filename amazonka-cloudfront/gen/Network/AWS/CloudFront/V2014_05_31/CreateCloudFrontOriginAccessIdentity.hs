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
    , mkCreateCloudFrontOriginAccessIdentityRequest
    -- ** Request lenses
    , ccfoairCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ccfoaisCloudFrontOriginAccessIdentity
    , ccfoaisLocation
    , ccfoaisETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCloudFrontOriginAccessIdentity' request.
mkCreateCloudFrontOriginAccessIdentityRequest :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoairCloudFrontOriginAccessIdentityConfig'
                                              -> CreateCloudFrontOriginAccessIdentity
mkCreateCloudFrontOriginAccessIdentityRequest p1 = CreateCloudFrontOriginAccessIdentity
    { _ccfoairCloudFrontOriginAccessIdentityConfig = p1
    }
{-# INLINE mkCreateCloudFrontOriginAccessIdentityRequest #-}

newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { _ccfoairCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
      -- ^ The origin access identity's configuration information.
    } deriving (Show, Generic)

-- | The origin access identity's configuration information.
ccfoairCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity (CloudFrontOriginAccessIdentityConfig)
ccfoairCloudFrontOriginAccessIdentityConfig = lens _ccfoairCloudFrontOriginAccessIdentityConfig (\s a -> s { _ccfoairCloudFrontOriginAccessIdentityConfig = a })
{-# INLINE ccfoairCloudFrontOriginAccessIdentityConfig #-}

instance ToPath CreateCloudFrontOriginAccessIdentity where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToXML CreateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateCloudFrontOriginAccessIdentityRequest"

data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoaisCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , _ccfoaisLocation :: Maybe Text
      -- ^ The fully qualified URI of the new origin access identity just
      -- created. For example:
      -- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
      -- 
    , _ccfoaisETag :: Maybe Text
      -- ^ The current version of the origin access identity created.
    } deriving (Show, Generic)

-- | The origin access identity's information.
ccfoaisCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ccfoaisCloudFrontOriginAccessIdentity = lens _ccfoaisCloudFrontOriginAccessIdentity (\s a -> s { _ccfoaisCloudFrontOriginAccessIdentity = a })
{-# INLINE ccfoaisCloudFrontOriginAccessIdentity #-}

-- | The fully qualified URI of the new origin access identity just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoaisLocation :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoaisLocation = lens _ccfoaisLocation (\s a -> s { _ccfoaisLocation = a })
{-# INLINE ccfoaisLocation #-}

-- | The current version of the origin access identity created.
ccfoaisETag :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoaisETag = lens _ccfoaisETag (\s a -> s { _ccfoaisETag = a })
{-# INLINE ccfoaisETag #-}

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Sv CreateCloudFrontOriginAccessIdentity = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
