{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an origin access identity.
module Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity
    (
    -- * Request
      GetCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , mkGetCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , gcfoairsCloudFrontOriginAccessIdentity
    , gcfoairsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get an origin access identity's information.
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCloudFrontOriginAccessIdentity' request.
mkGetCloudFrontOriginAccessIdentity :: Text -- ^ 'gcfoaiId'
                                    -> GetCloudFrontOriginAccessIdentity
mkGetCloudFrontOriginAccessIdentity p1 = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId = p1
    }
{-# INLINE mkGetCloudFrontOriginAccessIdentity #-}

-- | The identity's id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\s a -> s { _gcfoaiId = a })
{-# INLINE gcfoaiId #-}

instance ToPath GetCloudFrontOriginAccessIdentity where
    toPath GetCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toBS _gcfoaiId
        ]

instance ToQuery GetCloudFrontOriginAccessIdentity

instance ToHeaders GetCloudFrontOriginAccessIdentity

instance ToXML GetCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetCloudFrontOriginAccessIdentityRequest"

-- | The returned result of the corresponding request.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse
    { _gcfoairsCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _gcfoairsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The origin access identity's information.
gcfoairsCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
gcfoairsCloudFrontOriginAccessIdentity =
    lens _gcfoairsCloudFrontOriginAccessIdentity
         (\s a -> s { _gcfoairsCloudFrontOriginAccessIdentity = a })
{-# INLINE gcfoairsCloudFrontOriginAccessIdentity #-}

-- | The current version of the origin access identity's information. For
-- example: E2QWRUHAPOMQZL.
gcfoairsETag :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe Text)
gcfoairsETag = lens _gcfoairsETag (\s a -> s { _gcfoairsETag = a })
{-# INLINE gcfoairsETag #-}

instance AWSRequest GetCloudFrontOriginAccessIdentity where
    type Sv GetCloudFrontOriginAccessIdentity = CloudFront
    type Rs GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentityResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "ETag"
