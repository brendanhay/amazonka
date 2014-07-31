{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update an origin access identity.
module Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateCloudFrontOriginAccessIdentity' request.
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ '_ucfoairCloudFrontOriginAccessIdentityConfig'
                                     -> Text -- ^ '_ucfoairId'
                                     -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity p1 p2 = UpdateCloudFrontOriginAccessIdentity
    { _ucfoairCloudFrontOriginAccessIdentityConfig = p1
    , _ucfoairId = p2
    , _ucfoairIfMatch = Nothing
    }

data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity
    { _ucfoairCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
      -- ^ The identity's configuration information.
    , _ucfoairId :: Text
      -- ^ The identity's id.
    , _ucfoairIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when retrieving the
      -- identity's configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Generic)

instance ToPath UpdateCloudFrontOriginAccessIdentity where
    toPath UpdateCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toBS _ucfoairId
        , "/config"
        ]

instance ToQuery UpdateCloudFrontOriginAccessIdentity

instance ToHeaders UpdateCloudFrontOriginAccessIdentity where
    toHeaders UpdateCloudFrontOriginAccessIdentity{..} = concat
        [ "If-Match" =: _ucfoairIfMatch
        ]

instance ToXML UpdateCloudFrontOriginAccessIdentityRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateCloudFrontOriginAccessIdentityRequest"

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Sv UpdateCloudFrontOriginAccessIdentity = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UpdateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "ETag"

data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { _ucfoaisCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , _ucfoaisETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Generic)
