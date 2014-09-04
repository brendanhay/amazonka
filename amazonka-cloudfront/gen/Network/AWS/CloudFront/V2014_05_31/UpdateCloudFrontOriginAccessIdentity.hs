{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity
    (
    -- * Request
      UpdateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , mkUpdateCloudFrontOriginAccessIdentityRequest
    -- ** Request lenses
    , ucfoairCloudFrontOriginAccessIdentityConfig
    , ucfoairId
    , ucfoairIfMatch

    -- * Response
    , UpdateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ucfoaisCloudFrontOriginAccessIdentity
    , ucfoaisETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateCloudFrontOriginAccessIdentity' request.
mkUpdateCloudFrontOriginAccessIdentityRequest :: CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoairCloudFrontOriginAccessIdentityConfig'
                                              -> Text -- ^ 'ucfoairId'
                                              -> UpdateCloudFrontOriginAccessIdentity
mkUpdateCloudFrontOriginAccessIdentityRequest p1 p2 = UpdateCloudFrontOriginAccessIdentity
    { _ucfoairCloudFrontOriginAccessIdentityConfig = p1
    , _ucfoairId = p2
    , _ucfoairIfMatch = Nothing
    }
{-# INLINE mkUpdateCloudFrontOriginAccessIdentityRequest #-}

data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity
    { _ucfoairCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
      -- ^ The identity's configuration information.
    , _ucfoairId :: Text
      -- ^ The identity's id.
    , _ucfoairIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when retrieving the
      -- identity's configuration. For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The identity's configuration information.
ucfoairCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity (CloudFrontOriginAccessIdentityConfig)
ucfoairCloudFrontOriginAccessIdentityConfig = lens _ucfoairCloudFrontOriginAccessIdentityConfig (\s a -> s { _ucfoairCloudFrontOriginAccessIdentityConfig = a })
{-# INLINE ucfoairCloudFrontOriginAccessIdentityConfig #-}

-- | The identity's id.
ucfoairId :: Lens' UpdateCloudFrontOriginAccessIdentity (Text)
ucfoairId = lens _ucfoairId (\s a -> s { _ucfoairId = a })
{-# INLINE ucfoairId #-}

-- | The value of the ETag header you received when retrieving the identity's
-- configuration. For example: E2QWRUHAPOMQZL.
ucfoairIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity (Maybe Text)
ucfoairIfMatch = lens _ucfoairIfMatch (\s a -> s { _ucfoairIfMatch = a })
{-# INLINE ucfoairIfMatch #-}

instance ToPath UpdateCloudFrontOriginAccessIdentity where
    toPath UpdateCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toBS _ucfoairId
        , "/config"
        ]

instance ToQuery UpdateCloudFrontOriginAccessIdentity

instance ToHeaders UpdateCloudFrontOriginAccessIdentity

instance ToXML UpdateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateCloudFrontOriginAccessIdentityRequest"

data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { _ucfoaisCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
      -- ^ The origin access identity's information.
    , _ucfoaisETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The origin access identity's information.
ucfoaisCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ucfoaisCloudFrontOriginAccessIdentity = lens _ucfoaisCloudFrontOriginAccessIdentity (\s a -> s { _ucfoaisCloudFrontOriginAccessIdentity = a })
{-# INLINE ucfoaisCloudFrontOriginAccessIdentity #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoaisETag :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ucfoaisETag = lens _ucfoaisETag (\s a -> s { _ucfoaisETag = a })
{-# INLINE ucfoaisETag #-}

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Sv UpdateCloudFrontOriginAccessIdentity = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UpdateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "ETag"
