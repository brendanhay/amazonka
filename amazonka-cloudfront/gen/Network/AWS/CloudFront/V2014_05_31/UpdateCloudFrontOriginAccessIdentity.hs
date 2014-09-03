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
    , updateCloudFrontOriginAccessIdentity
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

-- | Minimum specification for a 'UpdateCloudFrontOriginAccessIdentity' request.
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoairCloudFrontOriginAccessIdentityConfig'
                                     -> Text -- ^ 'ucfoairId'
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
    } deriving (Show, Generic)

-- | The identity's configuration information.
ucfoairCloudFrontOriginAccessIdentityConfig
    :: Functor f
    => (CloudFrontOriginAccessIdentityConfig
    -> f (CloudFrontOriginAccessIdentityConfig))
    -> UpdateCloudFrontOriginAccessIdentity
    -> f UpdateCloudFrontOriginAccessIdentity
ucfoairCloudFrontOriginAccessIdentityConfig f x =
    (\y -> x { _ucfoairCloudFrontOriginAccessIdentityConfig = y })
       <$> f (_ucfoairCloudFrontOriginAccessIdentityConfig x)
{-# INLINE ucfoairCloudFrontOriginAccessIdentityConfig #-}

-- | The identity's id.
ucfoairId
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateCloudFrontOriginAccessIdentity
    -> f UpdateCloudFrontOriginAccessIdentity
ucfoairId f x =
    (\y -> x { _ucfoairId = y })
       <$> f (_ucfoairId x)
{-# INLINE ucfoairId #-}

-- | The value of the ETag header you received when retrieving the identity's
-- configuration. For example: E2QWRUHAPOMQZL.
ucfoairIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateCloudFrontOriginAccessIdentity
    -> f UpdateCloudFrontOriginAccessIdentity
ucfoairIfMatch f x =
    (\y -> x { _ucfoairIfMatch = y })
       <$> f (_ucfoairIfMatch x)
{-# INLINE ucfoairIfMatch #-}

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
ucfoaisCloudFrontOriginAccessIdentity
    :: Functor f
    => (Maybe CloudFrontOriginAccessIdentity
    -> f (Maybe CloudFrontOriginAccessIdentity))
    -> UpdateCloudFrontOriginAccessIdentityResponse
    -> f UpdateCloudFrontOriginAccessIdentityResponse
ucfoaisCloudFrontOriginAccessIdentity f x =
    (\y -> x { _ucfoaisCloudFrontOriginAccessIdentity = y })
       <$> f (_ucfoaisCloudFrontOriginAccessIdentity x)
{-# INLINE ucfoaisCloudFrontOriginAccessIdentity #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoaisETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateCloudFrontOriginAccessIdentityResponse
    -> f UpdateCloudFrontOriginAccessIdentityResponse
ucfoaisETag f x =
    (\y -> x { _ucfoaisETag = y })
       <$> f (_ucfoaisETag x)
{-# INLINE ucfoaisETag #-}

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Sv UpdateCloudFrontOriginAccessIdentity = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UpdateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "ETag"
