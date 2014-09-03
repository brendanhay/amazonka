{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an invalidation.
module Network.AWS.CloudFront.V2014_05_31.GetInvalidation
    (
    -- * Request
      GetInvalidation
    -- ** Request constructor
    , getInvalidation
    -- ** Request lenses
    , girDistributionId
    , girId

    -- * Response
    , GetInvalidationResponse
    -- ** Response lenses
    , gisInvalidation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetInvalidation' request.
getInvalidation :: Text -- ^ 'girDistributionId'
                -> Text -- ^ 'girId'
                -> GetInvalidation
getInvalidation p1 p2 = GetInvalidation
    { _girDistributionId = p1
    , _girId = p2
    }

data GetInvalidation = GetInvalidation
    { _girDistributionId :: Text
      -- ^ The distribution's id.
    , _girId :: Text
      -- ^ The invalidation's id.
    } deriving (Show, Generic)

-- | The distribution's id.
girDistributionId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetInvalidation
    -> f GetInvalidation
girDistributionId f x =
    (\y -> x { _girDistributionId = y })
       <$> f (_girDistributionId x)
{-# INLINE girDistributionId #-}

-- | The invalidation's id.
girId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetInvalidation
    -> f GetInvalidation
girId f x =
    (\y -> x { _girId = y })
       <$> f (_girId x)
{-# INLINE girId #-}

instance ToPath GetInvalidation where
    toPath GetInvalidation{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _girDistributionId
        , "/invalidation/"
        , toBS _girId
        ]

instance ToQuery GetInvalidation

instance ToHeaders GetInvalidation

instance ToXML GetInvalidation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetInvalidationRequest"

data GetInvalidationResponse = GetInvalidationResponse
    { _gisInvalidation :: Maybe Invalidation
      -- ^ The invalidation's information.
    } deriving (Show, Generic)

-- | The invalidation's information.
gisInvalidation
    :: Functor f
    => (Maybe Invalidation
    -> f (Maybe Invalidation))
    -> GetInvalidationResponse
    -> f GetInvalidationResponse
gisInvalidation f x =
    (\y -> x { _gisInvalidation = y })
       <$> f (_gisInvalidation x)
{-# INLINE gisInvalidation #-}

instance FromXML GetInvalidationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetInvalidation where
    type Sv GetInvalidation = CloudFront
    type Rs GetInvalidation = GetInvalidationResponse

    request = get
    response _ = xmlResponse
