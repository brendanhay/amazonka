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
{-# INLINE getInvalidation #-}

data GetInvalidation = GetInvalidation
    { _girDistributionId :: Text
      -- ^ The distribution's id.
    , _girId :: Text
      -- ^ The invalidation's id.
    } deriving (Show, Generic)

-- | The distribution's id.
girDistributionId :: Lens' GetInvalidation (Text)
girDistributionId f x =
    f (_girDistributionId x)
        <&> \y -> x { _girDistributionId = y }
{-# INLINE girDistributionId #-}

-- | The invalidation's id.
girId :: Lens' GetInvalidation (Text)
girId f x =
    f (_girId x)
        <&> \y -> x { _girId = y }
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
gisInvalidation :: Lens' GetInvalidationResponse (Maybe Invalidation)
gisInvalidation f x =
    f (_gisInvalidation x)
        <&> \y -> x { _gisInvalidation = y }
{-# INLINE gisInvalidation #-}

instance FromXML GetInvalidationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetInvalidation where
    type Sv GetInvalidation = CloudFront
    type Rs GetInvalidation = GetInvalidationResponse

    request = get
    response _ = xmlResponse
