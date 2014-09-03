{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.CreateInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new invalidation.
module Network.AWS.CloudFront.V2014_05_31.CreateInvalidation
    (
    -- * Request
      CreateInvalidation
    -- ** Request constructor
    , createInvalidation
    -- ** Request lenses
    , cirInvalidationBatch
    , cirDistributionId

    -- * Response
    , CreateInvalidationResponse
    -- ** Response lenses
    , cisInvalidation
    , cisLocation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateInvalidation' request.
createInvalidation :: InvalidationBatch -- ^ 'cirInvalidationBatch'
                   -> Text -- ^ 'cirDistributionId'
                   -> CreateInvalidation
createInvalidation p1 p2 = CreateInvalidation
    { _cirInvalidationBatch = p1
    , _cirDistributionId = p2
    }

data CreateInvalidation = CreateInvalidation
    { _cirInvalidationBatch :: InvalidationBatch
      -- ^ The batch information for the invalidation.
    , _cirDistributionId :: Text
      -- ^ The distribution's id.
    } deriving (Show, Generic)

-- | The batch information for the invalidation.
cirInvalidationBatch
    :: Functor f
    => (InvalidationBatch
    -> f (InvalidationBatch))
    -> CreateInvalidation
    -> f CreateInvalidation
cirInvalidationBatch f x =
    (\y -> x { _cirInvalidationBatch = y })
       <$> f (_cirInvalidationBatch x)
{-# INLINE cirInvalidationBatch #-}

-- | The distribution's id.
cirDistributionId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateInvalidation
    -> f CreateInvalidation
cirDistributionId f x =
    (\y -> x { _cirDistributionId = y })
       <$> f (_cirDistributionId x)
{-# INLINE cirDistributionId #-}

instance ToPath CreateInvalidation where
    toPath CreateInvalidation{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _cirDistributionId
        , "/invalidation"
        ]

instance ToQuery CreateInvalidation

instance ToHeaders CreateInvalidation

instance ToXML CreateInvalidation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateInvalidationRequest"

data CreateInvalidationResponse = CreateInvalidationResponse
    { _cisInvalidation :: Maybe Invalidation
      -- ^ The invalidation's information.
    , _cisLocation :: Maybe Text
      -- ^ The fully qualified URI of the distribution and invalidation
      -- batch request, including the Invalidation ID.
    } deriving (Show, Generic)

-- | The invalidation's information.
cisInvalidation
    :: Functor f
    => (Maybe Invalidation
    -> f (Maybe Invalidation))
    -> CreateInvalidationResponse
    -> f CreateInvalidationResponse
cisInvalidation f x =
    (\y -> x { _cisInvalidation = y })
       <$> f (_cisInvalidation x)
{-# INLINE cisInvalidation #-}

-- | The fully qualified URI of the distribution and invalidation batch request,
-- including the Invalidation ID.
cisLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInvalidationResponse
    -> f CreateInvalidationResponse
cisLocation f x =
    (\y -> x { _cisLocation = y })
       <$> f (_cisLocation x)
{-# INLINE cisLocation #-}

instance AWSRequest CreateInvalidation where
    type Sv CreateInvalidation = CloudFront
    type Rs CreateInvalidation = CreateInvalidationResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateInvalidationResponse
            <*> xml %|? "Invalidation"
            <*> hs ~:? "Location"
