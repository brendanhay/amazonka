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
    , mkCreateInvalidationRequest
    -- ** Request lenses
    , cirDistributionId
    , cirInvalidationBatch

    -- * Response
    , CreateInvalidationResponse
    -- ** Response lenses
    , cisLocation
    , cisInvalidation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInvalidation' request.
mkCreateInvalidationRequest :: Text -- ^ 'cirDistributionId'
                            -> InvalidationBatch -- ^ 'cirInvalidationBatch'
                            -> CreateInvalidation
mkCreateInvalidationRequest p1 p2 = CreateInvalidation
    { _cirDistributionId = p1
    , _cirInvalidationBatch = p2
    }
{-# INLINE mkCreateInvalidationRequest #-}

data CreateInvalidation = CreateInvalidation
    { _cirDistributionId :: Text
      -- ^ The distribution's id.
    , _cirInvalidationBatch :: InvalidationBatch
      -- ^ The batch information for the invalidation.
    } deriving (Show, Generic)

-- | The distribution's id.
cirDistributionId :: Lens' CreateInvalidation (Text)
cirDistributionId = lens _cirDistributionId (\s a -> s { _cirDistributionId = a })
{-# INLINE cirDistributionId #-}

-- | The batch information for the invalidation.
cirInvalidationBatch :: Lens' CreateInvalidation (InvalidationBatch)
cirInvalidationBatch = lens _cirInvalidationBatch (\s a -> s { _cirInvalidationBatch = a })
{-# INLINE cirInvalidationBatch #-}

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
    { _cisLocation :: Maybe Text
      -- ^ The fully qualified URI of the distribution and invalidation
      -- batch request, including the Invalidation ID.
    , _cisInvalidation :: Maybe Invalidation
      -- ^ The invalidation's information.
    } deriving (Show, Generic)

-- | The fully qualified URI of the distribution and invalidation batch request,
-- including the Invalidation ID.
cisLocation :: Lens' CreateInvalidationResponse (Maybe Text)
cisLocation = lens _cisLocation (\s a -> s { _cisLocation = a })
{-# INLINE cisLocation #-}

-- | The invalidation's information.
cisInvalidation :: Lens' CreateInvalidationResponse (Maybe Invalidation)
cisInvalidation = lens _cisInvalidation (\s a -> s { _cisInvalidation = a })
{-# INLINE cisInvalidation #-}

instance AWSRequest CreateInvalidation where
    type Sv CreateInvalidation = CloudFront
    type Rs CreateInvalidation = CreateInvalidationResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateInvalidationResponse
            <*> hs ~:? "Location"
            <*> xml %|? "Invalidation"
