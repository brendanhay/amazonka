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
    , mkCreateInvalidation
    -- ** Request lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Response
    , CreateInvalidationResponse
    -- ** Response lenses
    , cirsLocation
    , cirsInvalidation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create an invalidation.
data CreateInvalidation = CreateInvalidation
    { _ciDistributionId :: Text
    , _ciInvalidationBatch :: InvalidationBatch
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInvalidation' request.
mkCreateInvalidation :: Text -- ^ 'ciDistributionId'
                     -> InvalidationBatch -- ^ 'ciInvalidationBatch'
                     -> CreateInvalidation
mkCreateInvalidation p1 p2 = CreateInvalidation
    { _ciDistributionId = p1
    , _ciInvalidationBatch = p2
    }

-- | The distribution's id.
ciDistributionId :: Lens' CreateInvalidation Text
ciDistributionId =
    lens _ciDistributionId (\s a -> s { _ciDistributionId = a })

-- | The batch information for the invalidation.
ciInvalidationBatch :: Lens' CreateInvalidation InvalidationBatch
ciInvalidationBatch =
    lens _ciInvalidationBatch (\s a -> s { _ciInvalidationBatch = a })

instance ToPath CreateInvalidation where
    toPath CreateInvalidation{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _ciDistributionId
        , "/invalidation"
        ]

instance ToQuery CreateInvalidation

instance ToHeaders CreateInvalidation

instance ToXML CreateInvalidation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateInvalidationRequest"

-- | The returned result of the corresponding request.
data CreateInvalidationResponse = CreateInvalidationResponse
    { _cirsLocation :: Maybe Text
    , _cirsInvalidation :: Maybe Invalidation
    } deriving (Show, Generic)

-- | The fully qualified URI of the distribution and invalidation batch request,
-- including the Invalidation ID.
cirsLocation :: Lens' CreateInvalidationResponse (Maybe Text)
cirsLocation = lens _cirsLocation (\s a -> s { _cirsLocation = a })

-- | The invalidation's information.
cirsInvalidation :: Lens' CreateInvalidationResponse (Maybe Invalidation)
cirsInvalidation =
    lens _cirsInvalidation (\s a -> s { _cirsInvalidation = a })

instance AWSRequest CreateInvalidation where
    type Sv CreateInvalidation = CloudFront
    type Rs CreateInvalidation = CreateInvalidationResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateInvalidationResponse
            <*> hs ~:? "Location"
            <*> xml %|? "Invalidation"
