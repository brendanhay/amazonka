{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new invalidation.
module Network.AWS.CloudFront.CreateInvalidation
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
    -- ** Response constructor
    , mkCreateInvalidationResponse
    -- ** Response lenses
    , cirLocation
    , cirInvalidation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create an invalidation.
data CreateInvalidation = CreateInvalidation
    { _ciDistributionId :: !Text
    , _ciInvalidationBatch :: InvalidationBatch
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInvalidation' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DistributionId ::@ @Text@
--
-- * @InvalidationBatch ::@ @InvalidationBatch@
--
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

instance ToPath CreateInvalidation

instance ToQuery CreateInvalidation

instance ToHeaders CreateInvalidation

instance ToXML CreateInvalidation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateInvalidationRequest"

-- | The returned result of the corresponding request.
data CreateInvalidationResponse = CreateInvalidationResponse
    { _cirLocation :: !(Maybe Text)
    , _cirInvalidation :: Maybe Invalidation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInvalidationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Location ::@ @Maybe Text@
--
-- * @Invalidation ::@ @Maybe Invalidation@
--
mkCreateInvalidationResponse :: CreateInvalidationResponse
mkCreateInvalidationResponse = CreateInvalidationResponse
    { _cirLocation = Nothing
    , _cirInvalidation = Nothing
    }

-- | The fully qualified URI of the distribution and invalidation batch request,
-- including the Invalidation ID.
cirLocation :: Lens' CreateInvalidationResponse (Maybe Text)
cirLocation = lens _cirLocation (\s a -> s { _cirLocation = a })

-- | The invalidation's information.
cirInvalidation :: Lens' CreateInvalidationResponse (Maybe Invalidation)
cirInvalidation = lens _cirInvalidation (\s a -> s { _cirInvalidation = a })

instance AWSRequest CreateInvalidation where
    type Sv CreateInvalidation = CloudFront
    type Rs CreateInvalidation = CreateInvalidationResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateInvalidationResponse
            <*> hs ~:? "Location"
            <*> xml %|? "Invalidation"
