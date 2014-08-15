{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CloudFront.V2014_05_31.CreateInvalidation where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

data CreateInvalidation = CreateInvalidation
    { _cirInvalidationBatch :: InvalidationBatch
      -- ^ The batch information for the invalidation.
    , _cirDistributionId :: Text
      -- ^ The distribution's id.
    } deriving (Show, Generic)

makeLenses ''CreateInvalidation

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

makeLenses ''CreateInvalidationResponse

instance AWSRequest CreateInvalidation where
    type Sv CreateInvalidation = CloudFront
    type Rs CreateInvalidation = CreateInvalidationResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateInvalidationResponse
            <*> xml %|? "Invalidation"
            <*> hs ~:? "Location"
