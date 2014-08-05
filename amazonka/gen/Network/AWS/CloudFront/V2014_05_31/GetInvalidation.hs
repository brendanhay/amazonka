{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudFront.V2014_05_31.GetInvalidation where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

data GetInvalidation = GetInvalidation
    { _girDistributionId :: Text
      -- ^ The distribution's id.
    , _girId :: Text
      -- ^ The invalidation's id.
    } deriving (Show, Generic)

makeLenses ''GetInvalidation

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

makeLenses ''GetInvalidationResponse

instance AWSRequest GetInvalidation where
    type Sv GetInvalidation = CloudFront
    type Rs GetInvalidation = GetInvalidationResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetInvalidationResponse
            <*> xml %|? "Invalidation"
