{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.Route53.V2013_04_01.CreateHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action creates a new health check. To create a new health check, send
-- a POST request to the 2013-04-01/healthcheck resource. The request body
-- must include an XML document with a CreateHealthCheckRequest element. The
-- response returns the CreateHealthCheckResponse element that contains
-- metadata about the health check.
module Network.AWS.Route53.V2013_04_01.CreateHealthCheck where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

data CreateHealthCheck = CreateHealthCheck
    { _chcrHealthCheckConfig :: HealthCheckConfig
      -- ^ A complex type that contains health check configuration.
    , _chcrCallerReference :: Text
      -- ^ A unique string that identifies the request and that allows
      -- failed CreateHealthCheck requests to be retried without the risk
      -- of executing the operation twice. You must use a unique
      -- CallerReference string every time you create a health check.
      -- CallerReference can be any unique string; you might choose to use
      -- a string that identifies your project. Valid characters are any
      -- Unicode code points that are legal in an XML 1.0 document. The
      -- UTF-8 encoding of the value must be less than 128 bytes.
    } deriving (Show, Generic)

makeLenses ''CreateHealthCheck

instance ToPath CreateHealthCheck where
    toPath = const "/2013-04-01/healthcheck"

instance ToQuery CreateHealthCheck

instance ToHeaders CreateHealthCheck

instance ToXML CreateHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateHealthCheckRequest"

data CreateHealthCheckResponse = CreateHealthCheckResponse
    { _chcsHealthCheck :: HealthCheck
      -- ^ A complex type that contains identifying information about the
      -- health check.
    , _chcsLocation :: Text
      -- ^ The unique URL representing the new health check.
    } deriving (Show, Generic)

makeLenses ''CreateHealthCheckResponse

instance AWSRequest CreateHealthCheck where
    type Sv CreateHealthCheck = Route53
    type Rs CreateHealthCheck = CreateHealthCheckResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateHealthCheckResponse
            <*> xml %| "HealthCheck"
            <*> hs ~: "Location"
