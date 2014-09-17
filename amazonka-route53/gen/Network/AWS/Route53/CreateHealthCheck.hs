{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.CreateHealthCheck
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
module Network.AWS.Route53.CreateHealthCheck
    (
    -- * Request
      CreateHealthCheck
    -- ** Request constructor
    , mkCreateHealthCheck
    -- ** Request lenses
    , chcCallerReference
    , chcHealthCheckConfig

    -- * Response
    , CreateHealthCheckResponse
    -- ** Response constructor
    , mkCreateHealthCheckResponse
    -- ** Response lenses
    , chcrHealthCheck
    , chcrLocation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | &gt;A complex type that contains information about the request to create a
-- health check.
data CreateHealthCheck = CreateHealthCheck
    { _chcCallerReference :: Text
    , _chcHealthCheckConfig :: HealthCheckConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateHealthCheck' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CallerReference ::@ @Text@
--
-- * @HealthCheckConfig ::@ @HealthCheckConfig@
--
mkCreateHealthCheck :: Text -- ^ 'chcCallerReference'
                    -> HealthCheckConfig -- ^ 'chcHealthCheckConfig'
                    -> CreateHealthCheck
mkCreateHealthCheck p1 p2 = CreateHealthCheck
    { _chcCallerReference = p1
    , _chcHealthCheckConfig = p2
    }

-- | A unique string that identifies the request and that allows failed
-- CreateHealthCheck requests to be retried without the risk of executing the
-- operation twice. You must use a unique CallerReference string every time
-- you create a health check. CallerReference can be any unique string; you
-- might choose to use a string that identifies your project. Valid characters
-- are any Unicode code points that are legal in an XML 1.0 document. The
-- UTF-8 encoding of the value must be less than 128 bytes.
chcCallerReference :: Lens' CreateHealthCheck Text
chcCallerReference =
    lens _chcCallerReference (\s a -> s { _chcCallerReference = a })

-- | A complex type that contains health check configuration.
chcHealthCheckConfig :: Lens' CreateHealthCheck HealthCheckConfig
chcHealthCheckConfig =
    lens _chcHealthCheckConfig (\s a -> s { _chcHealthCheckConfig = a })

instance ToPath CreateHealthCheck

instance ToQuery CreateHealthCheck

instance ToHeaders CreateHealthCheck

instance ToXML CreateHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateHealthCheckRequest"

-- | A complex type containing the response information for the new health
-- check.
data CreateHealthCheckResponse = CreateHealthCheckResponse
    { _chcrHealthCheck :: HealthCheck
    , _chcrLocation :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateHealthCheckResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheck ::@ @HealthCheck@
--
-- * @Location ::@ @Text@
--
mkCreateHealthCheckResponse :: HealthCheck -- ^ 'chcrHealthCheck'
                            -> Text -- ^ 'chcrLocation'
                            -> CreateHealthCheckResponse
mkCreateHealthCheckResponse p1 p2 = CreateHealthCheckResponse
    { _chcrHealthCheck = p1
    , _chcrLocation = p2
    }

-- | A complex type that contains identifying information about the health
-- check.
chcrHealthCheck :: Lens' CreateHealthCheckResponse HealthCheck
chcrHealthCheck = lens _chcrHealthCheck (\s a -> s { _chcrHealthCheck = a })

-- | The unique URL representing the new health check.
chcrLocation :: Lens' CreateHealthCheckResponse Text
chcrLocation = lens _chcrLocation (\s a -> s { _chcrLocation = a })

instance AWSRequest CreateHealthCheck where
    type Sv CreateHealthCheck = Route53
    type Rs CreateHealthCheck = CreateHealthCheckResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateHealthCheckResponse
            <*> xml %| "HealthCheck"
            <*> hs ~: "Location"
