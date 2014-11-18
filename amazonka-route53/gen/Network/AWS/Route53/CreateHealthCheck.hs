{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHealthCheck.html>
module Network.AWS.Route53.CreateHealthCheck
    (
    -- * Request
      CreateHealthCheck
    -- ** Request constructor
    , createHealthCheck
    -- ** Request lenses
    , chcCallerReference
    , chcHealthCheckConfig

    -- * Response
    , CreateHealthCheckResponse
    -- ** Response constructor
    , createHealthCheckResponse
    -- ** Response lenses
    , chcrHealthCheck
    , chcrLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data CreateHealthCheck = CreateHealthCheck
    { _chcCallerReference   :: Text
    , _chcHealthCheckConfig :: HealthCheckConfig
    } deriving (Eq, Show, Generic)

-- | 'CreateHealthCheck' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcCallerReference' @::@ 'Text'
--
-- * 'chcHealthCheckConfig' @::@ 'HealthCheckConfig'
--
createHealthCheck :: Text -- ^ 'chcCallerReference'
                  -> HealthCheckConfig -- ^ 'chcHealthCheckConfig'
                  -> CreateHealthCheck
createHealthCheck p1 p2 = CreateHealthCheck
    { _chcCallerReference   = p1
    , _chcHealthCheckConfig = p2
    }

-- | A unique string that identifies the request and that allows failed
-- CreateHealthCheck requests to be retried without the risk of executing
-- the operation twice. You must use a unique CallerReference string every
-- time you create a health check. CallerReference can be any unique string;
-- you might choose to use a string that identifies your project. Valid
-- characters are any Unicode code points that are legal in an XML 1.0
-- document. The UTF-8 encoding of the value must be less than 128 bytes.
chcCallerReference :: Lens' CreateHealthCheck Text
chcCallerReference =
    lens _chcCallerReference (\s a -> s { _chcCallerReference = a })

-- | A complex type that contains health check configuration.
chcHealthCheckConfig :: Lens' CreateHealthCheck HealthCheckConfig
chcHealthCheckConfig =
    lens _chcHealthCheckConfig (\s a -> s { _chcHealthCheckConfig = a })

data CreateHealthCheckResponse = CreateHealthCheckResponse
    { _chcrHealthCheck :: HealthCheck
    , _chcrLocation    :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateHealthCheckResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHealthCheck' @::@ 'HealthCheck'
--
-- * 'chcrLocation' @::@ 'Text'
--
createHealthCheckResponse :: HealthCheck -- ^ 'chcrHealthCheck'
                          -> Text -- ^ 'chcrLocation'
                          -> CreateHealthCheckResponse
createHealthCheckResponse p1 p2 = CreateHealthCheckResponse
    { _chcrHealthCheck = p1
    , _chcrLocation    = p2
    }

-- | A complex type that contains identifying information about the health
-- check.
chcrHealthCheck :: Lens' CreateHealthCheckResponse HealthCheck
chcrHealthCheck = lens _chcrHealthCheck (\s a -> s { _chcrHealthCheck = a })

-- | The unique URL representing the new health check.
chcrLocation :: Lens' CreateHealthCheckResponse Text
chcrLocation = lens _chcrLocation (\s a -> s { _chcrLocation = a })

instance ToPath CreateHealthCheck where
    toPath = const "/2013-04-01/healthcheck"

instance ToQuery CreateHealthCheck where
    toQuery = const mempty

instance ToHeaders CreateHealthCheck

instance ToXML CreateHealthCheck where
    toXML CreateHealthCheck{..} = node "CreateHealthCheck"
        [ "CallerReference"   .= _chcCallerReference
        , "HealthCheckConfig" .= _chcHealthCheckConfig
        ]

instance AWSRequest CreateHealthCheck where
    type Sv CreateHealthCheck = Route53
    type Rs CreateHealthCheck = CreateHealthCheckResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateHealthCheckResponse
        <$> x %| "HealthCheck"
        <*> h ~: "Location"
