{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.UpdateHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action updates an existing health check. To update a health check,
-- send a POST request to the 2013-05-27/healthcheck/health check ID resource.
-- The request body must include an XML document with an
-- UpdateHealthCheckRequest element. The response returns an
-- UpdateHealthCheckResponse element, which contains metadata about the health
-- check.
module Network.AWS.Route53.V2013_04_01.UpdateHealthCheck
    (
    -- * Request
      UpdateHealthCheck
    -- ** Request constructor
    , mkUpdateHealthCheckRequest
    -- ** Request lenses
    , uhcrHealthCheckId
    , uhcrHealthCheckVersion
    , uhcrIPAddress
    , uhcrPort
    , uhcrResourcePath
    , uhcrFullyQualifiedDomainName
    , uhcrSearchString
    , uhcrFailureThreshold

    -- * Response
    , UpdateHealthCheckResponse
    -- ** Response lenses
    , uhcsHealthCheck
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateHealthCheck' request.
mkUpdateHealthCheckRequest :: Text -- ^ 'uhcrHealthCheckId'
                           -> UpdateHealthCheck
mkUpdateHealthCheckRequest p1 = UpdateHealthCheck
    { _uhcrHealthCheckId = p1
    , _uhcrHealthCheckVersion = Nothing
    , _uhcrIPAddress = Nothing
    , _uhcrPort = Nothing
    , _uhcrResourcePath = Nothing
    , _uhcrFullyQualifiedDomainName = Nothing
    , _uhcrSearchString = Nothing
    , _uhcrFailureThreshold = Nothing
    }
{-# INLINE mkUpdateHealthCheckRequest #-}

data UpdateHealthCheck = UpdateHealthCheck
    { _uhcrHealthCheckId :: Text
      -- ^ The ID of the health check to update.
    , _uhcrHealthCheckVersion :: Maybe Integer
      -- ^ Optional. When you specify a health check version, Route 53
      -- compares this value with the current value in the health check,
      -- which prevents you from updating the health check when the
      -- versions don't match. Using HealthCheckVersion lets you prevent
      -- overwriting another change to the health check.
    , _uhcrIPAddress :: Maybe Text
      -- ^ The IP address of the resource that you want to check. Specify
      -- this value only if you want to change it.
    , _uhcrPort :: Maybe Integer
      -- ^ The port on which you want Route 53 to open a connection to
      -- perform health checks. Specify this value only if you want to
      -- change it.
    , _uhcrResourcePath :: Maybe Text
      -- ^ The path that you want Amazon Route 53 to request when performing
      -- health checks. The path can be any value for which your endpoint
      -- will return an HTTP status code of 2xx or 3xx when the endpoint
      -- is healthy, for example the file /docs/route53-health-check.html.
      -- Specify this value only if you want to change it.
    , _uhcrFullyQualifiedDomainName :: Maybe Text
      -- ^ Fully qualified domain name of the instance to be health checked.
      -- Specify this value only if you want to change it.
    , _uhcrSearchString :: Maybe Text
      -- ^ If the value of Type is HTTP_STR_MATCH or HTTP_STR_MATCH, the
      -- string that you want Route 53 to search for in the response body
      -- from the specified resource. If the string appears in the
      -- response body, Route 53 considers the resource healthy. Specify
      -- this value only if you want to change it.
    , _uhcrFailureThreshold :: Maybe Integer
      -- ^ The number of consecutive health checks that an endpoint must
      -- pass or fail for Route 53 to change the current status of the
      -- endpoint from unhealthy to healthy or vice versa. Valid values
      -- are integers between 1 and 10. For more information, see "How
      -- Amazon Route 53 Determines Whether an Endpoint Is Healthy" in the
      -- Amazon Route 53 Developer Guide. Specify this value only if you
      -- want to change it.
    } deriving (Show, Generic)

-- | The ID of the health check to update.
uhcrHealthCheckId :: Lens' UpdateHealthCheck (Text)
uhcrHealthCheckId = lens _uhcrHealthCheckId (\s a -> s { _uhcrHealthCheckId = a })
{-# INLINE uhcrHealthCheckId #-}

-- | Optional. When you specify a health check version, Route 53 compares this
-- value with the current value in the health check, which prevents you from
-- updating the health check when the versions don't match. Using
-- HealthCheckVersion lets you prevent overwriting another change to the
-- health check.
uhcrHealthCheckVersion :: Lens' UpdateHealthCheck (Maybe Integer)
uhcrHealthCheckVersion = lens _uhcrHealthCheckVersion (\s a -> s { _uhcrHealthCheckVersion = a })
{-# INLINE uhcrHealthCheckVersion #-}

-- | The IP address of the resource that you want to check. Specify this value
-- only if you want to change it.
uhcrIPAddress :: Lens' UpdateHealthCheck (Maybe Text)
uhcrIPAddress = lens _uhcrIPAddress (\s a -> s { _uhcrIPAddress = a })
{-# INLINE uhcrIPAddress #-}

-- | The port on which you want Route 53 to open a connection to perform health
-- checks. Specify this value only if you want to change it.
uhcrPort :: Lens' UpdateHealthCheck (Maybe Integer)
uhcrPort = lens _uhcrPort (\s a -> s { _uhcrPort = a })
{-# INLINE uhcrPort #-}

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file /docs/route53-health-check.html. Specify this value only if you
-- want to change it.
uhcrResourcePath :: Lens' UpdateHealthCheck (Maybe Text)
uhcrResourcePath = lens _uhcrResourcePath (\s a -> s { _uhcrResourcePath = a })
{-# INLINE uhcrResourcePath #-}

-- | Fully qualified domain name of the instance to be health checked. Specify
-- this value only if you want to change it.
uhcrFullyQualifiedDomainName :: Lens' UpdateHealthCheck (Maybe Text)
uhcrFullyQualifiedDomainName = lens _uhcrFullyQualifiedDomainName (\s a -> s { _uhcrFullyQualifiedDomainName = a })
{-# INLINE uhcrFullyQualifiedDomainName #-}

-- | If the value of Type is HTTP_STR_MATCH or HTTP_STR_MATCH, the string that
-- you want Route 53 to search for in the response body from the specified
-- resource. If the string appears in the response body, Route 53 considers
-- the resource healthy. Specify this value only if you want to change it.
uhcrSearchString :: Lens' UpdateHealthCheck (Maybe Text)
uhcrSearchString = lens _uhcrSearchString (\s a -> s { _uhcrSearchString = a })
{-# INLINE uhcrSearchString #-}

-- | The number of consecutive health checks that an endpoint must pass or fail
-- for Route 53 to change the current status of the endpoint from unhealthy to
-- healthy or vice versa. Valid values are integers between 1 and 10. For more
-- information, see "How Amazon Route 53 Determines Whether an Endpoint Is
-- Healthy" in the Amazon Route 53 Developer Guide. Specify this value only if
-- you want to change it.
uhcrFailureThreshold :: Lens' UpdateHealthCheck (Maybe Integer)
uhcrFailureThreshold = lens _uhcrFailureThreshold (\s a -> s { _uhcrFailureThreshold = a })
{-# INLINE uhcrFailureThreshold #-}

instance ToPath UpdateHealthCheck where
    toPath UpdateHealthCheck{..} = mconcat
        [ "/2013-04-01/healthcheck/"
        , toBS _uhcrHealthCheckId
        ]

instance ToQuery UpdateHealthCheck

instance ToHeaders UpdateHealthCheck

instance ToXML UpdateHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateHealthCheckRequest"

newtype UpdateHealthCheckResponse = UpdateHealthCheckResponse
    { _uhcsHealthCheck :: HealthCheck
      -- ^ A complex type that contains identifying information about the
      -- health check.
    } deriving (Show, Generic)

-- | A complex type that contains identifying information about the health
-- check.
uhcsHealthCheck :: Lens' UpdateHealthCheckResponse (HealthCheck)
uhcsHealthCheck = lens _uhcsHealthCheck (\s a -> s { _uhcsHealthCheck = a })
{-# INLINE uhcsHealthCheck #-}

instance FromXML UpdateHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateHealthCheck where
    type Sv UpdateHealthCheck = Route53
    type Rs UpdateHealthCheck = UpdateHealthCheckResponse

    request = post
    response _ = xmlResponse
