{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53
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
module Network.AWS.Route53
    (
    -- * Request
      UpdateHealthCheck
    -- ** Request constructor
    , mkUpdateHealthCheck
    -- ** Request lenses
    , uhcHealthCheckId
    , uhcHealthCheckVersion
    , uhcIPAddress
    , uhcPort
    , uhcResourcePath
    , uhcFullyQualifiedDomainName
    , uhcSearchString
    , uhcFailureThreshold

    -- * Response
    , UpdateHealthCheckResponse
    -- ** Response constructor
    , mkUpdateHealthCheckResponse
    -- ** Response lenses
    , uhcrHealthCheck
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | &gt;A complex type that contains information about the request to update a
-- health check.
data UpdateHealthCheck = UpdateHealthCheck
    { _uhcHealthCheckId :: !Text
    , _uhcHealthCheckVersion :: !(Maybe Integer)
    , _uhcIPAddress :: !(Maybe Text)
    , _uhcPort :: !(Maybe Integer)
    , _uhcResourcePath :: !(Maybe Text)
    , _uhcFullyQualifiedDomainName :: !(Maybe Text)
    , _uhcSearchString :: !(Maybe Text)
    , _uhcFailureThreshold :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateHealthCheck' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheckId ::@ @Text@
--
-- * @HealthCheckVersion ::@ @Maybe Integer@
--
-- * @IPAddress ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @ResourcePath ::@ @Maybe Text@
--
-- * @FullyQualifiedDomainName ::@ @Maybe Text@
--
-- * @SearchString ::@ @Maybe Text@
--
-- * @FailureThreshold ::@ @Maybe Integer@
--
mkUpdateHealthCheck :: Text -- ^ 'uhcHealthCheckId'
                    -> UpdateHealthCheck
mkUpdateHealthCheck p1 = UpdateHealthCheck
    { _uhcHealthCheckId = p1
    , _uhcHealthCheckVersion = Nothing
    , _uhcIPAddress = Nothing
    , _uhcPort = Nothing
    , _uhcResourcePath = Nothing
    , _uhcFullyQualifiedDomainName = Nothing
    , _uhcSearchString = Nothing
    , _uhcFailureThreshold = Nothing
    }

-- | The ID of the health check to update.
uhcHealthCheckId :: Lens' UpdateHealthCheck Text
uhcHealthCheckId =
    lens _uhcHealthCheckId (\s a -> s { _uhcHealthCheckId = a })

-- | Optional. When you specify a health check version, Route 53 compares this
-- value with the current value in the health check, which prevents you from
-- updating the health check when the versions don't match. Using
-- HealthCheckVersion lets you prevent overwriting another change to the
-- health check.
uhcHealthCheckVersion :: Lens' UpdateHealthCheck (Maybe Integer)
uhcHealthCheckVersion =
    lens _uhcHealthCheckVersion (\s a -> s { _uhcHealthCheckVersion = a })

-- | The IP address of the resource that you want to check. Specify this value
-- only if you want to change it.
uhcIPAddress :: Lens' UpdateHealthCheck (Maybe Text)
uhcIPAddress = lens _uhcIPAddress (\s a -> s { _uhcIPAddress = a })

-- | The port on which you want Route 53 to open a connection to perform health
-- checks. Specify this value only if you want to change it.
uhcPort :: Lens' UpdateHealthCheck (Maybe Integer)
uhcPort = lens _uhcPort (\s a -> s { _uhcPort = a })

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file /docs/route53-health-check.html. Specify this value only if you
-- want to change it.
uhcResourcePath :: Lens' UpdateHealthCheck (Maybe Text)
uhcResourcePath = lens _uhcResourcePath (\s a -> s { _uhcResourcePath = a })

-- | Fully qualified domain name of the instance to be health checked. Specify
-- this value only if you want to change it.
uhcFullyQualifiedDomainName :: Lens' UpdateHealthCheck (Maybe Text)
uhcFullyQualifiedDomainName =
    lens _uhcFullyQualifiedDomainName
         (\s a -> s { _uhcFullyQualifiedDomainName = a })

-- | If the value of Type is HTTP_STR_MATCH or HTTP_STR_MATCH, the string that
-- you want Route 53 to search for in the response body from the specified
-- resource. If the string appears in the response body, Route 53 considers
-- the resource healthy. Specify this value only if you want to change it.
uhcSearchString :: Lens' UpdateHealthCheck (Maybe Text)
uhcSearchString = lens _uhcSearchString (\s a -> s { _uhcSearchString = a })

-- | The number of consecutive health checks that an endpoint must pass or fail
-- for Route 53 to change the current status of the endpoint from unhealthy to
-- healthy or vice versa. Valid values are integers between 1 and 10. For more
-- information, see "How Amazon Route 53 Determines Whether an Endpoint Is
-- Healthy" in the Amazon Route 53 Developer Guide. Specify this value only if
-- you want to change it.
uhcFailureThreshold :: Lens' UpdateHealthCheck (Maybe Integer)
uhcFailureThreshold =
    lens _uhcFailureThreshold (\s a -> s { _uhcFailureThreshold = a })

instance ToPath UpdateHealthCheck

instance ToQuery UpdateHealthCheck

instance ToHeaders UpdateHealthCheck

instance ToXML UpdateHealthCheck where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateHealthCheckRequest"

newtype UpdateHealthCheckResponse = UpdateHealthCheckResponse
    { _uhcrHealthCheck :: HealthCheck
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateHealthCheckResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheck ::@ @HealthCheck@
--
mkUpdateHealthCheckResponse :: HealthCheck -- ^ 'uhcrHealthCheck'
                            -> UpdateHealthCheckResponse
mkUpdateHealthCheckResponse p1 = UpdateHealthCheckResponse
    { _uhcrHealthCheck = p1
    }

-- | A complex type that contains identifying information about the health
-- check.
uhcrHealthCheck :: Lens' UpdateHealthCheckResponse HealthCheck
uhcrHealthCheck = lens _uhcrHealthCheck (\s a -> s { _uhcrHealthCheck = a })

instance FromXML UpdateHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateHealthCheck where
    type Sv UpdateHealthCheck = Route53
    type Rs UpdateHealthCheck = UpdateHealthCheckResponse

    request = get
    response _ = xmlResponse
