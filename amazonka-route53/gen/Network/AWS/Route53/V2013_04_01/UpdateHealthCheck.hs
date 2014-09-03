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
    , updateHealthCheck
    -- ** Request lenses
    , uhcrHealthCheckId
    , uhcrFailureThreshold
    , uhcrFullyQualifiedDomainName
    , uhcrHealthCheckVersion
    , uhcrIPAddress
    , uhcrPort
    , uhcrResourcePath
    , uhcrSearchString

    -- * Response
    , UpdateHealthCheckResponse
    -- ** Response lenses
    , uhcsHealthCheck
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateHealthCheck' request.
updateHealthCheck :: Text -- ^ 'uhcrHealthCheckId'
                  -> UpdateHealthCheck
updateHealthCheck p1 = UpdateHealthCheck
    { _uhcrHealthCheckId = p1
    , _uhcrFailureThreshold = Nothing
    , _uhcrFullyQualifiedDomainName = Nothing
    , _uhcrHealthCheckVersion = Nothing
    , _uhcrIPAddress = Nothing
    , _uhcrPort = Nothing
    , _uhcrResourcePath = Nothing
    , _uhcrSearchString = Nothing
    }

data UpdateHealthCheck = UpdateHealthCheck
    { _uhcrHealthCheckId :: Text
      -- ^ The ID of the health check to update.
    , _uhcrFailureThreshold :: Maybe Integer
      -- ^ The number of consecutive health checks that an endpoint must
      -- pass or fail for Route 53 to change the current status of the
      -- endpoint from unhealthy to healthy or vice versa. Valid values
      -- are integers between 1 and 10. For more information, see "How
      -- Amazon Route 53 Determines Whether an Endpoint Is Healthy" in the
      -- Amazon Route 53 Developer Guide. Specify this value only if you
      -- want to change it.
    , _uhcrFullyQualifiedDomainName :: Maybe Text
      -- ^ Fully qualified domain name of the instance to be health checked.
      -- Specify this value only if you want to change it.
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
    , _uhcrSearchString :: Maybe Text
      -- ^ If the value of Type is HTTP_STR_MATCH or HTTP_STR_MATCH, the
      -- string that you want Route 53 to search for in the response body
      -- from the specified resource. If the string appears in the
      -- response body, Route 53 considers the resource healthy. Specify
      -- this value only if you want to change it.
    } deriving (Show, Generic)

-- | The ID of the health check to update.
uhcrHealthCheckId
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrHealthCheckId f x =
    (\y -> x { _uhcrHealthCheckId = y })
       <$> f (_uhcrHealthCheckId x)
{-# INLINE uhcrHealthCheckId #-}

-- | The number of consecutive health checks that an endpoint must pass or fail
-- for Route 53 to change the current status of the endpoint from unhealthy to
-- healthy or vice versa. Valid values are integers between 1 and 10. For more
-- information, see "How Amazon Route 53 Determines Whether an Endpoint Is
-- Healthy" in the Amazon Route 53 Developer Guide. Specify this value only if
-- you want to change it.
uhcrFailureThreshold
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrFailureThreshold f x =
    (\y -> x { _uhcrFailureThreshold = y })
       <$> f (_uhcrFailureThreshold x)
{-# INLINE uhcrFailureThreshold #-}

-- | Fully qualified domain name of the instance to be health checked. Specify
-- this value only if you want to change it.
uhcrFullyQualifiedDomainName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrFullyQualifiedDomainName f x =
    (\y -> x { _uhcrFullyQualifiedDomainName = y })
       <$> f (_uhcrFullyQualifiedDomainName x)
{-# INLINE uhcrFullyQualifiedDomainName #-}

-- | Optional. When you specify a health check version, Route 53 compares this
-- value with the current value in the health check, which prevents you from
-- updating the health check when the versions don't match. Using
-- HealthCheckVersion lets you prevent overwriting another change to the
-- health check.
uhcrHealthCheckVersion
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrHealthCheckVersion f x =
    (\y -> x { _uhcrHealthCheckVersion = y })
       <$> f (_uhcrHealthCheckVersion x)
{-# INLINE uhcrHealthCheckVersion #-}

-- | The IP address of the resource that you want to check. Specify this value
-- only if you want to change it.
uhcrIPAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrIPAddress f x =
    (\y -> x { _uhcrIPAddress = y })
       <$> f (_uhcrIPAddress x)
{-# INLINE uhcrIPAddress #-}

-- | The port on which you want Route 53 to open a connection to perform health
-- checks. Specify this value only if you want to change it.
uhcrPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrPort f x =
    (\y -> x { _uhcrPort = y })
       <$> f (_uhcrPort x)
{-# INLINE uhcrPort #-}

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file /docs/route53-health-check.html. Specify this value only if you
-- want to change it.
uhcrResourcePath
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrResourcePath f x =
    (\y -> x { _uhcrResourcePath = y })
       <$> f (_uhcrResourcePath x)
{-# INLINE uhcrResourcePath #-}

-- | If the value of Type is HTTP_STR_MATCH or HTTP_STR_MATCH, the string that
-- you want Route 53 to search for in the response body from the specified
-- resource. If the string appears in the response body, Route 53 considers
-- the resource healthy. Specify this value only if you want to change it.
uhcrSearchString
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateHealthCheck
    -> f UpdateHealthCheck
uhcrSearchString f x =
    (\y -> x { _uhcrSearchString = y })
       <$> f (_uhcrSearchString x)
{-# INLINE uhcrSearchString #-}

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

data UpdateHealthCheckResponse = UpdateHealthCheckResponse
    { _uhcsHealthCheck :: HealthCheck
      -- ^ A complex type that contains identifying information about the
      -- health check.
    } deriving (Show, Generic)

-- | A complex type that contains identifying information about the health
-- check.
uhcsHealthCheck
    :: Functor f
    => (HealthCheck
    -> f (HealthCheck))
    -> UpdateHealthCheckResponse
    -> f UpdateHealthCheckResponse
uhcsHealthCheck f x =
    (\y -> x { _uhcsHealthCheck = y })
       <$> f (_uhcsHealthCheck x)
{-# INLINE uhcsHealthCheck #-}

instance FromXML UpdateHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateHealthCheck where
    type Sv UpdateHealthCheck = Route53
    type Rs UpdateHealthCheck = UpdateHealthCheckResponse

    request = post
    response _ = xmlResponse
