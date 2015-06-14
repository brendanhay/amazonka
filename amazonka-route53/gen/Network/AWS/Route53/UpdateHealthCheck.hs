{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.UpdateHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action updates an existing health check.
--
-- To update a health check, send a @POST@ request to the
-- @2013-04-01\/healthcheck\/health check ID@ resource. The request body
-- must include an XML document with an @UpdateHealthCheckRequest@ element.
-- The response returns an @UpdateHealthCheckResponse@ element, which
-- contains metadata about the health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html>
module Network.AWS.Route53.UpdateHealthCheck
    (
    -- * Request
      UpdateHealthCheck
    -- ** Request constructor
    , updateHealthCheck
    -- ** Request lenses
    , uhcIPAddress
    , uhcSearchString
    , uhcResourcePath
    , uhcFullyQualifiedDomainName
    , uhcHealthCheckId
    , uhcFailureThreshold
    , uhcHealthCheckVersion
    , uhcPort

    -- * Response
    , UpdateHealthCheckResponse
    -- ** Response constructor
    , updateHealthCheckResponse
    -- ** Response lenses
    , uhcrHealthCheck
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'updateHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uhcIPAddress'
--
-- * 'uhcSearchString'
--
-- * 'uhcResourcePath'
--
-- * 'uhcFullyQualifiedDomainName'
--
-- * 'uhcHealthCheckId'
--
-- * 'uhcFailureThreshold'
--
-- * 'uhcHealthCheckVersion'
--
-- * 'uhcPort'
data UpdateHealthCheck = UpdateHealthCheck'{_uhcIPAddress :: Maybe Text, _uhcSearchString :: Maybe Text, _uhcResourcePath :: Maybe Text, _uhcFullyQualifiedDomainName :: Maybe Text, _uhcHealthCheckId :: Text, _uhcFailureThreshold :: Nat, _uhcHealthCheckVersion :: Nat, _uhcPort :: Nat} deriving (Eq, Read, Show)

-- | 'UpdateHealthCheck' smart constructor.
updateHealthCheck :: Text -> Natural -> Natural -> Natural -> UpdateHealthCheck
updateHealthCheck pHealthCheckId pFailureThreshold pHealthCheckVersion pPort = UpdateHealthCheck'{_uhcIPAddress = Nothing, _uhcSearchString = Nothing, _uhcResourcePath = Nothing, _uhcFullyQualifiedDomainName = Nothing, _uhcHealthCheckId = pHealthCheckId, _uhcFailureThreshold = _Nat # pFailureThreshold, _uhcHealthCheckVersion = _Nat # pHealthCheckVersion, _uhcPort = _Nat # pPort};

-- | The IP address of the resource that you want to check.
--
-- Specify this value only if you want to change it.
uhcIPAddress :: Lens' UpdateHealthCheck (Maybe Text)
uhcIPAddress = lens _uhcIPAddress (\ s a -> s{_uhcIPAddress = a});

-- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTP_STR_MATCH@, the
-- string that you want Route 53 to search for in the response body from
-- the specified resource. If the string appears in the response body,
-- Route 53 considers the resource healthy.
--
-- Specify this value only if you want to change it.
uhcSearchString :: Lens' UpdateHealthCheck (Maybe Text)
uhcSearchString = lens _uhcSearchString (\ s a -> s{_uhcSearchString = a});

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file \/docs\/route53-health-check.html.
--
-- Specify this value only if you want to change it.
uhcResourcePath :: Lens' UpdateHealthCheck (Maybe Text)
uhcResourcePath = lens _uhcResourcePath (\ s a -> s{_uhcResourcePath = a});

-- | Fully qualified domain name of the instance to be health checked.
--
-- Specify this value only if you want to change it.
uhcFullyQualifiedDomainName :: Lens' UpdateHealthCheck (Maybe Text)
uhcFullyQualifiedDomainName = lens _uhcFullyQualifiedDomainName (\ s a -> s{_uhcFullyQualifiedDomainName = a});

-- | The ID of the health check to update.
uhcHealthCheckId :: Lens' UpdateHealthCheck Text
uhcHealthCheckId = lens _uhcHealthCheckId (\ s a -> s{_uhcHealthCheckId = a});

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
--
-- Specify this value only if you want to change it.
uhcFailureThreshold :: Lens' UpdateHealthCheck Natural
uhcFailureThreshold = lens _uhcFailureThreshold (\ s a -> s{_uhcFailureThreshold = a}) . _Nat;

-- | Optional. When you specify a health check version, Route 53 compares
-- this value with the current value in the health check, which prevents
-- you from updating the health check when the versions don\'t match. Using
-- @HealthCheckVersion@ lets you prevent overwriting another change to the
-- health check.
uhcHealthCheckVersion :: Lens' UpdateHealthCheck Natural
uhcHealthCheckVersion = lens _uhcHealthCheckVersion (\ s a -> s{_uhcHealthCheckVersion = a}) . _Nat;

-- | The port on which you want Route 53 to open a connection to perform
-- health checks.
--
-- Specify this value only if you want to change it.
uhcPort :: Lens' UpdateHealthCheck Natural
uhcPort = lens _uhcPort (\ s a -> s{_uhcPort = a}) . _Nat;

instance AWSRequest UpdateHealthCheck where
        type Sv UpdateHealthCheck = Route53
        type Rs UpdateHealthCheck = UpdateHealthCheckResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateHealthCheckResponse' <$> x .@ "HealthCheck")

instance ToElement UpdateHealthCheck where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHealthCheckRequest"

instance ToHeaders UpdateHealthCheck where
        toHeaders = const mempty

instance ToPath UpdateHealthCheck where
        toPath UpdateHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/",
               toText _uhcHealthCheckId]

instance ToQuery UpdateHealthCheck where
        toQuery = const mempty

instance ToXML UpdateHealthCheck where
        toXML UpdateHealthCheck'{..}
          = mconcat
              ["IPAddress" @= _uhcIPAddress,
               "SearchString" @= _uhcSearchString,
               "ResourcePath" @= _uhcResourcePath,
               "FullyQualifiedDomainName" @=
                 _uhcFullyQualifiedDomainName,
               "FailureThreshold" @= _uhcFailureThreshold,
               "HealthCheckVersion" @= _uhcHealthCheckVersion,
               "Port" @= _uhcPort]

-- | /See:/ 'updateHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uhcrHealthCheck'
newtype UpdateHealthCheckResponse = UpdateHealthCheckResponse'{_uhcrHealthCheck :: HealthCheck} deriving (Eq, Read, Show)

-- | 'UpdateHealthCheckResponse' smart constructor.
updateHealthCheckResponse :: HealthCheck -> UpdateHealthCheckResponse
updateHealthCheckResponse pHealthCheck = UpdateHealthCheckResponse'{_uhcrHealthCheck = pHealthCheck};

-- | FIXME: Undocumented member.
uhcrHealthCheck :: Lens' UpdateHealthCheckResponse HealthCheck
uhcrHealthCheck = lens _uhcrHealthCheck (\ s a -> s{_uhcrHealthCheck = a});
