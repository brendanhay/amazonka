{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action updates an existing health check.
--
-- To update a health check, send a 'POST' request to the
-- '2013-04-01\/healthcheck\/health check ID' resource. The request body
-- must include an XML document with an 'UpdateHealthCheckRequest' element.
-- The response returns an 'UpdateHealthCheckResponse' element, which
-- contains metadata about the health check.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHealthCheck.html AWS API Reference> for UpdateHealthCheck.
module Network.AWS.Route53.UpdateHealthCheck
    (
    -- * Creating a Request
      updateHealthCheck
    , UpdateHealthCheck
    -- * Request Lenses
    , uhcFailureThreshold
    , uhcIPAddress
    , uhcSearchString
    , uhcResourcePath
    , uhcHealthCheckVersion
    , uhcFullyQualifiedDomainName
    , uhcPort
    , uhcHealthCheckId

    -- * Destructuring the Response
    , updateHealthCheckResponse
    , UpdateHealthCheckResponse
    -- * Response Lenses
    , uhcrsStatus
    , uhcrsHealthCheck
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | >A complex type that contains information about the request to update a
-- health check.
--
-- /See:/ 'updateHealthCheck' smart constructor.
data UpdateHealthCheck = UpdateHealthCheck'
    { _uhcFailureThreshold         :: !(Maybe Nat)
    , _uhcIPAddress                :: !(Maybe Text)
    , _uhcSearchString             :: !(Maybe Text)
    , _uhcResourcePath             :: !(Maybe Text)
    , _uhcHealthCheckVersion       :: !(Maybe Nat)
    , _uhcFullyQualifiedDomainName :: !(Maybe Text)
    , _uhcPort                     :: !(Maybe Nat)
    , _uhcHealthCheckId            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhcFailureThreshold'
--
-- * 'uhcIPAddress'
--
-- * 'uhcSearchString'
--
-- * 'uhcResourcePath'
--
-- * 'uhcHealthCheckVersion'
--
-- * 'uhcFullyQualifiedDomainName'
--
-- * 'uhcPort'
--
-- * 'uhcHealthCheckId'
updateHealthCheck
    :: Text -- ^ 'uhcHealthCheckId'
    -> UpdateHealthCheck
updateHealthCheck pHealthCheckId_ =
    UpdateHealthCheck'
    { _uhcFailureThreshold = Nothing
    , _uhcIPAddress = Nothing
    , _uhcSearchString = Nothing
    , _uhcResourcePath = Nothing
    , _uhcHealthCheckVersion = Nothing
    , _uhcFullyQualifiedDomainName = Nothing
    , _uhcPort = Nothing
    , _uhcHealthCheckId = pHealthCheckId_
    }

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
--
-- Specify this value only if you want to change it.
uhcFailureThreshold :: Lens' UpdateHealthCheck (Maybe Natural)
uhcFailureThreshold = lens _uhcFailureThreshold (\ s a -> s{_uhcFailureThreshold = a}) . mapping _Nat;

-- | The IP address of the resource that you want to check.
--
-- Specify this value only if you want to change it.
uhcIPAddress :: Lens' UpdateHealthCheck (Maybe Text)
uhcIPAddress = lens _uhcIPAddress (\ s a -> s{_uhcIPAddress = a});

-- | If the value of 'Type' is 'HTTP_STR_MATCH' or 'HTTP_STR_MATCH', the
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

-- | Optional. When you specify a health check version, Route 53 compares
-- this value with the current value in the health check, which prevents
-- you from updating the health check when the versions don\'t match. Using
-- 'HealthCheckVersion' lets you prevent overwriting another change to the
-- health check.
uhcHealthCheckVersion :: Lens' UpdateHealthCheck (Maybe Natural)
uhcHealthCheckVersion = lens _uhcHealthCheckVersion (\ s a -> s{_uhcHealthCheckVersion = a}) . mapping _Nat;

-- | Fully qualified domain name of the instance to be health checked.
--
-- Specify this value only if you want to change it.
uhcFullyQualifiedDomainName :: Lens' UpdateHealthCheck (Maybe Text)
uhcFullyQualifiedDomainName = lens _uhcFullyQualifiedDomainName (\ s a -> s{_uhcFullyQualifiedDomainName = a});

-- | The port on which you want Route 53 to open a connection to perform
-- health checks.
--
-- Specify this value only if you want to change it.
uhcPort :: Lens' UpdateHealthCheck (Maybe Natural)
uhcPort = lens _uhcPort (\ s a -> s{_uhcPort = a}) . mapping _Nat;

-- | The ID of the health check to update.
uhcHealthCheckId :: Lens' UpdateHealthCheck Text
uhcHealthCheckId = lens _uhcHealthCheckId (\ s a -> s{_uhcHealthCheckId = a});

instance AWSRequest UpdateHealthCheck where
        type Rs UpdateHealthCheck = UpdateHealthCheckResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 UpdateHealthCheckResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HealthCheck"))

instance ToElement UpdateHealthCheck where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHealthCheckRequest"

instance ToHeaders UpdateHealthCheck where
        toHeaders = const mempty

instance ToPath UpdateHealthCheck where
        toPath UpdateHealthCheck'{..}
          = mconcat
              ["/2013-04-01/healthcheck/", toBS _uhcHealthCheckId]

instance ToQuery UpdateHealthCheck where
        toQuery = const mempty

instance ToXML UpdateHealthCheck where
        toXML UpdateHealthCheck'{..}
          = mconcat
              ["FailureThreshold" @= _uhcFailureThreshold,
               "IPAddress" @= _uhcIPAddress,
               "SearchString" @= _uhcSearchString,
               "ResourcePath" @= _uhcResourcePath,
               "HealthCheckVersion" @= _uhcHealthCheckVersion,
               "FullyQualifiedDomainName" @=
                 _uhcFullyQualifiedDomainName,
               "Port" @= _uhcPort]

-- | /See:/ 'updateHealthCheckResponse' smart constructor.
data UpdateHealthCheckResponse = UpdateHealthCheckResponse'
    { _uhcrsStatus      :: !Int
    , _uhcrsHealthCheck :: !HealthCheck
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhcrsStatus'
--
-- * 'uhcrsHealthCheck'
updateHealthCheckResponse
    :: Int -- ^ 'uhcrsStatus'
    -> HealthCheck -- ^ 'uhcrsHealthCheck'
    -> UpdateHealthCheckResponse
updateHealthCheckResponse pStatus_ pHealthCheck_ =
    UpdateHealthCheckResponse'
    { _uhcrsStatus = pStatus_
    , _uhcrsHealthCheck = pHealthCheck_
    }

-- | The response status code.
uhcrsStatus :: Lens' UpdateHealthCheckResponse Int
uhcrsStatus = lens _uhcrsStatus (\ s a -> s{_uhcrsStatus = a});

-- | Undocumented member.
uhcrsHealthCheck :: Lens' UpdateHealthCheckResponse HealthCheck
uhcrsHealthCheck = lens _uhcrsHealthCheck (\ s a -> s{_uhcrsHealthCheck = a});
