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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action updates an existing health check.
--
-- To update a health check, send a 'POST' request to the
-- '\/Route 53 API version\/healthcheck\/health check ID' resource. The
-- request body must include a document with an 'UpdateHealthCheckRequest'
-- element. The response returns an 'UpdateHealthCheckResponse' element,
-- which contains metadata about the health check.
module Network.AWS.Route53.UpdateHealthCheck
    (
    -- * Creating a Request
      updateHealthCheck
    , UpdateHealthCheck
    -- * Request Lenses
    , uhcFailureThreshold
    , uhcIPAddress
    , uhcEnableSNI
    , uhcSearchString
    , uhcHealthThreshold
    , uhcRegions
    , uhcResourcePath
    , uhcInsufficientDataHealthStatus
    , uhcHealthCheckVersion
    , uhcAlarmIdentifier
    , uhcInverted
    , uhcFullyQualifiedDomainName
    , uhcChildHealthChecks
    , uhcPort
    , uhcHealthCheckId

    -- * Destructuring the Response
    , updateHealthCheckResponse
    , UpdateHealthCheckResponse
    -- * Response Lenses
    , uhcrsResponseStatus
    , uhcrsHealthCheck
    ) where

import           Network.AWS.Lens
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
    { _uhcFailureThreshold             :: !(Maybe Nat)
    , _uhcIPAddress                    :: !(Maybe Text)
    , _uhcEnableSNI                    :: !(Maybe Bool)
    , _uhcSearchString                 :: !(Maybe Text)
    , _uhcHealthThreshold              :: !(Maybe Nat)
    , _uhcRegions                      :: !(Maybe (List1 HealthCheckRegion))
    , _uhcResourcePath                 :: !(Maybe Text)
    , _uhcInsufficientDataHealthStatus :: !(Maybe InsufficientDataHealthStatus)
    , _uhcHealthCheckVersion           :: !(Maybe Nat)
    , _uhcAlarmIdentifier              :: !(Maybe AlarmIdentifier)
    , _uhcInverted                     :: !(Maybe Bool)
    , _uhcFullyQualifiedDomainName     :: !(Maybe Text)
    , _uhcChildHealthChecks            :: !(Maybe [Text])
    , _uhcPort                         :: !(Maybe Nat)
    , _uhcHealthCheckId                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhcFailureThreshold'
--
-- * 'uhcIPAddress'
--
-- * 'uhcEnableSNI'
--
-- * 'uhcSearchString'
--
-- * 'uhcHealthThreshold'
--
-- * 'uhcRegions'
--
-- * 'uhcResourcePath'
--
-- * 'uhcInsufficientDataHealthStatus'
--
-- * 'uhcHealthCheckVersion'
--
-- * 'uhcAlarmIdentifier'
--
-- * 'uhcInverted'
--
-- * 'uhcFullyQualifiedDomainName'
--
-- * 'uhcChildHealthChecks'
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
    , _uhcEnableSNI = Nothing
    , _uhcSearchString = Nothing
    , _uhcHealthThreshold = Nothing
    , _uhcRegions = Nothing
    , _uhcResourcePath = Nothing
    , _uhcInsufficientDataHealthStatus = Nothing
    , _uhcHealthCheckVersion = Nothing
    , _uhcAlarmIdentifier = Nothing
    , _uhcInverted = Nothing
    , _uhcFullyQualifiedDomainName = Nothing
    , _uhcChildHealthChecks = Nothing
    , _uhcPort = Nothing
    , _uhcHealthCheckId = pHealthCheckId_
    }

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Amazon Route 53 to change the current status of the endpoint
-- from unhealthy to healthy or vice versa.
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

-- | Specify whether you want Amazon Route 53 to send the value of
-- 'FullyQualifiedDomainName' to the endpoint in the 'client_hello' message
-- during TLS negotiation. If you don\'t specify a value for 'EnableSNI',
-- Amazon Route 53 defaults to 'true' when 'Type' is 'HTTPS' or
-- 'HTTPS_STR_MATCH' and defaults to 'false' when 'Type' is any other
-- value.
--
-- Specify this value only if you want to change it.
uhcEnableSNI :: Lens' UpdateHealthCheck (Maybe Bool)
uhcEnableSNI = lens _uhcEnableSNI (\ s a -> s{_uhcEnableSNI = a});

-- | If the value of 'Type' is 'HTTP_STR_MATCH' or 'HTTP_STR_MATCH', the
-- string that you want Amazon Route 53 to search for in the response body
-- from the specified resource. If the string appears in the response body,
-- Amazon Route 53 considers the resource healthy. Amazon Route 53
-- considers case when searching for 'SearchString' in the response body.
--
-- Specify this value only if you want to change it.
uhcSearchString :: Lens' UpdateHealthCheck (Maybe Text)
uhcSearchString = lens _uhcSearchString (\ s a -> s{_uhcSearchString = a});

-- | The minimum number of child health checks that must be healthy for
-- Amazon Route 53 to consider the parent health check to be healthy. Valid
-- values are integers between 0 and 256, inclusive.
--
-- Specify this value only if you want to change it.
uhcHealthThreshold :: Lens' UpdateHealthCheck (Maybe Natural)
uhcHealthThreshold = lens _uhcHealthThreshold (\ s a -> s{_uhcHealthThreshold = a}) . mapping _Nat;

-- | A list of 'HealthCheckRegion' values that specify the Amazon EC2 regions
-- that you want Amazon Route 53 to use to perform health checks. You must
-- specify at least three regions.
--
-- When you remove a region from the list, Amazon Route 53 will briefly
-- continue to check your endpoint from that region.
--
-- Specify this value only if you want to change it.
uhcRegions :: Lens' UpdateHealthCheck (Maybe (NonEmpty HealthCheckRegion))
uhcRegions = lens _uhcRegions (\ s a -> s{_uhcRegions = a}) . mapping _List1;

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file \/docs\/route53-health-check.html.
--
-- Specify this value only if you want to change it.
uhcResourcePath :: Lens' UpdateHealthCheck (Maybe Text)
uhcResourcePath = lens _uhcResourcePath (\ s a -> s{_uhcResourcePath = a});

-- | Undocumented member.
uhcInsufficientDataHealthStatus :: Lens' UpdateHealthCheck (Maybe InsufficientDataHealthStatus)
uhcInsufficientDataHealthStatus = lens _uhcInsufficientDataHealthStatus (\ s a -> s{_uhcInsufficientDataHealthStatus = a});

-- | Optional. When you specify a health check version, Amazon Route 53
-- compares this value with the current value in the health check, which
-- prevents you from updating the health check when the versions don\'t
-- match. Using 'HealthCheckVersion' lets you prevent overwriting another
-- change to the health check.
uhcHealthCheckVersion :: Lens' UpdateHealthCheck (Maybe Natural)
uhcHealthCheckVersion = lens _uhcHealthCheckVersion (\ s a -> s{_uhcHealthCheckVersion = a}) . mapping _Nat;

-- | Undocumented member.
uhcAlarmIdentifier :: Lens' UpdateHealthCheck (Maybe AlarmIdentifier)
uhcAlarmIdentifier = lens _uhcAlarmIdentifier (\ s a -> s{_uhcAlarmIdentifier = a});

-- | A boolean value that indicates whether the status of health check should
-- be inverted. For example, if a health check is healthy but 'Inverted' is
-- 'True', then Amazon Route 53 considers the health check to be unhealthy.
--
-- Specify this value only if you want to change it.
uhcInverted :: Lens' UpdateHealthCheck (Maybe Bool)
uhcInverted = lens _uhcInverted (\ s a -> s{_uhcInverted = a});

-- | Fully qualified domain name of the instance to be health checked.
--
-- Specify this value only if you want to change it.
uhcFullyQualifiedDomainName :: Lens' UpdateHealthCheck (Maybe Text)
uhcFullyQualifiedDomainName = lens _uhcFullyQualifiedDomainName (\ s a -> s{_uhcFullyQualifiedDomainName = a});

-- | For a specified parent health check, a list of 'HealthCheckId' values
-- for the associated child health checks.
--
-- Specify this value only if you want to change it.
uhcChildHealthChecks :: Lens' UpdateHealthCheck [Text]
uhcChildHealthChecks = lens _uhcChildHealthChecks (\ s a -> s{_uhcChildHealthChecks = a}) . _Default . _Coerce;

-- | The port on which you want Amazon Route 53 to open a connection to
-- perform health checks.
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

instance Hashable UpdateHealthCheck

instance NFData UpdateHealthCheck

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
               "EnableSNI" @= _uhcEnableSNI,
               "SearchString" @= _uhcSearchString,
               "HealthThreshold" @= _uhcHealthThreshold,
               "Regions" @=
                 toXML (toXMLList "Region" <$> _uhcRegions),
               "ResourcePath" @= _uhcResourcePath,
               "InsufficientDataHealthStatus" @=
                 _uhcInsufficientDataHealthStatus,
               "HealthCheckVersion" @= _uhcHealthCheckVersion,
               "AlarmIdentifier" @= _uhcAlarmIdentifier,
               "Inverted" @= _uhcInverted,
               "FullyQualifiedDomainName" @=
                 _uhcFullyQualifiedDomainName,
               "ChildHealthChecks" @=
                 toXML
                   (toXMLList "ChildHealthCheck" <$>
                      _uhcChildHealthChecks),
               "Port" @= _uhcPort]

-- | /See:/ 'updateHealthCheckResponse' smart constructor.
data UpdateHealthCheckResponse = UpdateHealthCheckResponse'
    { _uhcrsResponseStatus :: !Int
    , _uhcrsHealthCheck    :: !HealthCheck
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhcrsResponseStatus'
--
-- * 'uhcrsHealthCheck'
updateHealthCheckResponse
    :: Int -- ^ 'uhcrsResponseStatus'
    -> HealthCheck -- ^ 'uhcrsHealthCheck'
    -> UpdateHealthCheckResponse
updateHealthCheckResponse pResponseStatus_ pHealthCheck_ =
    UpdateHealthCheckResponse'
    { _uhcrsResponseStatus = pResponseStatus_
    , _uhcrsHealthCheck = pHealthCheck_
    }

-- | The response status code.
uhcrsResponseStatus :: Lens' UpdateHealthCheckResponse Int
uhcrsResponseStatus = lens _uhcrsResponseStatus (\ s a -> s{_uhcrsResponseStatus = a});

-- | Undocumented member.
uhcrsHealthCheck :: Lens' UpdateHealthCheckResponse HealthCheck
uhcrsHealthCheck = lens _uhcrsHealthCheck (\ s a -> s{_uhcrsHealthCheck = a});

instance NFData UpdateHealthCheckResponse
