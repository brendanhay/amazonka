{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.UpdateHealthCheck
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action updates an existing health check.
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
    , uhcrqIPAddress
    , uhcrqFailureThreshold
    , uhcrqSearchString
    , uhcrqResourcePath
    , uhcrqHealthCheckVersion
    , uhcrqFullyQualifiedDomainName
    , uhcrqPort
    , uhcrqHealthCheckId

    -- * Response
    , UpdateHealthCheckResponse
    -- ** Response constructor
    , updateHealthCheckResponse
    -- ** Response lenses
    , uhcrsStatus
    , uhcrsHealthCheck
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | >A complex type that contains information about the request to update a
-- health check.
--
-- /See:/ 'updateHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uhcrqIPAddress'
--
-- * 'uhcrqFailureThreshold'
--
-- * 'uhcrqSearchString'
--
-- * 'uhcrqResourcePath'
--
-- * 'uhcrqHealthCheckVersion'
--
-- * 'uhcrqFullyQualifiedDomainName'
--
-- * 'uhcrqPort'
--
-- * 'uhcrqHealthCheckId'
data UpdateHealthCheck = UpdateHealthCheck'
    { _uhcrqIPAddress                :: !(Maybe Text)
    , _uhcrqFailureThreshold         :: !(Maybe Nat)
    , _uhcrqSearchString             :: !(Maybe Text)
    , _uhcrqResourcePath             :: !(Maybe Text)
    , _uhcrqHealthCheckVersion       :: !(Maybe Nat)
    , _uhcrqFullyQualifiedDomainName :: !(Maybe Text)
    , _uhcrqPort                     :: !(Maybe Nat)
    , _uhcrqHealthCheckId            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateHealthCheck' smart constructor.
updateHealthCheck :: Text -> UpdateHealthCheck
updateHealthCheck pHealthCheckId =
    UpdateHealthCheck'
    { _uhcrqIPAddress = Nothing
    , _uhcrqFailureThreshold = Nothing
    , _uhcrqSearchString = Nothing
    , _uhcrqResourcePath = Nothing
    , _uhcrqHealthCheckVersion = Nothing
    , _uhcrqFullyQualifiedDomainName = Nothing
    , _uhcrqPort = Nothing
    , _uhcrqHealthCheckId = pHealthCheckId
    }

-- | The IP address of the resource that you want to check.
--
-- Specify this value only if you want to change it.
uhcrqIPAddress :: Lens' UpdateHealthCheck (Maybe Text)
uhcrqIPAddress = lens _uhcrqIPAddress (\ s a -> s{_uhcrqIPAddress = a});

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
--
-- Specify this value only if you want to change it.
uhcrqFailureThreshold :: Lens' UpdateHealthCheck (Maybe Natural)
uhcrqFailureThreshold = lens _uhcrqFailureThreshold (\ s a -> s{_uhcrqFailureThreshold = a}) . mapping _Nat;

-- | If the value of @Type@ is @HTTP_STR_MATCH@ or @HTTP_STR_MATCH@, the
-- string that you want Route 53 to search for in the response body from
-- the specified resource. If the string appears in the response body,
-- Route 53 considers the resource healthy.
--
-- Specify this value only if you want to change it.
uhcrqSearchString :: Lens' UpdateHealthCheck (Maybe Text)
uhcrqSearchString = lens _uhcrqSearchString (\ s a -> s{_uhcrqSearchString = a});

-- | The path that you want Amazon Route 53 to request when performing health
-- checks. The path can be any value for which your endpoint will return an
-- HTTP status code of 2xx or 3xx when the endpoint is healthy, for example
-- the file \/docs\/route53-health-check.html.
--
-- Specify this value only if you want to change it.
uhcrqResourcePath :: Lens' UpdateHealthCheck (Maybe Text)
uhcrqResourcePath = lens _uhcrqResourcePath (\ s a -> s{_uhcrqResourcePath = a});

-- | Optional. When you specify a health check version, Route 53 compares
-- this value with the current value in the health check, which prevents
-- you from updating the health check when the versions don\'t match. Using
-- @HealthCheckVersion@ lets you prevent overwriting another change to the
-- health check.
uhcrqHealthCheckVersion :: Lens' UpdateHealthCheck (Maybe Natural)
uhcrqHealthCheckVersion = lens _uhcrqHealthCheckVersion (\ s a -> s{_uhcrqHealthCheckVersion = a}) . mapping _Nat;

-- | Fully qualified domain name of the instance to be health checked.
--
-- Specify this value only if you want to change it.
uhcrqFullyQualifiedDomainName :: Lens' UpdateHealthCheck (Maybe Text)
uhcrqFullyQualifiedDomainName = lens _uhcrqFullyQualifiedDomainName (\ s a -> s{_uhcrqFullyQualifiedDomainName = a});

-- | The port on which you want Route 53 to open a connection to perform
-- health checks.
--
-- Specify this value only if you want to change it.
uhcrqPort :: Lens' UpdateHealthCheck (Maybe Natural)
uhcrqPort = lens _uhcrqPort (\ s a -> s{_uhcrqPort = a}) . mapping _Nat;

-- | The ID of the health check to update.
uhcrqHealthCheckId :: Lens' UpdateHealthCheck Text
uhcrqHealthCheckId = lens _uhcrqHealthCheckId (\ s a -> s{_uhcrqHealthCheckId = a});

instance AWSRequest UpdateHealthCheck where
        type Sv UpdateHealthCheck = Route53
        type Rs UpdateHealthCheck = UpdateHealthCheckResponse
        request = postXML
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
              ["/2013-04-01/healthcheck/",
               toText _uhcrqHealthCheckId]

instance ToQuery UpdateHealthCheck where
        toQuery = const mempty

instance ToXML UpdateHealthCheck where
        toXML UpdateHealthCheck'{..}
          = mconcat
              ["IPAddress" @= _uhcrqIPAddress,
               "FailureThreshold" @= _uhcrqFailureThreshold,
               "SearchString" @= _uhcrqSearchString,
               "ResourcePath" @= _uhcrqResourcePath,
               "HealthCheckVersion" @= _uhcrqHealthCheckVersion,
               "FullyQualifiedDomainName" @=
                 _uhcrqFullyQualifiedDomainName,
               "Port" @= _uhcrqPort]

-- | /See:/ 'updateHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uhcrsStatus'
--
-- * 'uhcrsHealthCheck'
data UpdateHealthCheckResponse = UpdateHealthCheckResponse'
    { _uhcrsStatus      :: !Int
    , _uhcrsHealthCheck :: !HealthCheck
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateHealthCheckResponse' smart constructor.
updateHealthCheckResponse :: Int -> HealthCheck -> UpdateHealthCheckResponse
updateHealthCheckResponse pStatus pHealthCheck =
    UpdateHealthCheckResponse'
    { _uhcrsStatus = pStatus
    , _uhcrsHealthCheck = pHealthCheck
    }

-- | FIXME: Undocumented member.
uhcrsStatus :: Lens' UpdateHealthCheckResponse Int
uhcrsStatus = lens _uhcrsStatus (\ s a -> s{_uhcrsStatus = a});

-- | FIXME: Undocumented member.
uhcrsHealthCheck :: Lens' UpdateHealthCheckResponse HealthCheck
uhcrsHealthCheck = lens _uhcrsHealthCheck (\ s a -> s{_uhcrsHealthCheck = a});
