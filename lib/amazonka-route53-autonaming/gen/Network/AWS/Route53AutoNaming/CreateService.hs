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
-- Module      : Network.AWS.Route53AutoNaming.CreateService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service, which defines the configuration for the following entities:
--
--
--     * Up to three records (A, AAAA, and SRV) or one CNAME record
--
--     * Optionally, a health check
--
--
--
-- After you create the service, you can submit a 'RegisterInstance' request, and Amazon Route 53 uses the values in the configuration to create the specified entities.
--
-- For the current limit on the number of instances that you can register using the same namespace and using the same service, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html#limits-api-entities-autonaming Limits on Auto Naming> in the /Route 53 Developer Guide/ .
--
module Network.AWS.Route53AutoNaming.CreateService
    (
    -- * Creating a Request
      createService
    , CreateService
    -- * Request Lenses
    , csHealthCheckConfig
    , csCreatorRequestId
    , csHealthCheckCustomConfig
    , csDescription
    , csName
    , csDNSConfig

    -- * Destructuring the Response
    , createServiceResponse
    , CreateServiceResponse
    -- * Response Lenses
    , csrsService
    , csrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'createService' smart constructor.
data CreateService = CreateService'
  { _csHealthCheckConfig       :: !(Maybe HealthCheckConfig)
  , _csCreatorRequestId        :: !(Maybe Text)
  , _csHealthCheckCustomConfig :: !(Maybe HealthCheckCustomConfig)
  , _csDescription             :: !(Maybe Text)
  , _csName                    :: !Text
  , _csDNSConfig               :: !DNSConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csHealthCheckConfig' - /Public DNS namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, Route 53 associates the health check with all the records that you specify in @DnsConfig@ . For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing Route 53 Pricing> .
--
-- * 'csCreatorRequestId' - A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'csHealthCheckCustomConfig' - Undocumented member.
--
-- * 'csDescription' - A description for the service.
--
-- * 'csName' - The name that you want to assign to the service.
--
-- * 'csDNSConfig' - A complex type that contains information about the records that you want Route 53 to create when you register an instance.
createService
    :: Text -- ^ 'csName'
    -> DNSConfig -- ^ 'csDNSConfig'
    -> CreateService
createService pName_ pDNSConfig_ =
  CreateService'
    { _csHealthCheckConfig = Nothing
    , _csCreatorRequestId = Nothing
    , _csHealthCheckCustomConfig = Nothing
    , _csDescription = Nothing
    , _csName = pName_
    , _csDNSConfig = pDNSConfig_
    }


-- | /Public DNS namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, Route 53 associates the health check with all the records that you specify in @DnsConfig@ . For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing Route 53 Pricing> .
csHealthCheckConfig :: Lens' CreateService (Maybe HealthCheckConfig)
csHealthCheckConfig = lens _csHealthCheckConfig (\ s a -> s{_csHealthCheckConfig = a})

-- | A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
csCreatorRequestId :: Lens' CreateService (Maybe Text)
csCreatorRequestId = lens _csCreatorRequestId (\ s a -> s{_csCreatorRequestId = a})

-- | Undocumented member.
csHealthCheckCustomConfig :: Lens' CreateService (Maybe HealthCheckCustomConfig)
csHealthCheckCustomConfig = lens _csHealthCheckCustomConfig (\ s a -> s{_csHealthCheckCustomConfig = a})

-- | A description for the service.
csDescription :: Lens' CreateService (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | The name that you want to assign to the service.
csName :: Lens' CreateService Text
csName = lens _csName (\ s a -> s{_csName = a})

-- | A complex type that contains information about the records that you want Route 53 to create when you register an instance.
csDNSConfig :: Lens' CreateService DNSConfig
csDNSConfig = lens _csDNSConfig (\ s a -> s{_csDNSConfig = a})

instance AWSRequest CreateService where
        type Rs CreateService = CreateServiceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 CreateServiceResponse' <$>
                   (x .?> "Service") <*> (pure (fromEnum s)))

instance Hashable CreateService where

instance NFData CreateService where

instance ToHeaders CreateService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.CreateService" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateService where
        toJSON CreateService'{..}
          = object
              (catMaybes
                 [("HealthCheckConfig" .=) <$> _csHealthCheckConfig,
                  ("CreatorRequestId" .=) <$> _csCreatorRequestId,
                  ("HealthCheckCustomConfig" .=) <$>
                    _csHealthCheckCustomConfig,
                  ("Description" .=) <$> _csDescription,
                  Just ("Name" .= _csName),
                  Just ("DnsConfig" .= _csDNSConfig)])

instance ToPath CreateService where
        toPath = const "/"

instance ToQuery CreateService where
        toQuery = const mempty

-- | /See:/ 'createServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { _csrsService        :: !(Maybe ServiceInfo)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsService' - A complex type that contains information about the new service.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createServiceResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateServiceResponse
createServiceResponse pResponseStatus_ =
  CreateServiceResponse'
    {_csrsService = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | A complex type that contains information about the new service.
csrsService :: Lens' CreateServiceResponse (Maybe ServiceInfo)
csrsService = lens _csrsService (\ s a -> s{_csrsService = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateServiceResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateServiceResponse where
