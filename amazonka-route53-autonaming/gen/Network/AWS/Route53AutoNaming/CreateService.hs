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
--     * For public and private DNS namespaces, one of the following combinations of DNS records in Amazon Route 53:
--
--     * A
--
--     * AAAA
--
--     * A and AAAA
--
--     * SRV
--
--     * CNAME
--
--
--
--     * Optionally, a health check
--
--
--
-- After you create the service, you can submit a 'RegisterInstance' request, and AWS Cloud Map uses the values in the configuration to create the specified entities.
--
-- For the current limit on the number of instances that you can register using the same namespace and using the same service, see <http://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
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
    , csNamespaceId
    , csDNSConfig
    , csDescription
    , csName

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
  , _csNamespaceId             :: !(Maybe Text)
  , _csDNSConfig               :: !(Maybe DNSConfig)
  , _csDescription             :: !(Maybe Text)
  , _csName                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csHealthCheckConfig' - /Public DNS namespaces only./ A complex type that contains settings for an optional Route 53 health check. If you specify settings for a health check, AWS Cloud Map associates the health check with all the Route 53 DNS records that you specify in @DnsConfig@ . /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both. For information about the charges for health checks, see <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing> .
--
-- * 'csCreatorRequestId' - A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'csHealthCheckCustomConfig' - A complex type that contains information about an optional custom health check. /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- * 'csNamespaceId' - The ID of the namespace that you want to use to create the service.
--
-- * 'csDNSConfig' - A complex type that contains information about the Amazon Route 53 records that you want AWS Cloud Map to create when you register an instance.
--
-- * 'csDescription' - A description for the service.
--
-- * 'csName' - The name that you want to assign to the service.
createService
    :: Text -- ^ 'csName'
    -> CreateService
createService pName_ =
  CreateService'
    { _csHealthCheckConfig = Nothing
    , _csCreatorRequestId = Nothing
    , _csHealthCheckCustomConfig = Nothing
    , _csNamespaceId = Nothing
    , _csDNSConfig = Nothing
    , _csDescription = Nothing
    , _csName = pName_
    }


-- | /Public DNS namespaces only./ A complex type that contains settings for an optional Route 53 health check. If you specify settings for a health check, AWS Cloud Map associates the health check with all the Route 53 DNS records that you specify in @DnsConfig@ . /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both. For information about the charges for health checks, see <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing> .
csHealthCheckConfig :: Lens' CreateService (Maybe HealthCheckConfig)
csHealthCheckConfig = lens _csHealthCheckConfig (\ s a -> s{_csHealthCheckConfig = a})

-- | A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
csCreatorRequestId :: Lens' CreateService (Maybe Text)
csCreatorRequestId = lens _csCreatorRequestId (\ s a -> s{_csCreatorRequestId = a})

-- | A complex type that contains information about an optional custom health check. /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
csHealthCheckCustomConfig :: Lens' CreateService (Maybe HealthCheckCustomConfig)
csHealthCheckCustomConfig = lens _csHealthCheckCustomConfig (\ s a -> s{_csHealthCheckCustomConfig = a})

-- | The ID of the namespace that you want to use to create the service.
csNamespaceId :: Lens' CreateService (Maybe Text)
csNamespaceId = lens _csNamespaceId (\ s a -> s{_csNamespaceId = a})

-- | A complex type that contains information about the Amazon Route 53 records that you want AWS Cloud Map to create when you register an instance.
csDNSConfig :: Lens' CreateService (Maybe DNSConfig)
csDNSConfig = lens _csDNSConfig (\ s a -> s{_csDNSConfig = a})

-- | A description for the service.
csDescription :: Lens' CreateService (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | The name that you want to assign to the service.
csName :: Lens' CreateService Text
csName = lens _csName (\ s a -> s{_csName = a})

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
                  ("NamespaceId" .=) <$> _csNamespaceId,
                  ("DnsConfig" .=) <$> _csDNSConfig,
                  ("Description" .=) <$> _csDescription,
                  Just ("Name" .= _csName)])

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
