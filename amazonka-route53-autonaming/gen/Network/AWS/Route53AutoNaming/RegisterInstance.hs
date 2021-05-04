{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.RegisterInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates one or more records and, optionally, creates a health
-- check based on the settings in a specified service. When you submit a
-- @RegisterInstance@ request, the following occurs:
--
-- -   For each DNS record that you define in the service that is specified
--     by @ServiceId@, a record is created or updated in the hosted zone
--     that is associated with the corresponding namespace.
--
-- -   If the service includes @HealthCheckConfig@, a health check is
--     created based on the settings in the health check configuration.
--
-- -   The health check, if any, is associated with each of the new or
--     updated records.
--
-- One @RegisterInstance@ request must complete before you can submit
-- another request and specify the same service ID and instance ID.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
--
-- When AWS Cloud Map receives a DNS query for the specified DNS name, it
-- returns the applicable value:
--
-- -   __If the health check is healthy__: returns all the records
--
-- -   __If the health check is unhealthy__: returns the applicable value
--     for the last healthy instance
--
-- -   __If you didn\'t specify a health check configuration__: returns all
--     the records
--
-- For the current quota on the number of instances that you can register
-- using the same namespace and using the same service, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits>
-- in the /AWS Cloud Map Developer Guide/.
module Network.AWS.Route53AutoNaming.RegisterInstance
  ( -- * Creating a Request
    RegisterInstance (..),
    newRegisterInstance,

    -- * Request Lenses
    registerInstance_creatorRequestId,
    registerInstance_serviceId,
    registerInstance_instanceId,
    registerInstance_attributes,

    -- * Destructuring the Response
    RegisterInstanceResponse (..),
    newRegisterInstanceResponse,

    -- * Response Lenses
    registerInstanceResponse_operationId,
    registerInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newRegisterInstance' smart constructor.
data RegisterInstance = RegisterInstance'
  { -- | A unique string that identifies the request and that allows failed
    -- @RegisterInstance@ requests to be retried without the risk of executing
    -- the operation twice. You must use a unique @CreatorRequestId@ string
    -- every time you submit a @RegisterInstance@ request if you\'re
    -- registering additional instances for the same namespace and service.
    -- @CreatorRequestId@ can be any unique string, for example, a date\/time
    -- stamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service that you want to use for settings for the
    -- instance.
    serviceId :: Prelude.Text,
    -- | An identifier that you want to associate with the instance. Note the
    -- following:
    --
    -- -   If the service that is specified by @ServiceId@ includes settings
    --     for an @SRV@ record, the value of @InstanceId@ is automatically
    --     included as part of the value for the @SRV@ record. For more
    --     information, see
    --     <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type>.
    --
    -- -   You can use this value to update an existing instance.
    --
    -- -   To register a new instance, you must specify a value that is unique
    --     among instances that you register by using the same service.
    --
    -- -   If you specify an existing @InstanceId@ and @ServiceId@, AWS Cloud
    --     Map updates the existing DNS records, if any. If there\'s also an
    --     existing health check, AWS Cloud Map deletes the old health check
    --     and creates a new one.
    --
    --     The health check isn\'t deleted immediately, so it will still appear
    --     for a while if you submit a @ListHealthChecks@ request, for example.
    instanceId :: Prelude.Text,
    -- | A string map that contains the following information for the service
    -- that you specify in @ServiceId@:
    --
    -- -   The attributes that apply to the records that are defined in the
    --     service.
    --
    -- -   For each attribute, the applicable value.
    --
    -- Supported attribute keys include the following:
    --
    -- __AWS_ALIAS_DNS_NAME__
    --
    -- If you want AWS Cloud Map to create an Amazon Route 53 alias record that
    -- routes traffic to an Elastic Load Balancing load balancer, specify the
    -- DNS name that is associated with the load balancer. For information
    -- about how to get the DNS name, see \"DNSName\" in the topic
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget>
    -- in the /Route 53 API Reference/.
    --
    -- Note the following:
    --
    -- -   The configuration for the service that is specified by @ServiceId@
    --     must include settings for an @A@ record, an @AAAA@ record, or both.
    --
    -- -   In the service that is specified by @ServiceId@, the value of
    --     @RoutingPolicy@ must be @WEIGHTED@.
    --
    -- -   If the service that is specified by @ServiceId@ includes
    --     @HealthCheckConfig@ settings, AWS Cloud Map will create the Route 53
    --     health check, but it won\'t associate the health check with the
    --     alias record.
    --
    -- -   Auto naming currently doesn\'t support creating alias records that
    --     route traffic to AWS resources other than Elastic Load Balancing
    --     load balancers.
    --
    -- -   If you specify a value for @AWS_ALIAS_DNS_NAME@, don\'t specify
    --     values for any of the @AWS_INSTANCE@ attributes.
    --
    -- __AWS_EC2_INSTANCE_ID__
    --
    -- /HTTP namespaces only./ The Amazon EC2 instance ID for the instance. If
    -- the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the only other
    -- attribute that can be specified is @AWS_INIT_HEALTH_STATUS@. When the
    -- @AWS_EC2_INSTANCE_ID@ attribute is specified, then the
    -- @AWS_INSTANCE_IPV4@ attribute will be filled out with the primary
    -- private IPv4 address.
    --
    -- __AWS_INIT_HEALTH_STATUS__
    --
    -- If the service configuration includes @HealthCheckCustomConfig@, you can
    -- optionally use @AWS_INIT_HEALTH_STATUS@ to specify the initial status of
    -- the custom health check, @HEALTHY@ or @UNHEALTHY@. If you don\'t specify
    -- a value for @AWS_INIT_HEALTH_STATUS@, the initial status is @HEALTHY@.
    --
    -- __AWS_INSTANCE_CNAME__
    --
    -- If the service configuration includes a @CNAME@ record, the domain name
    -- that you want Route 53 to return in response to DNS queries, for
    -- example, @example.com@.
    --
    -- This value is required if the service specified by @ServiceId@ includes
    -- settings for an @CNAME@ record.
    --
    -- __AWS_INSTANCE_IPV4__
    --
    -- If the service configuration includes an @A@ record, the IPv4 address
    -- that you want Route 53 to return in response to DNS queries, for
    -- example, @192.0.2.44@.
    --
    -- This value is required if the service specified by @ServiceId@ includes
    -- settings for an @A@ record. If the service includes settings for an
    -- @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@,
    -- @AWS_INSTANCE_IPV6@, or both.
    --
    -- __AWS_INSTANCE_IPV6__
    --
    -- If the service configuration includes an @AAAA@ record, the IPv6 address
    -- that you want Route 53 to return in response to DNS queries, for
    -- example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@.
    --
    -- This value is required if the service specified by @ServiceId@ includes
    -- settings for an @AAAA@ record. If the service includes settings for an
    -- @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@,
    -- @AWS_INSTANCE_IPV6@, or both.
    --
    -- __AWS_INSTANCE_PORT__
    --
    -- If the service includes an @SRV@ record, the value that you want
    -- Route 53 to return for the port.
    --
    -- If the service includes @HealthCheckConfig@, the port on the endpoint
    -- that you want Route 53 to send requests to.
    --
    -- This value is required if you specified settings for an @SRV@ record or
    -- a Route 53 health check when you created the service.
    --
    -- __Custom attributes__
    --
    -- You can add up to 30 custom attributes. For each key-value pair, the
    -- maximum length of the attribute name is 255 characters, and the maximum
    -- length of the attribute value is 1,024 characters. The total size of all
    -- provided attributes (sum of all keys and values) must not exceed 5,000
    -- characters.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorRequestId', 'registerInstance_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @RegisterInstance@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CreatorRequestId@ string
-- every time you submit a @RegisterInstance@ request if you\'re
-- registering additional instances for the same namespace and service.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
--
-- 'serviceId', 'registerInstance_serviceId' - The ID of the service that you want to use for settings for the
-- instance.
--
-- 'instanceId', 'registerInstance_instanceId' - An identifier that you want to associate with the instance. Note the
-- following:
--
-- -   If the service that is specified by @ServiceId@ includes settings
--     for an @SRV@ record, the value of @InstanceId@ is automatically
--     included as part of the value for the @SRV@ record. For more
--     information, see
--     <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type>.
--
-- -   You can use this value to update an existing instance.
--
-- -   To register a new instance, you must specify a value that is unique
--     among instances that you register by using the same service.
--
-- -   If you specify an existing @InstanceId@ and @ServiceId@, AWS Cloud
--     Map updates the existing DNS records, if any. If there\'s also an
--     existing health check, AWS Cloud Map deletes the old health check
--     and creates a new one.
--
--     The health check isn\'t deleted immediately, so it will still appear
--     for a while if you submit a @ListHealthChecks@ request, for example.
--
-- 'attributes', 'registerInstance_attributes' - A string map that contains the following information for the service
-- that you specify in @ServiceId@:
--
-- -   The attributes that apply to the records that are defined in the
--     service.
--
-- -   For each attribute, the applicable value.
--
-- Supported attribute keys include the following:
--
-- __AWS_ALIAS_DNS_NAME__
--
-- If you want AWS Cloud Map to create an Amazon Route 53 alias record that
-- routes traffic to an Elastic Load Balancing load balancer, specify the
-- DNS name that is associated with the load balancer. For information
-- about how to get the DNS name, see \"DNSName\" in the topic
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget>
-- in the /Route 53 API Reference/.
--
-- Note the following:
--
-- -   The configuration for the service that is specified by @ServiceId@
--     must include settings for an @A@ record, an @AAAA@ record, or both.
--
-- -   In the service that is specified by @ServiceId@, the value of
--     @RoutingPolicy@ must be @WEIGHTED@.
--
-- -   If the service that is specified by @ServiceId@ includes
--     @HealthCheckConfig@ settings, AWS Cloud Map will create the Route 53
--     health check, but it won\'t associate the health check with the
--     alias record.
--
-- -   Auto naming currently doesn\'t support creating alias records that
--     route traffic to AWS resources other than Elastic Load Balancing
--     load balancers.
--
-- -   If you specify a value for @AWS_ALIAS_DNS_NAME@, don\'t specify
--     values for any of the @AWS_INSTANCE@ attributes.
--
-- __AWS_EC2_INSTANCE_ID__
--
-- /HTTP namespaces only./ The Amazon EC2 instance ID for the instance. If
-- the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the only other
-- attribute that can be specified is @AWS_INIT_HEALTH_STATUS@. When the
-- @AWS_EC2_INSTANCE_ID@ attribute is specified, then the
-- @AWS_INSTANCE_IPV4@ attribute will be filled out with the primary
-- private IPv4 address.
--
-- __AWS_INIT_HEALTH_STATUS__
--
-- If the service configuration includes @HealthCheckCustomConfig@, you can
-- optionally use @AWS_INIT_HEALTH_STATUS@ to specify the initial status of
-- the custom health check, @HEALTHY@ or @UNHEALTHY@. If you don\'t specify
-- a value for @AWS_INIT_HEALTH_STATUS@, the initial status is @HEALTHY@.
--
-- __AWS_INSTANCE_CNAME__
--
-- If the service configuration includes a @CNAME@ record, the domain name
-- that you want Route 53 to return in response to DNS queries, for
-- example, @example.com@.
--
-- This value is required if the service specified by @ServiceId@ includes
-- settings for an @CNAME@ record.
--
-- __AWS_INSTANCE_IPV4__
--
-- If the service configuration includes an @A@ record, the IPv4 address
-- that you want Route 53 to return in response to DNS queries, for
-- example, @192.0.2.44@.
--
-- This value is required if the service specified by @ServiceId@ includes
-- settings for an @A@ record. If the service includes settings for an
-- @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@,
-- @AWS_INSTANCE_IPV6@, or both.
--
-- __AWS_INSTANCE_IPV6__
--
-- If the service configuration includes an @AAAA@ record, the IPv6 address
-- that you want Route 53 to return in response to DNS queries, for
-- example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@.
--
-- This value is required if the service specified by @ServiceId@ includes
-- settings for an @AAAA@ record. If the service includes settings for an
-- @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@,
-- @AWS_INSTANCE_IPV6@, or both.
--
-- __AWS_INSTANCE_PORT__
--
-- If the service includes an @SRV@ record, the value that you want
-- Route 53 to return for the port.
--
-- If the service includes @HealthCheckConfig@, the port on the endpoint
-- that you want Route 53 to send requests to.
--
-- This value is required if you specified settings for an @SRV@ record or
-- a Route 53 health check when you created the service.
--
-- __Custom attributes__
--
-- You can add up to 30 custom attributes. For each key-value pair, the
-- maximum length of the attribute name is 255 characters, and the maximum
-- length of the attribute value is 1,024 characters. The total size of all
-- provided attributes (sum of all keys and values) must not exceed 5,000
-- characters.
newRegisterInstance ::
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  RegisterInstance
newRegisterInstance pServiceId_ pInstanceId_ =
  RegisterInstance'
    { creatorRequestId =
        Prelude.Nothing,
      serviceId = pServiceId_,
      instanceId = pInstanceId_,
      attributes = Prelude.mempty
    }

-- | A unique string that identifies the request and that allows failed
-- @RegisterInstance@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CreatorRequestId@ string
-- every time you submit a @RegisterInstance@ request if you\'re
-- registering additional instances for the same namespace and service.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
registerInstance_creatorRequestId :: Lens.Lens' RegisterInstance (Prelude.Maybe Prelude.Text)
registerInstance_creatorRequestId = Lens.lens (\RegisterInstance' {creatorRequestId} -> creatorRequestId) (\s@RegisterInstance' {} a -> s {creatorRequestId = a} :: RegisterInstance)

-- | The ID of the service that you want to use for settings for the
-- instance.
registerInstance_serviceId :: Lens.Lens' RegisterInstance Prelude.Text
registerInstance_serviceId = Lens.lens (\RegisterInstance' {serviceId} -> serviceId) (\s@RegisterInstance' {} a -> s {serviceId = a} :: RegisterInstance)

-- | An identifier that you want to associate with the instance. Note the
-- following:
--
-- -   If the service that is specified by @ServiceId@ includes settings
--     for an @SRV@ record, the value of @InstanceId@ is automatically
--     included as part of the value for the @SRV@ record. For more
--     information, see
--     <https://docs.aws.amazon.com/cloud-map/latest/api/API_DnsRecord.html#cloudmap-Type-DnsRecord-Type DnsRecord > Type>.
--
-- -   You can use this value to update an existing instance.
--
-- -   To register a new instance, you must specify a value that is unique
--     among instances that you register by using the same service.
--
-- -   If you specify an existing @InstanceId@ and @ServiceId@, AWS Cloud
--     Map updates the existing DNS records, if any. If there\'s also an
--     existing health check, AWS Cloud Map deletes the old health check
--     and creates a new one.
--
--     The health check isn\'t deleted immediately, so it will still appear
--     for a while if you submit a @ListHealthChecks@ request, for example.
registerInstance_instanceId :: Lens.Lens' RegisterInstance Prelude.Text
registerInstance_instanceId = Lens.lens (\RegisterInstance' {instanceId} -> instanceId) (\s@RegisterInstance' {} a -> s {instanceId = a} :: RegisterInstance)

-- | A string map that contains the following information for the service
-- that you specify in @ServiceId@:
--
-- -   The attributes that apply to the records that are defined in the
--     service.
--
-- -   For each attribute, the applicable value.
--
-- Supported attribute keys include the following:
--
-- __AWS_ALIAS_DNS_NAME__
--
-- If you want AWS Cloud Map to create an Amazon Route 53 alias record that
-- routes traffic to an Elastic Load Balancing load balancer, specify the
-- DNS name that is associated with the load balancer. For information
-- about how to get the DNS name, see \"DNSName\" in the topic
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html AliasTarget>
-- in the /Route 53 API Reference/.
--
-- Note the following:
--
-- -   The configuration for the service that is specified by @ServiceId@
--     must include settings for an @A@ record, an @AAAA@ record, or both.
--
-- -   In the service that is specified by @ServiceId@, the value of
--     @RoutingPolicy@ must be @WEIGHTED@.
--
-- -   If the service that is specified by @ServiceId@ includes
--     @HealthCheckConfig@ settings, AWS Cloud Map will create the Route 53
--     health check, but it won\'t associate the health check with the
--     alias record.
--
-- -   Auto naming currently doesn\'t support creating alias records that
--     route traffic to AWS resources other than Elastic Load Balancing
--     load balancers.
--
-- -   If you specify a value for @AWS_ALIAS_DNS_NAME@, don\'t specify
--     values for any of the @AWS_INSTANCE@ attributes.
--
-- __AWS_EC2_INSTANCE_ID__
--
-- /HTTP namespaces only./ The Amazon EC2 instance ID for the instance. If
-- the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the only other
-- attribute that can be specified is @AWS_INIT_HEALTH_STATUS@. When the
-- @AWS_EC2_INSTANCE_ID@ attribute is specified, then the
-- @AWS_INSTANCE_IPV4@ attribute will be filled out with the primary
-- private IPv4 address.
--
-- __AWS_INIT_HEALTH_STATUS__
--
-- If the service configuration includes @HealthCheckCustomConfig@, you can
-- optionally use @AWS_INIT_HEALTH_STATUS@ to specify the initial status of
-- the custom health check, @HEALTHY@ or @UNHEALTHY@. If you don\'t specify
-- a value for @AWS_INIT_HEALTH_STATUS@, the initial status is @HEALTHY@.
--
-- __AWS_INSTANCE_CNAME__
--
-- If the service configuration includes a @CNAME@ record, the domain name
-- that you want Route 53 to return in response to DNS queries, for
-- example, @example.com@.
--
-- This value is required if the service specified by @ServiceId@ includes
-- settings for an @CNAME@ record.
--
-- __AWS_INSTANCE_IPV4__
--
-- If the service configuration includes an @A@ record, the IPv4 address
-- that you want Route 53 to return in response to DNS queries, for
-- example, @192.0.2.44@.
--
-- This value is required if the service specified by @ServiceId@ includes
-- settings for an @A@ record. If the service includes settings for an
-- @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@,
-- @AWS_INSTANCE_IPV6@, or both.
--
-- __AWS_INSTANCE_IPV6__
--
-- If the service configuration includes an @AAAA@ record, the IPv6 address
-- that you want Route 53 to return in response to DNS queries, for
-- example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@.
--
-- This value is required if the service specified by @ServiceId@ includes
-- settings for an @AAAA@ record. If the service includes settings for an
-- @SRV@ record, you must specify a value for @AWS_INSTANCE_IPV4@,
-- @AWS_INSTANCE_IPV6@, or both.
--
-- __AWS_INSTANCE_PORT__
--
-- If the service includes an @SRV@ record, the value that you want
-- Route 53 to return for the port.
--
-- If the service includes @HealthCheckConfig@, the port on the endpoint
-- that you want Route 53 to send requests to.
--
-- This value is required if you specified settings for an @SRV@ record or
-- a Route 53 health check when you created the service.
--
-- __Custom attributes__
--
-- You can add up to 30 custom attributes. For each key-value pair, the
-- maximum length of the attribute name is 255 characters, and the maximum
-- length of the attribute value is 1,024 characters. The total size of all
-- provided attributes (sum of all keys and values) must not exceed 5,000
-- characters.
registerInstance_attributes :: Lens.Lens' RegisterInstance (Prelude.HashMap Prelude.Text Prelude.Text)
registerInstance_attributes = Lens.lens (\RegisterInstance' {attributes} -> attributes) (\s@RegisterInstance' {} a -> s {attributes = a} :: RegisterInstance) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RegisterInstance where
  type Rs RegisterInstance = RegisterInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterInstanceResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterInstance

instance Prelude.NFData RegisterInstance

instance Prelude.ToHeaders RegisterInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.RegisterInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterInstance where
  toJSON RegisterInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Prelude..=)
              Prelude.<$> creatorRequestId,
            Prelude.Just ("ServiceId" Prelude..= serviceId),
            Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("Attributes" Prelude..= attributes)
          ]
      )

instance Prelude.ToPath RegisterInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterInstanceResponse' smart constructor.
data RegisterInstanceResponse = RegisterInstanceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'registerInstanceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'registerInstanceResponse_httpStatus' - The response's http status code.
newRegisterInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterInstanceResponse
newRegisterInstanceResponse pHttpStatus_ =
  RegisterInstanceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
registerInstanceResponse_operationId :: Lens.Lens' RegisterInstanceResponse (Prelude.Maybe Prelude.Text)
registerInstanceResponse_operationId = Lens.lens (\RegisterInstanceResponse' {operationId} -> operationId) (\s@RegisterInstanceResponse' {} a -> s {operationId = a} :: RegisterInstanceResponse)

-- | The response's http status code.
registerInstanceResponse_httpStatus :: Lens.Lens' RegisterInstanceResponse Prelude.Int
registerInstanceResponse_httpStatus = Lens.lens (\RegisterInstanceResponse' {httpStatus} -> httpStatus) (\s@RegisterInstanceResponse' {} a -> s {httpStatus = a} :: RegisterInstanceResponse)

instance Prelude.NFData RegisterInstanceResponse
