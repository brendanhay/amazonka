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
-- Module      : Network.AWS.Route53AutoNaming.CreateService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service, which defines the configuration for the following
-- entities:
--
-- -   For public and private DNS namespaces, one of the following
--     combinations of DNS records in Amazon Route 53:
--
--     -   @A@
--
--     -   @AAAA@
--
--     -   @A@ and @AAAA@
--
--     -   @SRV@
--
--     -   @CNAME@
--
-- -   Optionally, a health check
--
-- After you create the service, you can submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance>
-- request, and AWS Cloud Map uses the values in the configuration to
-- create the specified entities.
--
-- For the current quota on the number of instances that you can register
-- using the same namespace and using the same service, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits>
-- in the /AWS Cloud Map Developer Guide/.
module Network.AWS.Route53AutoNaming.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_namespaceId,
    createService_dnsConfig,
    createService_creatorRequestId,
    createService_tags,
    createService_description,
    createService_healthCheckCustomConfig,
    createService_type,
    createService_healthCheckConfig,
    createService_name,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_service,
    createServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | The ID of the namespace that you want to use to create the service.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about the Amazon Route 53
    -- records that you want AWS Cloud Map to create when you register an
    -- instance.
    dnsConfig :: Prelude.Maybe DnsConfig,
    -- | A unique string that identifies the request and that allows failed
    -- @CreateService@ requests to be retried without the risk of executing the
    -- operation twice. @CreatorRequestId@ can be any unique string, for
    -- example, a date\/time stamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The tags to add to the service. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | A description for the service.
    description :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about an optional custom health
    -- check.
    --
    -- If you specify a health check configuration, you can specify either
    -- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
    --
    -- You can\'t add, update, or delete a @HealthCheckCustomConfig@
    -- configuration from an existing service.
    healthCheckCustomConfig :: Prelude.Maybe HealthCheckCustomConfig,
    -- | If present, specifies that the service instances are only discoverable
    -- using the @DiscoverInstances@ API operation. No DNS records will be
    -- registered for the service instances. The only valid value is @HTTP@.
    type' :: Prelude.Maybe ServiceTypeOption,
    -- | /Public DNS and HTTP namespaces only./ A complex type that contains
    -- settings for an optional Route 53 health check. If you specify settings
    -- for a health check, AWS Cloud Map associates the health check with all
    -- the Route 53 DNS records that you specify in @DnsConfig@.
    --
    -- If you specify a health check configuration, you can specify either
    -- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
    --
    -- For information about the charges for health checks, see
    -- <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing>.
    healthCheckConfig :: Prelude.Maybe HealthCheckConfig,
    -- | The name that you want to assign to the service.
    --
    -- If you want AWS Cloud Map to create an @SRV@ record when you register an
    -- instance, and if you\'re using a system that requires a specific @SRV@
    -- format, such as <http://www.haproxy.org/ HAProxy>, specify the following
    -- for @Name@:
    --
    -- -   Start the name with an underscore (_), such as @_exampleservice@
    --
    -- -   End the name with /._protocol/, such as @._tcp@
    --
    -- When you register an instance, AWS Cloud Map creates an @SRV@ record and
    -- assigns a name to the record by concatenating the service name and the
    -- namespace name, for example:
    --
    -- @_exampleservice._tcp.example.com@
    --
    -- For a single DNS namespace, you cannot create two services with names
    -- that differ only by case (such as EXAMPLE and example). Otherwise, these
    -- services will have the same DNS name. However, you can create multiple
    -- HTTP services with names that differ only by case because HTTP services
    -- are case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceId', 'createService_namespaceId' - The ID of the namespace that you want to use to create the service.
--
-- 'dnsConfig', 'createService_dnsConfig' - A complex type that contains information about the Amazon Route 53
-- records that you want AWS Cloud Map to create when you register an
-- instance.
--
-- 'creatorRequestId', 'createService_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @CreateService@ requests to be retried without the risk of executing the
-- operation twice. @CreatorRequestId@ can be any unique string, for
-- example, a date\/time stamp.
--
-- 'tags', 'createService_tags' - The tags to add to the service. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'description', 'createService_description' - A description for the service.
--
-- 'healthCheckCustomConfig', 'createService_healthCheckCustomConfig' - A complex type that contains information about an optional custom health
-- check.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- You can\'t add, update, or delete a @HealthCheckCustomConfig@
-- configuration from an existing service.
--
-- 'type'', 'createService_type' - If present, specifies that the service instances are only discoverable
-- using the @DiscoverInstances@ API operation. No DNS records will be
-- registered for the service instances. The only valid value is @HTTP@.
--
-- 'healthCheckConfig', 'createService_healthCheckConfig' - /Public DNS and HTTP namespaces only./ A complex type that contains
-- settings for an optional Route 53 health check. If you specify settings
-- for a health check, AWS Cloud Map associates the health check with all
-- the Route 53 DNS records that you specify in @DnsConfig@.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- For information about the charges for health checks, see
-- <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing>.
--
-- 'name', 'createService_name' - The name that you want to assign to the service.
--
-- If you want AWS Cloud Map to create an @SRV@ record when you register an
-- instance, and if you\'re using a system that requires a specific @SRV@
-- format, such as <http://www.haproxy.org/ HAProxy>, specify the following
-- for @Name@:
--
-- -   Start the name with an underscore (_), such as @_exampleservice@
--
-- -   End the name with /._protocol/, such as @._tcp@
--
-- When you register an instance, AWS Cloud Map creates an @SRV@ record and
-- assigns a name to the record by concatenating the service name and the
-- namespace name, for example:
--
-- @_exampleservice._tcp.example.com@
--
-- For a single DNS namespace, you cannot create two services with names
-- that differ only by case (such as EXAMPLE and example). Otherwise, these
-- services will have the same DNS name. However, you can create multiple
-- HTTP services with names that differ only by case because HTTP services
-- are case sensitive.
newCreateService ::
  -- | 'name'
  Prelude.Text ->
  CreateService
newCreateService pName_ =
  CreateService'
    { namespaceId = Prelude.Nothing,
      dnsConfig = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      healthCheckCustomConfig = Prelude.Nothing,
      type' = Prelude.Nothing,
      healthCheckConfig = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the namespace that you want to use to create the service.
createService_namespaceId :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_namespaceId = Lens.lens (\CreateService' {namespaceId} -> namespaceId) (\s@CreateService' {} a -> s {namespaceId = a} :: CreateService)

-- | A complex type that contains information about the Amazon Route 53
-- records that you want AWS Cloud Map to create when you register an
-- instance.
createService_dnsConfig :: Lens.Lens' CreateService (Prelude.Maybe DnsConfig)
createService_dnsConfig = Lens.lens (\CreateService' {dnsConfig} -> dnsConfig) (\s@CreateService' {} a -> s {dnsConfig = a} :: CreateService)

-- | A unique string that identifies the request and that allows failed
-- @CreateService@ requests to be retried without the risk of executing the
-- operation twice. @CreatorRequestId@ can be any unique string, for
-- example, a date\/time stamp.
createService_creatorRequestId :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_creatorRequestId = Lens.lens (\CreateService' {creatorRequestId} -> creatorRequestId) (\s@CreateService' {} a -> s {creatorRequestId = a} :: CreateService)

-- | The tags to add to the service. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens._Coerce

-- | A description for the service.
createService_description :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_description = Lens.lens (\CreateService' {description} -> description) (\s@CreateService' {} a -> s {description = a} :: CreateService)

-- | A complex type that contains information about an optional custom health
-- check.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- You can\'t add, update, or delete a @HealthCheckCustomConfig@
-- configuration from an existing service.
createService_healthCheckCustomConfig :: Lens.Lens' CreateService (Prelude.Maybe HealthCheckCustomConfig)
createService_healthCheckCustomConfig = Lens.lens (\CreateService' {healthCheckCustomConfig} -> healthCheckCustomConfig) (\s@CreateService' {} a -> s {healthCheckCustomConfig = a} :: CreateService)

-- | If present, specifies that the service instances are only discoverable
-- using the @DiscoverInstances@ API operation. No DNS records will be
-- registered for the service instances. The only valid value is @HTTP@.
createService_type :: Lens.Lens' CreateService (Prelude.Maybe ServiceTypeOption)
createService_type = Lens.lens (\CreateService' {type'} -> type') (\s@CreateService' {} a -> s {type' = a} :: CreateService)

-- | /Public DNS and HTTP namespaces only./ A complex type that contains
-- settings for an optional Route 53 health check. If you specify settings
-- for a health check, AWS Cloud Map associates the health check with all
-- the Route 53 DNS records that you specify in @DnsConfig@.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- For information about the charges for health checks, see
-- <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing>.
createService_healthCheckConfig :: Lens.Lens' CreateService (Prelude.Maybe HealthCheckConfig)
createService_healthCheckConfig = Lens.lens (\CreateService' {healthCheckConfig} -> healthCheckConfig) (\s@CreateService' {} a -> s {healthCheckConfig = a} :: CreateService)

-- | The name that you want to assign to the service.
--
-- If you want AWS Cloud Map to create an @SRV@ record when you register an
-- instance, and if you\'re using a system that requires a specific @SRV@
-- format, such as <http://www.haproxy.org/ HAProxy>, specify the following
-- for @Name@:
--
-- -   Start the name with an underscore (_), such as @_exampleservice@
--
-- -   End the name with /._protocol/, such as @._tcp@
--
-- When you register an instance, AWS Cloud Map creates an @SRV@ record and
-- assigns a name to the record by concatenating the service name and the
-- namespace name, for example:
--
-- @_exampleservice._tcp.example.com@
--
-- For a single DNS namespace, you cannot create two services with names
-- that differ only by case (such as EXAMPLE and example). Otherwise, these
-- services will have the same DNS name. However, you can create multiple
-- HTTP services with names that differ only by case because HTTP services
-- are case sensitive.
createService_name :: Lens.Lens' CreateService Prelude.Text
createService_name = Lens.lens (\CreateService' {name} -> name) (\s@CreateService' {} a -> s {name = a} :: CreateService)

instance Core.AWSRequest CreateService where
  type
    AWSResponse CreateService =
      CreateServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (x Core..?> "Service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateService

instance Prelude.NFData CreateService

instance Core.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.CreateService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateService where
  toJSON CreateService' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NamespaceId" Core..=) Prelude.<$> namespaceId,
            ("DnsConfig" Core..=) Prelude.<$> dnsConfig,
            ("CreatorRequestId" Core..=)
              Prelude.<$> creatorRequestId,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("HealthCheckCustomConfig" Core..=)
              Prelude.<$> healthCheckCustomConfig,
            ("Type" Core..=) Prelude.<$> type',
            ("HealthCheckConfig" Core..=)
              Prelude.<$> healthCheckConfig,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateService where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | A complex type that contains information about the new service.
    service :: Prelude.Maybe ServiceInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'createServiceResponse_service' - A complex type that contains information about the new service.
--
-- 'httpStatus', 'createServiceResponse_httpStatus' - The response's http status code.
newCreateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceResponse
newCreateServiceResponse pHttpStatus_ =
  CreateServiceResponse'
    { service = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about the new service.
createServiceResponse_service :: Lens.Lens' CreateServiceResponse (Prelude.Maybe ServiceInfo)
createServiceResponse_service = Lens.lens (\CreateServiceResponse' {service} -> service) (\s@CreateServiceResponse' {} a -> s {service = a} :: CreateServiceResponse)

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse
