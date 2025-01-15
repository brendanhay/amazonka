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
-- Module      : Amazonka.Route53AutoNaming.CreateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service. This action defines the configuration for the
-- following entities:
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
-- request, and Cloud Map uses the values in the configuration to create
-- the specified entities.
--
-- For the current quota on the number of instances that you can register
-- using the same namespace and using the same service, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html Cloud Map quotas>
-- in the /Cloud Map Developer Guide/.
module Amazonka.Route53AutoNaming.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_creatorRequestId,
    createService_description,
    createService_dnsConfig,
    createService_healthCheckConfig,
    createService_healthCheckCustomConfig,
    createService_namespaceId,
    createService_tags,
    createService_type,
    createService_name,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_service,
    createServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreateService@ requests to be retried without the risk of running the
    -- operation twice. @CreatorRequestId@ can be any unique string (for
    -- example, a date\/timestamp).
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | A description for the service.
    description :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about the Amazon Route 53
    -- records that you want Cloud Map to create when you register an instance.
    dnsConfig :: Prelude.Maybe DnsConfig,
    -- | /Public DNS and HTTP namespaces only./ A complex type that contains
    -- settings for an optional Route 53 health check. If you specify settings
    -- for a health check, Cloud Map associates the health check with all the
    -- Route 53 DNS records that you specify in @DnsConfig@.
    --
    -- If you specify a health check configuration, you can specify either
    -- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
    --
    -- For information about the charges for health checks, see
    -- <http://aws.amazon.com/cloud-map/pricing/ Cloud Map Pricing>.
    healthCheckConfig :: Prelude.Maybe HealthCheckConfig,
    -- | A complex type that contains information about an optional custom health
    -- check.
    --
    -- If you specify a health check configuration, you can specify either
    -- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
    --
    -- You can\'t add, update, or delete a @HealthCheckCustomConfig@
    -- configuration from an existing service.
    healthCheckCustomConfig :: Prelude.Maybe HealthCheckCustomConfig,
    -- | The ID of the namespace that you want to use to create the service. The
    -- namespace ID must be specified, but it can be specified either here or
    -- in the @DnsConfig@ object.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | The tags to add to the service. Each tag consists of a key and an
    -- optional value that you define. Tags keys can be up to 128 characters in
    -- length, and tag values can be up to 256 characters in length.
    tags :: Prelude.Maybe [Tag],
    -- | If present, specifies that the service instances are only discoverable
    -- using the @DiscoverInstances@ API operation. No DNS records is
    -- registered for the service instances. The only valid value is @HTTP@.
    type' :: Prelude.Maybe ServiceTypeOption,
    -- | The name that you want to assign to the service.
    --
    -- Do not include sensitive information in the name if the namespace is
    -- discoverable by public DNS queries.
    --
    -- If you want Cloud Map to create an @SRV@ record when you register an
    -- instance and you\'re using a system that requires a specific @SRV@
    -- format, such as <http://www.haproxy.org/ HAProxy>, specify the following
    -- for @Name@:
    --
    -- -   Start the name with an underscore (_), such as @_exampleservice@.
    --
    -- -   End the name with /._protocol/, such as @._tcp@.
    --
    -- When you register an instance, Cloud Map creates an @SRV@ record and
    -- assigns a name to the record by concatenating the service name and the
    -- namespace name (for example,
    --
    -- @_exampleservice._tcp.example.com@).
    --
    -- For services that are accessible by DNS queries, you can\'t create
    -- multiple services with names that differ only by case (such as EXAMPLE
    -- and example). Otherwise, these services have the same DNS name and
    -- can\'t be distinguished. However, if you use a namespace that\'s only
    -- accessible by API calls, then you can create services that with names
    -- that differ only by case.
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
-- 'creatorRequestId', 'createService_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @CreateService@ requests to be retried without the risk of running the
-- operation twice. @CreatorRequestId@ can be any unique string (for
-- example, a date\/timestamp).
--
-- 'description', 'createService_description' - A description for the service.
--
-- 'dnsConfig', 'createService_dnsConfig' - A complex type that contains information about the Amazon Route 53
-- records that you want Cloud Map to create when you register an instance.
--
-- 'healthCheckConfig', 'createService_healthCheckConfig' - /Public DNS and HTTP namespaces only./ A complex type that contains
-- settings for an optional Route 53 health check. If you specify settings
-- for a health check, Cloud Map associates the health check with all the
-- Route 53 DNS records that you specify in @DnsConfig@.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- For information about the charges for health checks, see
-- <http://aws.amazon.com/cloud-map/pricing/ Cloud Map Pricing>.
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
-- 'namespaceId', 'createService_namespaceId' - The ID of the namespace that you want to use to create the service. The
-- namespace ID must be specified, but it can be specified either here or
-- in the @DnsConfig@ object.
--
-- 'tags', 'createService_tags' - The tags to add to the service. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
--
-- 'type'', 'createService_type' - If present, specifies that the service instances are only discoverable
-- using the @DiscoverInstances@ API operation. No DNS records is
-- registered for the service instances. The only valid value is @HTTP@.
--
-- 'name', 'createService_name' - The name that you want to assign to the service.
--
-- Do not include sensitive information in the name if the namespace is
-- discoverable by public DNS queries.
--
-- If you want Cloud Map to create an @SRV@ record when you register an
-- instance and you\'re using a system that requires a specific @SRV@
-- format, such as <http://www.haproxy.org/ HAProxy>, specify the following
-- for @Name@:
--
-- -   Start the name with an underscore (_), such as @_exampleservice@.
--
-- -   End the name with /._protocol/, such as @._tcp@.
--
-- When you register an instance, Cloud Map creates an @SRV@ record and
-- assigns a name to the record by concatenating the service name and the
-- namespace name (for example,
--
-- @_exampleservice._tcp.example.com@).
--
-- For services that are accessible by DNS queries, you can\'t create
-- multiple services with names that differ only by case (such as EXAMPLE
-- and example). Otherwise, these services have the same DNS name and
-- can\'t be distinguished. However, if you use a namespace that\'s only
-- accessible by API calls, then you can create services that with names
-- that differ only by case.
newCreateService ::
  -- | 'name'
  Prelude.Text ->
  CreateService
newCreateService pName_ =
  CreateService'
    { creatorRequestId = Prelude.Nothing,
      description = Prelude.Nothing,
      dnsConfig = Prelude.Nothing,
      healthCheckConfig = Prelude.Nothing,
      healthCheckCustomConfig = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreateService@ requests to be retried without the risk of running the
-- operation twice. @CreatorRequestId@ can be any unique string (for
-- example, a date\/timestamp).
createService_creatorRequestId :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_creatorRequestId = Lens.lens (\CreateService' {creatorRequestId} -> creatorRequestId) (\s@CreateService' {} a -> s {creatorRequestId = a} :: CreateService)

-- | A description for the service.
createService_description :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_description = Lens.lens (\CreateService' {description} -> description) (\s@CreateService' {} a -> s {description = a} :: CreateService)

-- | A complex type that contains information about the Amazon Route 53
-- records that you want Cloud Map to create when you register an instance.
createService_dnsConfig :: Lens.Lens' CreateService (Prelude.Maybe DnsConfig)
createService_dnsConfig = Lens.lens (\CreateService' {dnsConfig} -> dnsConfig) (\s@CreateService' {} a -> s {dnsConfig = a} :: CreateService)

-- | /Public DNS and HTTP namespaces only./ A complex type that contains
-- settings for an optional Route 53 health check. If you specify settings
-- for a health check, Cloud Map associates the health check with all the
-- Route 53 DNS records that you specify in @DnsConfig@.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- For information about the charges for health checks, see
-- <http://aws.amazon.com/cloud-map/pricing/ Cloud Map Pricing>.
createService_healthCheckConfig :: Lens.Lens' CreateService (Prelude.Maybe HealthCheckConfig)
createService_healthCheckConfig = Lens.lens (\CreateService' {healthCheckConfig} -> healthCheckConfig) (\s@CreateService' {} a -> s {healthCheckConfig = a} :: CreateService)

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

-- | The ID of the namespace that you want to use to create the service. The
-- namespace ID must be specified, but it can be specified either here or
-- in the @DnsConfig@ object.
createService_namespaceId :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_namespaceId = Lens.lens (\CreateService' {namespaceId} -> namespaceId) (\s@CreateService' {} a -> s {namespaceId = a} :: CreateService)

-- | The tags to add to the service. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | If present, specifies that the service instances are only discoverable
-- using the @DiscoverInstances@ API operation. No DNS records is
-- registered for the service instances. The only valid value is @HTTP@.
createService_type :: Lens.Lens' CreateService (Prelude.Maybe ServiceTypeOption)
createService_type = Lens.lens (\CreateService' {type'} -> type') (\s@CreateService' {} a -> s {type' = a} :: CreateService)

-- | The name that you want to assign to the service.
--
-- Do not include sensitive information in the name if the namespace is
-- discoverable by public DNS queries.
--
-- If you want Cloud Map to create an @SRV@ record when you register an
-- instance and you\'re using a system that requires a specific @SRV@
-- format, such as <http://www.haproxy.org/ HAProxy>, specify the following
-- for @Name@:
--
-- -   Start the name with an underscore (_), such as @_exampleservice@.
--
-- -   End the name with /._protocol/, such as @._tcp@.
--
-- When you register an instance, Cloud Map creates an @SRV@ record and
-- assigns a name to the record by concatenating the service name and the
-- namespace name (for example,
--
-- @_exampleservice._tcp.example.com@).
--
-- For services that are accessible by DNS queries, you can\'t create
-- multiple services with names that differ only by case (such as EXAMPLE
-- and example). Otherwise, these services have the same DNS name and
-- can\'t be distinguished. However, if you use a namespace that\'s only
-- accessible by API calls, then you can create services that with names
-- that differ only by case.
createService_name :: Lens.Lens' CreateService Prelude.Text
createService_name = Lens.lens (\CreateService' {name} -> name) (\s@CreateService' {} a -> s {name = a} :: CreateService)

instance Core.AWSRequest CreateService where
  type
    AWSResponse CreateService =
      CreateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (x Data..?> "Service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateService where
  hashWithSalt _salt CreateService' {..} =
    _salt
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dnsConfig
      `Prelude.hashWithSalt` healthCheckConfig
      `Prelude.hashWithSalt` healthCheckCustomConfig
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateService where
  rnf CreateService' {..} =
    Prelude.rnf creatorRequestId `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf dnsConfig `Prelude.seq`
          Prelude.rnf healthCheckConfig `Prelude.seq`
            Prelude.rnf healthCheckCustomConfig `Prelude.seq`
              Prelude.rnf namespaceId `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf type' `Prelude.seq`
                    Prelude.rnf name

instance Data.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.CreateService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateService where
  toJSON CreateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            ("Description" Data..=) Prelude.<$> description,
            ("DnsConfig" Data..=) Prelude.<$> dnsConfig,
            ("HealthCheckConfig" Data..=)
              Prelude.<$> healthCheckConfig,
            ("HealthCheckCustomConfig" Data..=)
              Prelude.<$> healthCheckCustomConfig,
            ("NamespaceId" Data..=) Prelude.<$> namespaceId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateService where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateService where
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

instance Prelude.NFData CreateServiceResponse where
  rnf CreateServiceResponse' {..} =
    Prelude.rnf service `Prelude.seq`
      Prelude.rnf httpStatus
