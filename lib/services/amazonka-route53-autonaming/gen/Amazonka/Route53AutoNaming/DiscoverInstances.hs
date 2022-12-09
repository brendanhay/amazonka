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
-- Module      : Amazonka.Route53AutoNaming.DiscoverInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Discovers registered instances for a specified namespace and service.
-- You can use @DiscoverInstances@ to discover instances for any type of
-- namespace. For public and private DNS namespaces, you can also use DNS
-- queries to discover instances.
module Amazonka.Route53AutoNaming.DiscoverInstances
  ( -- * Creating a Request
    DiscoverInstances (..),
    newDiscoverInstances,

    -- * Request Lenses
    discoverInstances_healthStatus,
    discoverInstances_maxResults,
    discoverInstances_optionalParameters,
    discoverInstances_queryParameters,
    discoverInstances_namespaceName,
    discoverInstances_serviceName,

    -- * Destructuring the Response
    DiscoverInstancesResponse (..),
    newDiscoverInstancesResponse,

    -- * Response Lenses
    discoverInstancesResponse_instances,
    discoverInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newDiscoverInstances' smart constructor.
data DiscoverInstances = DiscoverInstances'
  { -- | The health status of the instances that you want to discover. This
    -- parameter is ignored for services that don\'t have a health check
    -- configured, and all instances are returned.
    --
    -- [HEALTHY]
    --     Returns healthy instances.
    --
    -- [UNHEALTHY]
    --     Returns unhealthy instances.
    --
    -- [ALL]
    --     Returns all instances.
    --
    -- [HEALTHY_OR_ELSE_ALL]
    --     Returns healthy instances, unless none are reporting a healthy
    --     state. In that case, return all instances. This is also called
    --     failing open.
    healthStatus :: Prelude.Maybe HealthStatusFilter,
    -- | The maximum number of instances that you want Cloud Map to return in the
    -- response to a @DiscoverInstances@ request. If you don\'t specify a value
    -- for @MaxResults@, Cloud Map returns up to 100 instances.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Opportunistic filters to scope the results based on custom attributes.
    -- If there are instances that match both the filters specified in both the
    -- @QueryParameters@ parameter and this parameter, all of these instances
    -- are returned. Otherwise, the filters are ignored, and only instances
    -- that match the filters that are specified in the @QueryParameters@
    -- parameter are returned.
    optionalParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Filters to scope the results based on custom attributes for the instance
    -- (for example, @{version=v1, az=1a}@). Only instances that match all the
    -- specified key-value pairs are returned.
    queryParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The @HttpName@ name of the namespace. It\'s found in the
    -- @HttpProperties@ member of the @Properties@ member of the namespace.
    namespaceName :: Prelude.Text,
    -- | The name of the service that you specified when you registered the
    -- instance.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoverInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthStatus', 'discoverInstances_healthStatus' - The health status of the instances that you want to discover. This
-- parameter is ignored for services that don\'t have a health check
-- configured, and all instances are returned.
--
-- [HEALTHY]
--     Returns healthy instances.
--
-- [UNHEALTHY]
--     Returns unhealthy instances.
--
-- [ALL]
--     Returns all instances.
--
-- [HEALTHY_OR_ELSE_ALL]
--     Returns healthy instances, unless none are reporting a healthy
--     state. In that case, return all instances. This is also called
--     failing open.
--
-- 'maxResults', 'discoverInstances_maxResults' - The maximum number of instances that you want Cloud Map to return in the
-- response to a @DiscoverInstances@ request. If you don\'t specify a value
-- for @MaxResults@, Cloud Map returns up to 100 instances.
--
-- 'optionalParameters', 'discoverInstances_optionalParameters' - Opportunistic filters to scope the results based on custom attributes.
-- If there are instances that match both the filters specified in both the
-- @QueryParameters@ parameter and this parameter, all of these instances
-- are returned. Otherwise, the filters are ignored, and only instances
-- that match the filters that are specified in the @QueryParameters@
-- parameter are returned.
--
-- 'queryParameters', 'discoverInstances_queryParameters' - Filters to scope the results based on custom attributes for the instance
-- (for example, @{version=v1, az=1a}@). Only instances that match all the
-- specified key-value pairs are returned.
--
-- 'namespaceName', 'discoverInstances_namespaceName' - The @HttpName@ name of the namespace. It\'s found in the
-- @HttpProperties@ member of the @Properties@ member of the namespace.
--
-- 'serviceName', 'discoverInstances_serviceName' - The name of the service that you specified when you registered the
-- instance.
newDiscoverInstances ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  DiscoverInstances
newDiscoverInstances pNamespaceName_ pServiceName_ =
  DiscoverInstances'
    { healthStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      optionalParameters = Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      namespaceName = pNamespaceName_,
      serviceName = pServiceName_
    }

-- | The health status of the instances that you want to discover. This
-- parameter is ignored for services that don\'t have a health check
-- configured, and all instances are returned.
--
-- [HEALTHY]
--     Returns healthy instances.
--
-- [UNHEALTHY]
--     Returns unhealthy instances.
--
-- [ALL]
--     Returns all instances.
--
-- [HEALTHY_OR_ELSE_ALL]
--     Returns healthy instances, unless none are reporting a healthy
--     state. In that case, return all instances. This is also called
--     failing open.
discoverInstances_healthStatus :: Lens.Lens' DiscoverInstances (Prelude.Maybe HealthStatusFilter)
discoverInstances_healthStatus = Lens.lens (\DiscoverInstances' {healthStatus} -> healthStatus) (\s@DiscoverInstances' {} a -> s {healthStatus = a} :: DiscoverInstances)

-- | The maximum number of instances that you want Cloud Map to return in the
-- response to a @DiscoverInstances@ request. If you don\'t specify a value
-- for @MaxResults@, Cloud Map returns up to 100 instances.
discoverInstances_maxResults :: Lens.Lens' DiscoverInstances (Prelude.Maybe Prelude.Natural)
discoverInstances_maxResults = Lens.lens (\DiscoverInstances' {maxResults} -> maxResults) (\s@DiscoverInstances' {} a -> s {maxResults = a} :: DiscoverInstances)

-- | Opportunistic filters to scope the results based on custom attributes.
-- If there are instances that match both the filters specified in both the
-- @QueryParameters@ parameter and this parameter, all of these instances
-- are returned. Otherwise, the filters are ignored, and only instances
-- that match the filters that are specified in the @QueryParameters@
-- parameter are returned.
discoverInstances_optionalParameters :: Lens.Lens' DiscoverInstances (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
discoverInstances_optionalParameters = Lens.lens (\DiscoverInstances' {optionalParameters} -> optionalParameters) (\s@DiscoverInstances' {} a -> s {optionalParameters = a} :: DiscoverInstances) Prelude.. Lens.mapping Lens.coerced

-- | Filters to scope the results based on custom attributes for the instance
-- (for example, @{version=v1, az=1a}@). Only instances that match all the
-- specified key-value pairs are returned.
discoverInstances_queryParameters :: Lens.Lens' DiscoverInstances (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
discoverInstances_queryParameters = Lens.lens (\DiscoverInstances' {queryParameters} -> queryParameters) (\s@DiscoverInstances' {} a -> s {queryParameters = a} :: DiscoverInstances) Prelude.. Lens.mapping Lens.coerced

-- | The @HttpName@ name of the namespace. It\'s found in the
-- @HttpProperties@ member of the @Properties@ member of the namespace.
discoverInstances_namespaceName :: Lens.Lens' DiscoverInstances Prelude.Text
discoverInstances_namespaceName = Lens.lens (\DiscoverInstances' {namespaceName} -> namespaceName) (\s@DiscoverInstances' {} a -> s {namespaceName = a} :: DiscoverInstances)

-- | The name of the service that you specified when you registered the
-- instance.
discoverInstances_serviceName :: Lens.Lens' DiscoverInstances Prelude.Text
discoverInstances_serviceName = Lens.lens (\DiscoverInstances' {serviceName} -> serviceName) (\s@DiscoverInstances' {} a -> s {serviceName = a} :: DiscoverInstances)

instance Core.AWSRequest DiscoverInstances where
  type
    AWSResponse DiscoverInstances =
      DiscoverInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverInstancesResponse'
            Prelude.<$> (x Data..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DiscoverInstances where
  hashWithSalt _salt DiscoverInstances' {..} =
    _salt `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` optionalParameters
      `Prelude.hashWithSalt` queryParameters
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData DiscoverInstances where
  rnf DiscoverInstances' {..} =
    Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf optionalParameters
      `Prelude.seq` Prelude.rnf queryParameters
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders DiscoverInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.DiscoverInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DiscoverInstances where
  toJSON DiscoverInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HealthStatus" Data..=) Prelude.<$> healthStatus,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("OptionalParameters" Data..=)
              Prelude.<$> optionalParameters,
            ("QueryParameters" Data..=)
              Prelude.<$> queryParameters,
            Prelude.Just ("NamespaceName" Data..= namespaceName),
            Prelude.Just ("ServiceName" Data..= serviceName)
          ]
      )

instance Data.ToPath DiscoverInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DiscoverInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDiscoverInstancesResponse' smart constructor.
data DiscoverInstancesResponse = DiscoverInstancesResponse'
  { -- | A complex type that contains one @HttpInstanceSummary@ for each
    -- registered instance.
    instances :: Prelude.Maybe [HttpInstanceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoverInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'discoverInstancesResponse_instances' - A complex type that contains one @HttpInstanceSummary@ for each
-- registered instance.
--
-- 'httpStatus', 'discoverInstancesResponse_httpStatus' - The response's http status code.
newDiscoverInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DiscoverInstancesResponse
newDiscoverInstancesResponse pHttpStatus_ =
  DiscoverInstancesResponse'
    { instances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains one @HttpInstanceSummary@ for each
-- registered instance.
discoverInstancesResponse_instances :: Lens.Lens' DiscoverInstancesResponse (Prelude.Maybe [HttpInstanceSummary])
discoverInstancesResponse_instances = Lens.lens (\DiscoverInstancesResponse' {instances} -> instances) (\s@DiscoverInstancesResponse' {} a -> s {instances = a} :: DiscoverInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
discoverInstancesResponse_httpStatus :: Lens.Lens' DiscoverInstancesResponse Prelude.Int
discoverInstancesResponse_httpStatus = Lens.lens (\DiscoverInstancesResponse' {httpStatus} -> httpStatus) (\s@DiscoverInstancesResponse' {} a -> s {httpStatus = a} :: DiscoverInstancesResponse)

instance Prelude.NFData DiscoverInstancesResponse where
  rnf DiscoverInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf httpStatus
