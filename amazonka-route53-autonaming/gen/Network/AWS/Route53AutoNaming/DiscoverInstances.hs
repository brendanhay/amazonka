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
-- Module      : Network.AWS.Route53AutoNaming.DiscoverInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Discovers registered instances for a specified namespace and service.
-- You can use @DiscoverInstances@ to discover instances for any type of
-- namespace. For public and private DNS namespaces, you can also use DNS
-- queries to discover instances.
module Network.AWS.Route53AutoNaming.DiscoverInstances
  ( -- * Creating a Request
    DiscoverInstances (..),
    newDiscoverInstances,

    -- * Request Lenses
    discoverInstances_maxResults,
    discoverInstances_optionalParameters,
    discoverInstances_healthStatus,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newDiscoverInstances' smart constructor.
data DiscoverInstances = DiscoverInstances'
  { -- | The maximum number of instances that you want AWS Cloud Map to return in
    -- the response to a @DiscoverInstances@ request. If you don\'t specify a
    -- value for @MaxResults@, AWS Cloud Map returns up to 100 instances.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Opportunistic filters to scope the results based on custom attributes.
    -- If there are instances that match both the filters specified in both the
    -- @QueryParameters@ parameter and this parameter, they are returned.
    -- Otherwise, these filters are ignored and only instances that match the
    -- filters specified in the @QueryParameters@ parameter are returned.
    optionalParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The health status of the instances that you want to discover.
    healthStatus :: Prelude.Maybe HealthStatusFilter,
    -- | Filters to scope the results based on custom attributes for the
    -- instance. For example, @{version=v1, az=1a}@. Only instances that match
    -- all the specified key-value pairs will be returned.
    queryParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the namespace that you specified when you registered the
    -- instance.
    namespaceName :: Prelude.Text,
    -- | The name of the service that you specified when you registered the
    -- instance.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DiscoverInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'discoverInstances_maxResults' - The maximum number of instances that you want AWS Cloud Map to return in
-- the response to a @DiscoverInstances@ request. If you don\'t specify a
-- value for @MaxResults@, AWS Cloud Map returns up to 100 instances.
--
-- 'optionalParameters', 'discoverInstances_optionalParameters' - Opportunistic filters to scope the results based on custom attributes.
-- If there are instances that match both the filters specified in both the
-- @QueryParameters@ parameter and this parameter, they are returned.
-- Otherwise, these filters are ignored and only instances that match the
-- filters specified in the @QueryParameters@ parameter are returned.
--
-- 'healthStatus', 'discoverInstances_healthStatus' - The health status of the instances that you want to discover.
--
-- 'queryParameters', 'discoverInstances_queryParameters' - Filters to scope the results based on custom attributes for the
-- instance. For example, @{version=v1, az=1a}@. Only instances that match
-- all the specified key-value pairs will be returned.
--
-- 'namespaceName', 'discoverInstances_namespaceName' - The name of the namespace that you specified when you registered the
-- instance.
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
    { maxResults = Prelude.Nothing,
      optionalParameters = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      namespaceName = pNamespaceName_,
      serviceName = pServiceName_
    }

-- | The maximum number of instances that you want AWS Cloud Map to return in
-- the response to a @DiscoverInstances@ request. If you don\'t specify a
-- value for @MaxResults@, AWS Cloud Map returns up to 100 instances.
discoverInstances_maxResults :: Lens.Lens' DiscoverInstances (Prelude.Maybe Prelude.Natural)
discoverInstances_maxResults = Lens.lens (\DiscoverInstances' {maxResults} -> maxResults) (\s@DiscoverInstances' {} a -> s {maxResults = a} :: DiscoverInstances)

-- | Opportunistic filters to scope the results based on custom attributes.
-- If there are instances that match both the filters specified in both the
-- @QueryParameters@ parameter and this parameter, they are returned.
-- Otherwise, these filters are ignored and only instances that match the
-- filters specified in the @QueryParameters@ parameter are returned.
discoverInstances_optionalParameters :: Lens.Lens' DiscoverInstances (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
discoverInstances_optionalParameters = Lens.lens (\DiscoverInstances' {optionalParameters} -> optionalParameters) (\s@DiscoverInstances' {} a -> s {optionalParameters = a} :: DiscoverInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The health status of the instances that you want to discover.
discoverInstances_healthStatus :: Lens.Lens' DiscoverInstances (Prelude.Maybe HealthStatusFilter)
discoverInstances_healthStatus = Lens.lens (\DiscoverInstances' {healthStatus} -> healthStatus) (\s@DiscoverInstances' {} a -> s {healthStatus = a} :: DiscoverInstances)

-- | Filters to scope the results based on custom attributes for the
-- instance. For example, @{version=v1, az=1a}@. Only instances that match
-- all the specified key-value pairs will be returned.
discoverInstances_queryParameters :: Lens.Lens' DiscoverInstances (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
discoverInstances_queryParameters = Lens.lens (\DiscoverInstances' {queryParameters} -> queryParameters) (\s@DiscoverInstances' {} a -> s {queryParameters = a} :: DiscoverInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the namespace that you specified when you registered the
-- instance.
discoverInstances_namespaceName :: Lens.Lens' DiscoverInstances Prelude.Text
discoverInstances_namespaceName = Lens.lens (\DiscoverInstances' {namespaceName} -> namespaceName) (\s@DiscoverInstances' {} a -> s {namespaceName = a} :: DiscoverInstances)

-- | The name of the service that you specified when you registered the
-- instance.
discoverInstances_serviceName :: Lens.Lens' DiscoverInstances Prelude.Text
discoverInstances_serviceName = Lens.lens (\DiscoverInstances' {serviceName} -> serviceName) (\s@DiscoverInstances' {} a -> s {serviceName = a} :: DiscoverInstances)

instance Prelude.AWSRequest DiscoverInstances where
  type Rs DiscoverInstances = DiscoverInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverInstancesResponse'
            Prelude.<$> ( x Prelude..?> "Instances"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DiscoverInstances

instance Prelude.NFData DiscoverInstances

instance Prelude.ToHeaders DiscoverInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.DiscoverInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DiscoverInstances where
  toJSON DiscoverInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("OptionalParameters" Prelude..=)
              Prelude.<$> optionalParameters,
            ("HealthStatus" Prelude..=) Prelude.<$> healthStatus,
            ("QueryParameters" Prelude..=)
              Prelude.<$> queryParameters,
            Prelude.Just
              ("NamespaceName" Prelude..= namespaceName),
            Prelude.Just ("ServiceName" Prelude..= serviceName)
          ]
      )

instance Prelude.ToPath DiscoverInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DiscoverInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDiscoverInstancesResponse' smart constructor.
data DiscoverInstancesResponse = DiscoverInstancesResponse'
  { -- | A complex type that contains one @HttpInstanceSummary@ for each
    -- registered instance.
    instances :: Prelude.Maybe [HttpInstanceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
discoverInstancesResponse_instances = Lens.lens (\DiscoverInstancesResponse' {instances} -> instances) (\s@DiscoverInstancesResponse' {} a -> s {instances = a} :: DiscoverInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
discoverInstancesResponse_httpStatus :: Lens.Lens' DiscoverInstancesResponse Prelude.Int
discoverInstancesResponse_httpStatus = Lens.lens (\DiscoverInstancesResponse' {httpStatus} -> httpStatus) (\s@DiscoverInstancesResponse' {} a -> s {httpStatus = a} :: DiscoverInstancesResponse)

instance Prelude.NFData DiscoverInstancesResponse
