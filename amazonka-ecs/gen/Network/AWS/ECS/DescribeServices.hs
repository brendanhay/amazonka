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
-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified services running in your cluster.
module Network.AWS.ECS.DescribeServices
  ( -- * Creating a Request
    DescribeServices (..),
    newDescribeServices,

    -- * Request Lenses
    describeServices_include,
    describeServices_cluster,
    describeServices_services,

    -- * Destructuring the Response
    DescribeServicesResponse (..),
    newDescribeServicesResponse,

    -- * Response Lenses
    describeServicesResponse_services,
    describeServicesResponse_failures,
    describeServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | Specifies whether you want to see the resource tags for the service. If
    -- @TAGS@ is specified, the tags are included in the response. If this
    -- field is omitted, tags are not included in the response.
    include :: Core.Maybe [ServiceField],
    -- | The short name or full Amazon Resource Name (ARN)the cluster that hosts
    -- the service to describe. If you do not specify a cluster, the default
    -- cluster is assumed. This parameter is required if the service or
    -- services you are describing were launched in any cluster other than the
    -- default cluster.
    cluster :: Core.Maybe Core.Text,
    -- | A list of services to describe. You may specify up to 10 services to
    -- describe in a single operation.
    services :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeServices_include' - Specifies whether you want to see the resource tags for the service. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
--
-- 'cluster', 'describeServices_cluster' - The short name or full Amazon Resource Name (ARN)the cluster that hosts
-- the service to describe. If you do not specify a cluster, the default
-- cluster is assumed. This parameter is required if the service or
-- services you are describing were launched in any cluster other than the
-- default cluster.
--
-- 'services', 'describeServices_services' - A list of services to describe. You may specify up to 10 services to
-- describe in a single operation.
newDescribeServices ::
  DescribeServices
newDescribeServices =
  DescribeServices'
    { include = Core.Nothing,
      cluster = Core.Nothing,
      services = Core.mempty
    }

-- | Specifies whether you want to see the resource tags for the service. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
describeServices_include :: Lens.Lens' DescribeServices (Core.Maybe [ServiceField])
describeServices_include = Lens.lens (\DescribeServices' {include} -> include) (\s@DescribeServices' {} a -> s {include = a} :: DescribeServices) Core.. Lens.mapping Lens._Coerce

-- | The short name or full Amazon Resource Name (ARN)the cluster that hosts
-- the service to describe. If you do not specify a cluster, the default
-- cluster is assumed. This parameter is required if the service or
-- services you are describing were launched in any cluster other than the
-- default cluster.
describeServices_cluster :: Lens.Lens' DescribeServices (Core.Maybe Core.Text)
describeServices_cluster = Lens.lens (\DescribeServices' {cluster} -> cluster) (\s@DescribeServices' {} a -> s {cluster = a} :: DescribeServices)

-- | A list of services to describe. You may specify up to 10 services to
-- describe in a single operation.
describeServices_services :: Lens.Lens' DescribeServices [Core.Text]
describeServices_services = Lens.lens (\DescribeServices' {services} -> services) (\s@DescribeServices' {} a -> s {services = a} :: DescribeServices) Core.. Lens._Coerce

instance Core.AWSRequest DescribeServices where
  type
    AWSResponse DescribeServices =
      DescribeServicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Core.<$> (x Core..?> "services" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeServices

instance Core.NFData DescribeServices

instance Core.ToHeaders DescribeServices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeServices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("include" Core..=) Core.<$> include,
            ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("services" Core..= services)
          ]
      )

instance Core.ToPath DescribeServices where
  toPath = Core.const "/"

instance Core.ToQuery DescribeServices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | The list of services described.
    services :: Core.Maybe [ContainerService],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'describeServicesResponse_services' - The list of services described.
--
-- 'failures', 'describeServicesResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'describeServicesResponse_httpStatus' - The response's http status code.
newDescribeServicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeServicesResponse
newDescribeServicesResponse pHttpStatus_ =
  DescribeServicesResponse'
    { services = Core.Nothing,
      failures = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of services described.
describeServicesResponse_services :: Lens.Lens' DescribeServicesResponse (Core.Maybe [ContainerService])
describeServicesResponse_services = Lens.lens (\DescribeServicesResponse' {services} -> services) (\s@DescribeServicesResponse' {} a -> s {services = a} :: DescribeServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | Any failures associated with the call.
describeServicesResponse_failures :: Lens.Lens' DescribeServicesResponse (Core.Maybe [Failure])
describeServicesResponse_failures = Lens.lens (\DescribeServicesResponse' {failures} -> failures) (\s@DescribeServicesResponse' {} a -> s {failures = a} :: DescribeServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeServicesResponse_httpStatus :: Lens.Lens' DescribeServicesResponse Core.Int
describeServicesResponse_httpStatus = Lens.lens (\DescribeServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeServicesResponse' {} a -> s {httpStatus = a} :: DescribeServicesResponse)

instance Core.NFData DescribeServicesResponse
