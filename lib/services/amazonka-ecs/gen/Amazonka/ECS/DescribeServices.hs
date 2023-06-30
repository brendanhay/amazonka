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
-- Module      : Amazonka.ECS.DescribeServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified services running in your cluster.
module Amazonka.ECS.DescribeServices
  ( -- * Creating a Request
    DescribeServices (..),
    newDescribeServices,

    -- * Request Lenses
    describeServices_cluster,
    describeServices_include,
    describeServices_services,

    -- * Destructuring the Response
    DescribeServicesResponse (..),
    newDescribeServicesResponse,

    -- * Response Lenses
    describeServicesResponse_failures,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServices' smart constructor.
data DescribeServices = DescribeServices'
  { -- | The short name or full Amazon Resource Name (ARN)the cluster that hosts
    -- the service to describe. If you do not specify a cluster, the default
    -- cluster is assumed. This parameter is required if the service or
    -- services you are describing were launched in any cluster other than the
    -- default cluster.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Determines whether you want to see the resource tags for the service. If
    -- @TAGS@ is specified, the tags are included in the response. If this
    -- field is omitted, tags aren\'t included in the response.
    include :: Prelude.Maybe [ServiceField],
    -- | A list of services to describe. You may specify up to 10 services to
    -- describe in a single operation.
    services :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'describeServices_cluster' - The short name or full Amazon Resource Name (ARN)the cluster that hosts
-- the service to describe. If you do not specify a cluster, the default
-- cluster is assumed. This parameter is required if the service or
-- services you are describing were launched in any cluster other than the
-- default cluster.
--
-- 'include', 'describeServices_include' - Determines whether you want to see the resource tags for the service. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags aren\'t included in the response.
--
-- 'services', 'describeServices_services' - A list of services to describe. You may specify up to 10 services to
-- describe in a single operation.
newDescribeServices ::
  DescribeServices
newDescribeServices =
  DescribeServices'
    { cluster = Prelude.Nothing,
      include = Prelude.Nothing,
      services = Prelude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN)the cluster that hosts
-- the service to describe. If you do not specify a cluster, the default
-- cluster is assumed. This parameter is required if the service or
-- services you are describing were launched in any cluster other than the
-- default cluster.
describeServices_cluster :: Lens.Lens' DescribeServices (Prelude.Maybe Prelude.Text)
describeServices_cluster = Lens.lens (\DescribeServices' {cluster} -> cluster) (\s@DescribeServices' {} a -> s {cluster = a} :: DescribeServices)

-- | Determines whether you want to see the resource tags for the service. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags aren\'t included in the response.
describeServices_include :: Lens.Lens' DescribeServices (Prelude.Maybe [ServiceField])
describeServices_include = Lens.lens (\DescribeServices' {include} -> include) (\s@DescribeServices' {} a -> s {include = a} :: DescribeServices) Prelude.. Lens.mapping Lens.coerced

-- | A list of services to describe. You may specify up to 10 services to
-- describe in a single operation.
describeServices_services :: Lens.Lens' DescribeServices [Prelude.Text]
describeServices_services = Lens.lens (\DescribeServices' {services} -> services) (\s@DescribeServices' {} a -> s {services = a} :: DescribeServices) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeServices where
  type
    AWSResponse DescribeServices =
      DescribeServicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServicesResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "services" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServices where
  hashWithSalt _salt DescribeServices' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` services

instance Prelude.NFData DescribeServices where
  rnf DescribeServices' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf include
      `Prelude.seq` Prelude.rnf services

instance Data.ToHeaders DescribeServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DescribeServices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeServices where
  toJSON DescribeServices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            ("include" Data..=) Prelude.<$> include,
            Prelude.Just ("services" Data..= services)
          ]
      )

instance Data.ToPath DescribeServices where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The list of services described.
    services :: Prelude.Maybe [ContainerService],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'describeServicesResponse_failures' - Any failures associated with the call.
--
-- 'services', 'describeServicesResponse_services' - The list of services described.
--
-- 'httpStatus', 'describeServicesResponse_httpStatus' - The response's http status code.
newDescribeServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServicesResponse
newDescribeServicesResponse pHttpStatus_ =
  DescribeServicesResponse'
    { failures =
        Prelude.Nothing,
      services = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
describeServicesResponse_failures :: Lens.Lens' DescribeServicesResponse (Prelude.Maybe [Failure])
describeServicesResponse_failures = Lens.lens (\DescribeServicesResponse' {failures} -> failures) (\s@DescribeServicesResponse' {} a -> s {failures = a} :: DescribeServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of services described.
describeServicesResponse_services :: Lens.Lens' DescribeServicesResponse (Prelude.Maybe [ContainerService])
describeServicesResponse_services = Lens.lens (\DescribeServicesResponse' {services} -> services) (\s@DescribeServicesResponse' {} a -> s {services = a} :: DescribeServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServicesResponse_httpStatus :: Lens.Lens' DescribeServicesResponse Prelude.Int
describeServicesResponse_httpStatus = Lens.lens (\DescribeServicesResponse' {httpStatus} -> httpStatus) (\s@DescribeServicesResponse' {} a -> s {httpStatus = a} :: DescribeServicesResponse)

instance Prelude.NFData DescribeServicesResponse where
  rnf DescribeServicesResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf httpStatus
