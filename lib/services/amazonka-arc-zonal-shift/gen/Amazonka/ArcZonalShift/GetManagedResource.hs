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
-- Module      : Amazonka.ArcZonalShift.GetManagedResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a resource that\'s been registered for zonal
-- shifts with Amazon Route 53 Application Recovery Controller in this AWS
-- Region. Resources that are registered for zonal shifts are managed
-- resources in Route 53 ARC.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
module Amazonka.ArcZonalShift.GetManagedResource
  ( -- * Creating a Request
    GetManagedResource (..),
    newGetManagedResource,

    -- * Request Lenses
    getManagedResource_resourceIdentifier,

    -- * Destructuring the Response
    GetManagedResourceResponse (..),
    newGetManagedResourceResponse,

    -- * Response Lenses
    getManagedResourceResponse_arn,
    getManagedResourceResponse_name,
    getManagedResourceResponse_httpStatus,
    getManagedResourceResponse_appliedWeights,
    getManagedResourceResponse_zonalShifts,
  )
where

import Amazonka.ArcZonalShift.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetManagedResource' smart constructor.
data GetManagedResource = GetManagedResource'
  { -- | The identifier for the resource to include in a zonal shift. The
    -- identifier is the Amazon Resource Name (ARN) for the resource.
    --
    -- At this time, you can only start a zonal shift for Network Load
    -- Balancers and Application Load Balancers with cross-zone load balancing
    -- turned off.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'getManagedResource_resourceIdentifier' - The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
newGetManagedResource ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  GetManagedResource
newGetManagedResource pResourceIdentifier_ =
  GetManagedResource'
    { resourceIdentifier =
        pResourceIdentifier_
    }

-- | The identifier for the resource to include in a zonal shift. The
-- identifier is the Amazon Resource Name (ARN) for the resource.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
getManagedResource_resourceIdentifier :: Lens.Lens' GetManagedResource Prelude.Text
getManagedResource_resourceIdentifier = Lens.lens (\GetManagedResource' {resourceIdentifier} -> resourceIdentifier) (\s@GetManagedResource' {} a -> s {resourceIdentifier = a} :: GetManagedResource)

instance Core.AWSRequest GetManagedResource where
  type
    AWSResponse GetManagedResource =
      GetManagedResourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetManagedResourceResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "appliedWeights" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "zonalShifts" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetManagedResource where
  hashWithSalt _salt GetManagedResource' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData GetManagedResource where
  rnf GetManagedResource' {..} =
    Prelude.rnf resourceIdentifier

instance Data.ToHeaders GetManagedResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetManagedResource where
  toPath GetManagedResource' {..} =
    Prelude.mconcat
      ["/managedresources/", Data.toBS resourceIdentifier]

instance Data.ToQuery GetManagedResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetManagedResourceResponse' smart constructor.
data GetManagedResourceResponse = GetManagedResourceResponse'
  { -- | The Amazon Resource Name (ARN) for the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A collection of key-value pairs that indicate whether resources are
    -- active in Availability Zones or not. The key name is the Availability
    -- Zone where the resource is deployed. The value is 1 or 0.
    appliedWeights :: Prelude.HashMap Prelude.Text Prelude.Double,
    -- | The zonal shifts that are currently active for a resource.
    zonalShifts :: [ZonalShiftInResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getManagedResourceResponse_arn' - The Amazon Resource Name (ARN) for the resource.
--
-- 'name', 'getManagedResourceResponse_name' - The name of the resource.
--
-- 'httpStatus', 'getManagedResourceResponse_httpStatus' - The response's http status code.
--
-- 'appliedWeights', 'getManagedResourceResponse_appliedWeights' - A collection of key-value pairs that indicate whether resources are
-- active in Availability Zones or not. The key name is the Availability
-- Zone where the resource is deployed. The value is 1 or 0.
--
-- 'zonalShifts', 'getManagedResourceResponse_zonalShifts' - The zonal shifts that are currently active for a resource.
newGetManagedResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetManagedResourceResponse
newGetManagedResourceResponse pHttpStatus_ =
  GetManagedResourceResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      appliedWeights = Prelude.mempty,
      zonalShifts = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) for the resource.
getManagedResourceResponse_arn :: Lens.Lens' GetManagedResourceResponse (Prelude.Maybe Prelude.Text)
getManagedResourceResponse_arn = Lens.lens (\GetManagedResourceResponse' {arn} -> arn) (\s@GetManagedResourceResponse' {} a -> s {arn = a} :: GetManagedResourceResponse)

-- | The name of the resource.
getManagedResourceResponse_name :: Lens.Lens' GetManagedResourceResponse (Prelude.Maybe Prelude.Text)
getManagedResourceResponse_name = Lens.lens (\GetManagedResourceResponse' {name} -> name) (\s@GetManagedResourceResponse' {} a -> s {name = a} :: GetManagedResourceResponse)

-- | The response's http status code.
getManagedResourceResponse_httpStatus :: Lens.Lens' GetManagedResourceResponse Prelude.Int
getManagedResourceResponse_httpStatus = Lens.lens (\GetManagedResourceResponse' {httpStatus} -> httpStatus) (\s@GetManagedResourceResponse' {} a -> s {httpStatus = a} :: GetManagedResourceResponse)

-- | A collection of key-value pairs that indicate whether resources are
-- active in Availability Zones or not. The key name is the Availability
-- Zone where the resource is deployed. The value is 1 or 0.
getManagedResourceResponse_appliedWeights :: Lens.Lens' GetManagedResourceResponse (Prelude.HashMap Prelude.Text Prelude.Double)
getManagedResourceResponse_appliedWeights = Lens.lens (\GetManagedResourceResponse' {appliedWeights} -> appliedWeights) (\s@GetManagedResourceResponse' {} a -> s {appliedWeights = a} :: GetManagedResourceResponse) Prelude.. Lens.coerced

-- | The zonal shifts that are currently active for a resource.
getManagedResourceResponse_zonalShifts :: Lens.Lens' GetManagedResourceResponse [ZonalShiftInResource]
getManagedResourceResponse_zonalShifts = Lens.lens (\GetManagedResourceResponse' {zonalShifts} -> zonalShifts) (\s@GetManagedResourceResponse' {} a -> s {zonalShifts = a} :: GetManagedResourceResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetManagedResourceResponse where
  rnf GetManagedResourceResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf appliedWeights `Prelude.seq`
            Prelude.rnf zonalShifts
