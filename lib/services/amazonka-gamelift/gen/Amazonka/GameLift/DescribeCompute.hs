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
-- Module      : Amazonka.GameLift.DescribeCompute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a compute resource. To request a compute
-- resource specify the fleet ID and compute name. If successful, Amazon
-- GameLift returns an object containing the build properties.
module Amazonka.GameLift.DescribeCompute
  ( -- * Creating a Request
    DescribeCompute (..),
    newDescribeCompute,

    -- * Request Lenses
    describeCompute_fleetId,
    describeCompute_computeName,

    -- * Destructuring the Response
    DescribeComputeResponse (..),
    newDescribeComputeResponse,

    -- * Response Lenses
    describeComputeResponse_compute,
    describeComputeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCompute' smart constructor.
data DescribeCompute = DescribeCompute'
  { -- | A unique identifier for the fleet the compute is registered to.
    fleetId :: Prelude.Text,
    -- | A descriptive label that is associated with the compute resource
    -- registered to your fleet.
    computeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCompute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeCompute_fleetId' - A unique identifier for the fleet the compute is registered to.
--
-- 'computeName', 'describeCompute_computeName' - A descriptive label that is associated with the compute resource
-- registered to your fleet.
newDescribeCompute ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'computeName'
  Prelude.Text ->
  DescribeCompute
newDescribeCompute pFleetId_ pComputeName_ =
  DescribeCompute'
    { fleetId = pFleetId_,
      computeName = pComputeName_
    }

-- | A unique identifier for the fleet the compute is registered to.
describeCompute_fleetId :: Lens.Lens' DescribeCompute Prelude.Text
describeCompute_fleetId = Lens.lens (\DescribeCompute' {fleetId} -> fleetId) (\s@DescribeCompute' {} a -> s {fleetId = a} :: DescribeCompute)

-- | A descriptive label that is associated with the compute resource
-- registered to your fleet.
describeCompute_computeName :: Lens.Lens' DescribeCompute Prelude.Text
describeCompute_computeName = Lens.lens (\DescribeCompute' {computeName} -> computeName) (\s@DescribeCompute' {} a -> s {computeName = a} :: DescribeCompute)

instance Core.AWSRequest DescribeCompute where
  type
    AWSResponse DescribeCompute =
      DescribeComputeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComputeResponse'
            Prelude.<$> (x Data..?> "Compute")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCompute where
  hashWithSalt _salt DescribeCompute' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` computeName

instance Prelude.NFData DescribeCompute where
  rnf DescribeCompute' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf computeName

instance Data.ToHeaders DescribeCompute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DescribeCompute" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCompute where
  toJSON DescribeCompute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("ComputeName" Data..= computeName)
          ]
      )

instance Data.ToPath DescribeCompute where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCompute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeComputeResponse' smart constructor.
data DescribeComputeResponse = DescribeComputeResponse'
  { -- | The details of the compute resource you registered to the specified
    -- fleet.
    compute :: Prelude.Maybe Compute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComputeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compute', 'describeComputeResponse_compute' - The details of the compute resource you registered to the specified
-- fleet.
--
-- 'httpStatus', 'describeComputeResponse_httpStatus' - The response's http status code.
newDescribeComputeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComputeResponse
newDescribeComputeResponse pHttpStatus_ =
  DescribeComputeResponse'
    { compute = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the compute resource you registered to the specified
-- fleet.
describeComputeResponse_compute :: Lens.Lens' DescribeComputeResponse (Prelude.Maybe Compute)
describeComputeResponse_compute = Lens.lens (\DescribeComputeResponse' {compute} -> compute) (\s@DescribeComputeResponse' {} a -> s {compute = a} :: DescribeComputeResponse)

-- | The response's http status code.
describeComputeResponse_httpStatus :: Lens.Lens' DescribeComputeResponse Prelude.Int
describeComputeResponse_httpStatus = Lens.lens (\DescribeComputeResponse' {httpStatus} -> httpStatus) (\s@DescribeComputeResponse' {} a -> s {httpStatus = a} :: DescribeComputeResponse)

instance Prelude.NFData DescribeComputeResponse where
  rnf DescribeComputeResponse' {..} =
    Prelude.rnf compute
      `Prelude.seq` Prelude.rnf httpStatus
