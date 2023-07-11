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
-- Module      : Amazonka.CostExplorer.UpdateCostAllocationTagsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates status for cost allocation tags in bulk, with maximum batch size
-- of 20. If the tag status that\'s updated is the same as the existing tag
-- status, the request doesn\'t fail. Instead, it doesn\'t have any effect
-- on the tag status (for example, activating the active tag).
module Amazonka.CostExplorer.UpdateCostAllocationTagsStatus
  ( -- * Creating a Request
    UpdateCostAllocationTagsStatus (..),
    newUpdateCostAllocationTagsStatus,

    -- * Request Lenses
    updateCostAllocationTagsStatus_costAllocationTagsStatus,

    -- * Destructuring the Response
    UpdateCostAllocationTagsStatusResponse (..),
    newUpdateCostAllocationTagsStatusResponse,

    -- * Response Lenses
    updateCostAllocationTagsStatusResponse_errors,
    updateCostAllocationTagsStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCostAllocationTagsStatus' smart constructor.
data UpdateCostAllocationTagsStatus = UpdateCostAllocationTagsStatus'
  { -- | The list of @CostAllocationTagStatusEntry@ objects that are used to
    -- update cost allocation tags status for this request.
    costAllocationTagsStatus :: Prelude.NonEmpty CostAllocationTagStatusEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCostAllocationTagsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costAllocationTagsStatus', 'updateCostAllocationTagsStatus_costAllocationTagsStatus' - The list of @CostAllocationTagStatusEntry@ objects that are used to
-- update cost allocation tags status for this request.
newUpdateCostAllocationTagsStatus ::
  -- | 'costAllocationTagsStatus'
  Prelude.NonEmpty CostAllocationTagStatusEntry ->
  UpdateCostAllocationTagsStatus
newUpdateCostAllocationTagsStatus
  pCostAllocationTagsStatus_ =
    UpdateCostAllocationTagsStatus'
      { costAllocationTagsStatus =
          Lens.coerced
            Lens.# pCostAllocationTagsStatus_
      }

-- | The list of @CostAllocationTagStatusEntry@ objects that are used to
-- update cost allocation tags status for this request.
updateCostAllocationTagsStatus_costAllocationTagsStatus :: Lens.Lens' UpdateCostAllocationTagsStatus (Prelude.NonEmpty CostAllocationTagStatusEntry)
updateCostAllocationTagsStatus_costAllocationTagsStatus = Lens.lens (\UpdateCostAllocationTagsStatus' {costAllocationTagsStatus} -> costAllocationTagsStatus) (\s@UpdateCostAllocationTagsStatus' {} a -> s {costAllocationTagsStatus = a} :: UpdateCostAllocationTagsStatus) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateCostAllocationTagsStatus
  where
  type
    AWSResponse UpdateCostAllocationTagsStatus =
      UpdateCostAllocationTagsStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCostAllocationTagsStatusResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCostAllocationTagsStatus
  where
  hashWithSalt
    _salt
    UpdateCostAllocationTagsStatus' {..} =
      _salt
        `Prelude.hashWithSalt` costAllocationTagsStatus

instance
  Prelude.NFData
    UpdateCostAllocationTagsStatus
  where
  rnf UpdateCostAllocationTagsStatus' {..} =
    Prelude.rnf costAllocationTagsStatus

instance
  Data.ToHeaders
    UpdateCostAllocationTagsStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.UpdateCostAllocationTagsStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCostAllocationTagsStatus where
  toJSON UpdateCostAllocationTagsStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CostAllocationTagsStatus"
                  Data..= costAllocationTagsStatus
              )
          ]
      )

instance Data.ToPath UpdateCostAllocationTagsStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCostAllocationTagsStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCostAllocationTagsStatusResponse' smart constructor.
data UpdateCostAllocationTagsStatusResponse = UpdateCostAllocationTagsStatusResponse'
  { -- | A list of @UpdateCostAllocationTagsStatusError@ objects with error
    -- details about each cost allocation tag that can\'t be updated. If
    -- there\'s no failure, an empty array returns.
    errors :: Prelude.Maybe [UpdateCostAllocationTagsStatusError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCostAllocationTagsStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'updateCostAllocationTagsStatusResponse_errors' - A list of @UpdateCostAllocationTagsStatusError@ objects with error
-- details about each cost allocation tag that can\'t be updated. If
-- there\'s no failure, an empty array returns.
--
-- 'httpStatus', 'updateCostAllocationTagsStatusResponse_httpStatus' - The response's http status code.
newUpdateCostAllocationTagsStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCostAllocationTagsStatusResponse
newUpdateCostAllocationTagsStatusResponse
  pHttpStatus_ =
    UpdateCostAllocationTagsStatusResponse'
      { errors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of @UpdateCostAllocationTagsStatusError@ objects with error
-- details about each cost allocation tag that can\'t be updated. If
-- there\'s no failure, an empty array returns.
updateCostAllocationTagsStatusResponse_errors :: Lens.Lens' UpdateCostAllocationTagsStatusResponse (Prelude.Maybe [UpdateCostAllocationTagsStatusError])
updateCostAllocationTagsStatusResponse_errors = Lens.lens (\UpdateCostAllocationTagsStatusResponse' {errors} -> errors) (\s@UpdateCostAllocationTagsStatusResponse' {} a -> s {errors = a} :: UpdateCostAllocationTagsStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateCostAllocationTagsStatusResponse_httpStatus :: Lens.Lens' UpdateCostAllocationTagsStatusResponse Prelude.Int
updateCostAllocationTagsStatusResponse_httpStatus = Lens.lens (\UpdateCostAllocationTagsStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateCostAllocationTagsStatusResponse' {} a -> s {httpStatus = a} :: UpdateCostAllocationTagsStatusResponse)

instance
  Prelude.NFData
    UpdateCostAllocationTagsStatusResponse
  where
  rnf UpdateCostAllocationTagsStatusResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
