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
-- Module      : Network.AWS.GuardDuty.UpdateFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the filter specified by the filter name.
module Network.AWS.GuardDuty.UpdateFilter
  ( -- * Creating a Request
    UpdateFilter (..),
    newUpdateFilter,

    -- * Request Lenses
    updateFilter_findingCriteria,
    updateFilter_action,
    updateFilter_description,
    updateFilter_rank,
    updateFilter_detectorId,
    updateFilter_filterName,

    -- * Destructuring the Response
    UpdateFilterResponse (..),
    newUpdateFilterResponse,

    -- * Response Lenses
    updateFilterResponse_httpStatus,
    updateFilterResponse_name,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFilter' smart constructor.
data UpdateFilter = UpdateFilter'
  { -- | Represents the criteria to be used in the filter for querying findings.
    findingCriteria :: Prelude.Maybe FindingCriteria,
    -- | Specifies the action that is to be applied to the findings that match
    -- the filter.
    action :: Prelude.Maybe FilterAction,
    -- | The description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the position of the filter in the list of current filters.
    -- Also specifies the order in which this filter is applied to the
    -- findings.
    rank :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID of the detector that specifies the GuardDuty service where
    -- you want to update a filter.
    detectorId :: Prelude.Text,
    -- | The name of the filter.
    filterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingCriteria', 'updateFilter_findingCriteria' - Represents the criteria to be used in the filter for querying findings.
--
-- 'action', 'updateFilter_action' - Specifies the action that is to be applied to the findings that match
-- the filter.
--
-- 'description', 'updateFilter_description' - The description of the filter.
--
-- 'rank', 'updateFilter_rank' - Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
--
-- 'detectorId', 'updateFilter_detectorId' - The unique ID of the detector that specifies the GuardDuty service where
-- you want to update a filter.
--
-- 'filterName', 'updateFilter_filterName' - The name of the filter.
newUpdateFilter ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'filterName'
  Prelude.Text ->
  UpdateFilter
newUpdateFilter pDetectorId_ pFilterName_ =
  UpdateFilter'
    { findingCriteria = Prelude.Nothing,
      action = Prelude.Nothing,
      description = Prelude.Nothing,
      rank = Prelude.Nothing,
      detectorId = pDetectorId_,
      filterName = pFilterName_
    }

-- | Represents the criteria to be used in the filter for querying findings.
updateFilter_findingCriteria :: Lens.Lens' UpdateFilter (Prelude.Maybe FindingCriteria)
updateFilter_findingCriteria = Lens.lens (\UpdateFilter' {findingCriteria} -> findingCriteria) (\s@UpdateFilter' {} a -> s {findingCriteria = a} :: UpdateFilter)

-- | Specifies the action that is to be applied to the findings that match
-- the filter.
updateFilter_action :: Lens.Lens' UpdateFilter (Prelude.Maybe FilterAction)
updateFilter_action = Lens.lens (\UpdateFilter' {action} -> action) (\s@UpdateFilter' {} a -> s {action = a} :: UpdateFilter)

-- | The description of the filter.
updateFilter_description :: Lens.Lens' UpdateFilter (Prelude.Maybe Prelude.Text)
updateFilter_description = Lens.lens (\UpdateFilter' {description} -> description) (\s@UpdateFilter' {} a -> s {description = a} :: UpdateFilter)

-- | Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
updateFilter_rank :: Lens.Lens' UpdateFilter (Prelude.Maybe Prelude.Natural)
updateFilter_rank = Lens.lens (\UpdateFilter' {rank} -> rank) (\s@UpdateFilter' {} a -> s {rank = a} :: UpdateFilter)

-- | The unique ID of the detector that specifies the GuardDuty service where
-- you want to update a filter.
updateFilter_detectorId :: Lens.Lens' UpdateFilter Prelude.Text
updateFilter_detectorId = Lens.lens (\UpdateFilter' {detectorId} -> detectorId) (\s@UpdateFilter' {} a -> s {detectorId = a} :: UpdateFilter)

-- | The name of the filter.
updateFilter_filterName :: Lens.Lens' UpdateFilter Prelude.Text
updateFilter_filterName = Lens.lens (\UpdateFilter' {filterName} -> filterName) (\s@UpdateFilter' {} a -> s {filterName = a} :: UpdateFilter)

instance Core.AWSRequest UpdateFilter where
  type AWSResponse UpdateFilter = UpdateFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "name")
      )

instance Prelude.Hashable UpdateFilter

instance Prelude.NFData UpdateFilter

instance Core.ToHeaders UpdateFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFilter where
  toJSON UpdateFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("findingCriteria" Core..=)
              Prelude.<$> findingCriteria,
            ("action" Core..=) Prelude.<$> action,
            ("description" Core..=) Prelude.<$> description,
            ("rank" Core..=) Prelude.<$> rank
          ]
      )

instance Core.ToPath UpdateFilter where
  toPath UpdateFilter' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/filter/",
        Core.toBS filterName
      ]

instance Core.ToQuery UpdateFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFilterResponse' smart constructor.
data UpdateFilterResponse = UpdateFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the filter.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFilterResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateFilterResponse_name' - The name of the filter.
newUpdateFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateFilterResponse
newUpdateFilterResponse pHttpStatus_ pName_ =
  UpdateFilterResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateFilterResponse_httpStatus :: Lens.Lens' UpdateFilterResponse Prelude.Int
updateFilterResponse_httpStatus = Lens.lens (\UpdateFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateFilterResponse' {} a -> s {httpStatus = a} :: UpdateFilterResponse)

-- | The name of the filter.
updateFilterResponse_name :: Lens.Lens' UpdateFilterResponse Prelude.Text
updateFilterResponse_name = Lens.lens (\UpdateFilterResponse' {name} -> name) (\s@UpdateFilterResponse' {} a -> s {name = a} :: UpdateFilterResponse)

instance Prelude.NFData UpdateFilterResponse
