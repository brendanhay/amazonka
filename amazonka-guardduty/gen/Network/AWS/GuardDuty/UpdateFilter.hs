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
    updateFilter_rank,
    updateFilter_findingCriteria,
    updateFilter_action,
    updateFilter_description,
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFilter' smart constructor.
data UpdateFilter = UpdateFilter'
  { -- | Specifies the position of the filter in the list of current filters.
    -- Also specifies the order in which this filter is applied to the
    -- findings.
    rank :: Core.Maybe Core.Natural,
    -- | Represents the criteria to be used in the filter for querying findings.
    findingCriteria :: Core.Maybe FindingCriteria,
    -- | Specifies the action that is to be applied to the findings that match
    -- the filter.
    action :: Core.Maybe FilterAction,
    -- | The description of the filter.
    description :: Core.Maybe Core.Text,
    -- | The unique ID of the detector that specifies the GuardDuty service where
    -- you want to update a filter.
    detectorId :: Core.Text,
    -- | The name of the filter.
    filterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rank', 'updateFilter_rank' - Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
--
-- 'findingCriteria', 'updateFilter_findingCriteria' - Represents the criteria to be used in the filter for querying findings.
--
-- 'action', 'updateFilter_action' - Specifies the action that is to be applied to the findings that match
-- the filter.
--
-- 'description', 'updateFilter_description' - The description of the filter.
--
-- 'detectorId', 'updateFilter_detectorId' - The unique ID of the detector that specifies the GuardDuty service where
-- you want to update a filter.
--
-- 'filterName', 'updateFilter_filterName' - The name of the filter.
newUpdateFilter ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'filterName'
  Core.Text ->
  UpdateFilter
newUpdateFilter pDetectorId_ pFilterName_ =
  UpdateFilter'
    { rank = Core.Nothing,
      findingCriteria = Core.Nothing,
      action = Core.Nothing,
      description = Core.Nothing,
      detectorId = pDetectorId_,
      filterName = pFilterName_
    }

-- | Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
updateFilter_rank :: Lens.Lens' UpdateFilter (Core.Maybe Core.Natural)
updateFilter_rank = Lens.lens (\UpdateFilter' {rank} -> rank) (\s@UpdateFilter' {} a -> s {rank = a} :: UpdateFilter)

-- | Represents the criteria to be used in the filter for querying findings.
updateFilter_findingCriteria :: Lens.Lens' UpdateFilter (Core.Maybe FindingCriteria)
updateFilter_findingCriteria = Lens.lens (\UpdateFilter' {findingCriteria} -> findingCriteria) (\s@UpdateFilter' {} a -> s {findingCriteria = a} :: UpdateFilter)

-- | Specifies the action that is to be applied to the findings that match
-- the filter.
updateFilter_action :: Lens.Lens' UpdateFilter (Core.Maybe FilterAction)
updateFilter_action = Lens.lens (\UpdateFilter' {action} -> action) (\s@UpdateFilter' {} a -> s {action = a} :: UpdateFilter)

-- | The description of the filter.
updateFilter_description :: Lens.Lens' UpdateFilter (Core.Maybe Core.Text)
updateFilter_description = Lens.lens (\UpdateFilter' {description} -> description) (\s@UpdateFilter' {} a -> s {description = a} :: UpdateFilter)

-- | The unique ID of the detector that specifies the GuardDuty service where
-- you want to update a filter.
updateFilter_detectorId :: Lens.Lens' UpdateFilter Core.Text
updateFilter_detectorId = Lens.lens (\UpdateFilter' {detectorId} -> detectorId) (\s@UpdateFilter' {} a -> s {detectorId = a} :: UpdateFilter)

-- | The name of the filter.
updateFilter_filterName :: Lens.Lens' UpdateFilter Core.Text
updateFilter_filterName = Lens.lens (\UpdateFilter' {filterName} -> filterName) (\s@UpdateFilter' {} a -> s {filterName = a} :: UpdateFilter)

instance Core.AWSRequest UpdateFilter where
  type AWSResponse UpdateFilter = UpdateFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFilterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "name")
      )

instance Core.Hashable UpdateFilter

instance Core.NFData UpdateFilter

instance Core.ToHeaders UpdateFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFilter where
  toJSON UpdateFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rank" Core..=) Core.<$> rank,
            ("findingCriteria" Core..=) Core.<$> findingCriteria,
            ("action" Core..=) Core.<$> action,
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateFilter where
  toPath UpdateFilter' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/filter/",
        Core.toBS filterName
      ]

instance Core.ToQuery UpdateFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateFilterResponse' smart constructor.
data UpdateFilterResponse = UpdateFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the filter.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'name'
  Core.Text ->
  UpdateFilterResponse
newUpdateFilterResponse pHttpStatus_ pName_ =
  UpdateFilterResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateFilterResponse_httpStatus :: Lens.Lens' UpdateFilterResponse Core.Int
updateFilterResponse_httpStatus = Lens.lens (\UpdateFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateFilterResponse' {} a -> s {httpStatus = a} :: UpdateFilterResponse)

-- | The name of the filter.
updateFilterResponse_name :: Lens.Lens' UpdateFilterResponse Core.Text
updateFilterResponse_name = Lens.lens (\UpdateFilterResponse' {name} -> name) (\s@UpdateFilterResponse' {} a -> s {name = a} :: UpdateFilterResponse)

instance Core.NFData UpdateFilterResponse
