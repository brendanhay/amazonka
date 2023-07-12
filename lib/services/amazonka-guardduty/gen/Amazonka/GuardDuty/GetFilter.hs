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
-- Module      : Amazonka.GuardDuty.GetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the filter specified by the filter name.
module Amazonka.GuardDuty.GetFilter
  ( -- * Creating a Request
    GetFilter (..),
    newGetFilter,

    -- * Request Lenses
    getFilter_detectorId,
    getFilter_filterName,

    -- * Destructuring the Response
    GetFilterResponse (..),
    newGetFilterResponse,

    -- * Response Lenses
    getFilterResponse_description,
    getFilterResponse_rank,
    getFilterResponse_tags,
    getFilterResponse_httpStatus,
    getFilterResponse_name,
    getFilterResponse_action,
    getFilterResponse_findingCriteria,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFilter' smart constructor.
data GetFilter = GetFilter'
  { -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Prelude.Text,
    -- | The name of the filter you want to get.
    filterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getFilter_detectorId' - The unique ID of the detector that the filter is associated with.
--
-- 'filterName', 'getFilter_filterName' - The name of the filter you want to get.
newGetFilter ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'filterName'
  Prelude.Text ->
  GetFilter
newGetFilter pDetectorId_ pFilterName_ =
  GetFilter'
    { detectorId = pDetectorId_,
      filterName = pFilterName_
    }

-- | The unique ID of the detector that the filter is associated with.
getFilter_detectorId :: Lens.Lens' GetFilter Prelude.Text
getFilter_detectorId = Lens.lens (\GetFilter' {detectorId} -> detectorId) (\s@GetFilter' {} a -> s {detectorId = a} :: GetFilter)

-- | The name of the filter you want to get.
getFilter_filterName :: Lens.Lens' GetFilter Prelude.Text
getFilter_filterName = Lens.lens (\GetFilter' {filterName} -> filterName) (\s@GetFilter' {} a -> s {filterName = a} :: GetFilter)

instance Core.AWSRequest GetFilter where
  type AWSResponse GetFilter = GetFilterResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFilterResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "rank")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "action")
            Prelude.<*> (x Data..:> "findingCriteria")
      )

instance Prelude.Hashable GetFilter where
  hashWithSalt _salt GetFilter' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` filterName

instance Prelude.NFData GetFilter where
  rnf GetFilter' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf filterName

instance Data.ToHeaders GetFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetFilter where
  toPath GetFilter' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/filter/",
        Data.toBS filterName
      ]

instance Data.ToQuery GetFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFilterResponse' smart constructor.
data GetFilterResponse = GetFilterResponse'
  { -- | The description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the position of the filter in the list of current filters.
    -- Also specifies the order in which this filter is applied to the
    -- findings.
    rank :: Prelude.Maybe Prelude.Natural,
    -- | The tags of the filter resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the filter.
    name :: Prelude.Text,
    -- | Specifies the action that is to be applied to the findings that match
    -- the filter.
    action :: FilterAction,
    -- | Represents the criteria to be used in the filter for querying findings.
    findingCriteria :: FindingCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getFilterResponse_description' - The description of the filter.
--
-- 'rank', 'getFilterResponse_rank' - Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
--
-- 'tags', 'getFilterResponse_tags' - The tags of the filter resource.
--
-- 'httpStatus', 'getFilterResponse_httpStatus' - The response's http status code.
--
-- 'name', 'getFilterResponse_name' - The name of the filter.
--
-- 'action', 'getFilterResponse_action' - Specifies the action that is to be applied to the findings that match
-- the filter.
--
-- 'findingCriteria', 'getFilterResponse_findingCriteria' - Represents the criteria to be used in the filter for querying findings.
newGetFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'action'
  FilterAction ->
  -- | 'findingCriteria'
  FindingCriteria ->
  GetFilterResponse
newGetFilterResponse
  pHttpStatus_
  pName_
  pAction_
  pFindingCriteria_ =
    GetFilterResponse'
      { description = Prelude.Nothing,
        rank = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        action = pAction_,
        findingCriteria = pFindingCriteria_
      }

-- | The description of the filter.
getFilterResponse_description :: Lens.Lens' GetFilterResponse (Prelude.Maybe Prelude.Text)
getFilterResponse_description = Lens.lens (\GetFilterResponse' {description} -> description) (\s@GetFilterResponse' {} a -> s {description = a} :: GetFilterResponse)

-- | Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
getFilterResponse_rank :: Lens.Lens' GetFilterResponse (Prelude.Maybe Prelude.Natural)
getFilterResponse_rank = Lens.lens (\GetFilterResponse' {rank} -> rank) (\s@GetFilterResponse' {} a -> s {rank = a} :: GetFilterResponse)

-- | The tags of the filter resource.
getFilterResponse_tags :: Lens.Lens' GetFilterResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getFilterResponse_tags = Lens.lens (\GetFilterResponse' {tags} -> tags) (\s@GetFilterResponse' {} a -> s {tags = a} :: GetFilterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getFilterResponse_httpStatus :: Lens.Lens' GetFilterResponse Prelude.Int
getFilterResponse_httpStatus = Lens.lens (\GetFilterResponse' {httpStatus} -> httpStatus) (\s@GetFilterResponse' {} a -> s {httpStatus = a} :: GetFilterResponse)

-- | The name of the filter.
getFilterResponse_name :: Lens.Lens' GetFilterResponse Prelude.Text
getFilterResponse_name = Lens.lens (\GetFilterResponse' {name} -> name) (\s@GetFilterResponse' {} a -> s {name = a} :: GetFilterResponse)

-- | Specifies the action that is to be applied to the findings that match
-- the filter.
getFilterResponse_action :: Lens.Lens' GetFilterResponse FilterAction
getFilterResponse_action = Lens.lens (\GetFilterResponse' {action} -> action) (\s@GetFilterResponse' {} a -> s {action = a} :: GetFilterResponse)

-- | Represents the criteria to be used in the filter for querying findings.
getFilterResponse_findingCriteria :: Lens.Lens' GetFilterResponse FindingCriteria
getFilterResponse_findingCriteria = Lens.lens (\GetFilterResponse' {findingCriteria} -> findingCriteria) (\s@GetFilterResponse' {} a -> s {findingCriteria = a} :: GetFilterResponse)

instance Prelude.NFData GetFilterResponse where
  rnf GetFilterResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf rank
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf findingCriteria
