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
-- Module      : Amazonka.SecurityHub.UpdateFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @UpdateFindings@ is deprecated. Instead of @UpdateFindings@, use
-- @BatchUpdateFindings@.
--
-- Updates the @Note@ and @RecordState@ of the Security Hub-aggregated
-- findings that the filter attributes specify. Any member account that can
-- view the finding also sees the update to the finding.
module Amazonka.SecurityHub.UpdateFindings
  ( -- * Creating a Request
    UpdateFindings (..),
    newUpdateFindings,

    -- * Request Lenses
    updateFindings_note,
    updateFindings_recordState,
    updateFindings_filters,

    -- * Destructuring the Response
    UpdateFindingsResponse (..),
    newUpdateFindingsResponse,

    -- * Response Lenses
    updateFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newUpdateFindings' smart constructor.
data UpdateFindings = UpdateFindings'
  { -- | The updated note for the finding.
    note :: Prelude.Maybe NoteUpdate,
    -- | The updated record state for the finding.
    recordState :: Prelude.Maybe RecordState,
    -- | A collection of attributes that specify which findings you want to
    -- update.
    filters :: AwsSecurityFindingFilters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'note', 'updateFindings_note' - The updated note for the finding.
--
-- 'recordState', 'updateFindings_recordState' - The updated record state for the finding.
--
-- 'filters', 'updateFindings_filters' - A collection of attributes that specify which findings you want to
-- update.
newUpdateFindings ::
  -- | 'filters'
  AwsSecurityFindingFilters ->
  UpdateFindings
newUpdateFindings pFilters_ =
  UpdateFindings'
    { note = Prelude.Nothing,
      recordState = Prelude.Nothing,
      filters = pFilters_
    }

-- | The updated note for the finding.
updateFindings_note :: Lens.Lens' UpdateFindings (Prelude.Maybe NoteUpdate)
updateFindings_note = Lens.lens (\UpdateFindings' {note} -> note) (\s@UpdateFindings' {} a -> s {note = a} :: UpdateFindings)

-- | The updated record state for the finding.
updateFindings_recordState :: Lens.Lens' UpdateFindings (Prelude.Maybe RecordState)
updateFindings_recordState = Lens.lens (\UpdateFindings' {recordState} -> recordState) (\s@UpdateFindings' {} a -> s {recordState = a} :: UpdateFindings)

-- | A collection of attributes that specify which findings you want to
-- update.
updateFindings_filters :: Lens.Lens' UpdateFindings AwsSecurityFindingFilters
updateFindings_filters = Lens.lens (\UpdateFindings' {filters} -> filters) (\s@UpdateFindings' {} a -> s {filters = a} :: UpdateFindings)

instance Core.AWSRequest UpdateFindings where
  type
    AWSResponse UpdateFindings =
      UpdateFindingsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFindings where
  hashWithSalt _salt UpdateFindings' {..} =
    _salt `Prelude.hashWithSalt` note
      `Prelude.hashWithSalt` recordState
      `Prelude.hashWithSalt` filters

instance Prelude.NFData UpdateFindings where
  rnf UpdateFindings' {..} =
    Prelude.rnf note
      `Prelude.seq` Prelude.rnf recordState
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders UpdateFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFindings where
  toJSON UpdateFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Note" Data..=) Prelude.<$> note,
            ("RecordState" Data..=) Prelude.<$> recordState,
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath UpdateFindings where
  toPath = Prelude.const "/findings"

instance Data.ToQuery UpdateFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFindingsResponse' smart constructor.
data UpdateFindingsResponse = UpdateFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFindingsResponse_httpStatus' - The response's http status code.
newUpdateFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFindingsResponse
newUpdateFindingsResponse pHttpStatus_ =
  UpdateFindingsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateFindingsResponse_httpStatus :: Lens.Lens' UpdateFindingsResponse Prelude.Int
updateFindingsResponse_httpStatus = Lens.lens (\UpdateFindingsResponse' {httpStatus} -> httpStatus) (\s@UpdateFindingsResponse' {} a -> s {httpStatus = a} :: UpdateFindingsResponse)

instance Prelude.NFData UpdateFindingsResponse where
  rnf UpdateFindingsResponse' {..} =
    Prelude.rnf httpStatus
