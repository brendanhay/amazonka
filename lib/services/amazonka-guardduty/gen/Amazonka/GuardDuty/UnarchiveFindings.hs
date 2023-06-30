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
-- Module      : Amazonka.GuardDuty.UnarchiveFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchives GuardDuty findings specified by the @findingIds@.
module Amazonka.GuardDuty.UnarchiveFindings
  ( -- * Creating a Request
    UnarchiveFindings (..),
    newUnarchiveFindings,

    -- * Request Lenses
    unarchiveFindings_detectorId,
    unarchiveFindings_findingIds,

    -- * Destructuring the Response
    UnarchiveFindingsResponse (..),
    newUnarchiveFindingsResponse,

    -- * Response Lenses
    unarchiveFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnarchiveFindings' smart constructor.
data UnarchiveFindings = UnarchiveFindings'
  { -- | The ID of the detector associated with the findings to unarchive.
    detectorId :: Prelude.Text,
    -- | The IDs of the findings to unarchive.
    findingIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnarchiveFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'unarchiveFindings_detectorId' - The ID of the detector associated with the findings to unarchive.
--
-- 'findingIds', 'unarchiveFindings_findingIds' - The IDs of the findings to unarchive.
newUnarchiveFindings ::
  -- | 'detectorId'
  Prelude.Text ->
  UnarchiveFindings
newUnarchiveFindings pDetectorId_ =
  UnarchiveFindings'
    { detectorId = pDetectorId_,
      findingIds = Prelude.mempty
    }

-- | The ID of the detector associated with the findings to unarchive.
unarchiveFindings_detectorId :: Lens.Lens' UnarchiveFindings Prelude.Text
unarchiveFindings_detectorId = Lens.lens (\UnarchiveFindings' {detectorId} -> detectorId) (\s@UnarchiveFindings' {} a -> s {detectorId = a} :: UnarchiveFindings)

-- | The IDs of the findings to unarchive.
unarchiveFindings_findingIds :: Lens.Lens' UnarchiveFindings [Prelude.Text]
unarchiveFindings_findingIds = Lens.lens (\UnarchiveFindings' {findingIds} -> findingIds) (\s@UnarchiveFindings' {} a -> s {findingIds = a} :: UnarchiveFindings) Prelude.. Lens.coerced

instance Core.AWSRequest UnarchiveFindings where
  type
    AWSResponse UnarchiveFindings =
      UnarchiveFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnarchiveFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnarchiveFindings where
  hashWithSalt _salt UnarchiveFindings' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` findingIds

instance Prelude.NFData UnarchiveFindings where
  rnf UnarchiveFindings' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf findingIds

instance Data.ToHeaders UnarchiveFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnarchiveFindings where
  toJSON UnarchiveFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("findingIds" Data..= findingIds)]
      )

instance Data.ToPath UnarchiveFindings where
  toPath UnarchiveFindings' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/findings/unarchive"
      ]

instance Data.ToQuery UnarchiveFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnarchiveFindingsResponse' smart constructor.
data UnarchiveFindingsResponse = UnarchiveFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnarchiveFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'unarchiveFindingsResponse_httpStatus' - The response's http status code.
newUnarchiveFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnarchiveFindingsResponse
newUnarchiveFindingsResponse pHttpStatus_ =
  UnarchiveFindingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
unarchiveFindingsResponse_httpStatus :: Lens.Lens' UnarchiveFindingsResponse Prelude.Int
unarchiveFindingsResponse_httpStatus = Lens.lens (\UnarchiveFindingsResponse' {httpStatus} -> httpStatus) (\s@UnarchiveFindingsResponse' {} a -> s {httpStatus = a} :: UnarchiveFindingsResponse)

instance Prelude.NFData UnarchiveFindingsResponse where
  rnf UnarchiveFindingsResponse' {..} =
    Prelude.rnf httpStatus
