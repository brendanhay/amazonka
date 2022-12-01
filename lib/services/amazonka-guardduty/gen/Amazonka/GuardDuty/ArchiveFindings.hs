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
-- Module      : Amazonka.GuardDuty.ArchiveFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archives GuardDuty findings that are specified by the list of finding
-- IDs.
--
-- Only the administrator account can archive findings. Member accounts
-- don\'t have permission to archive findings from their accounts.
module Amazonka.GuardDuty.ArchiveFindings
  ( -- * Creating a Request
    ArchiveFindings (..),
    newArchiveFindings,

    -- * Request Lenses
    archiveFindings_detectorId,
    archiveFindings_findingIds,

    -- * Destructuring the Response
    ArchiveFindingsResponse (..),
    newArchiveFindingsResponse,

    -- * Response Lenses
    archiveFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newArchiveFindings' smart constructor.
data ArchiveFindings = ArchiveFindings'
  { -- | The ID of the detector that specifies the GuardDuty service whose
    -- findings you want to archive.
    detectorId :: Prelude.Text,
    -- | The IDs of the findings that you want to archive.
    findingIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'archiveFindings_detectorId' - The ID of the detector that specifies the GuardDuty service whose
-- findings you want to archive.
--
-- 'findingIds', 'archiveFindings_findingIds' - The IDs of the findings that you want to archive.
newArchiveFindings ::
  -- | 'detectorId'
  Prelude.Text ->
  ArchiveFindings
newArchiveFindings pDetectorId_ =
  ArchiveFindings'
    { detectorId = pDetectorId_,
      findingIds = Prelude.mempty
    }

-- | The ID of the detector that specifies the GuardDuty service whose
-- findings you want to archive.
archiveFindings_detectorId :: Lens.Lens' ArchiveFindings Prelude.Text
archiveFindings_detectorId = Lens.lens (\ArchiveFindings' {detectorId} -> detectorId) (\s@ArchiveFindings' {} a -> s {detectorId = a} :: ArchiveFindings)

-- | The IDs of the findings that you want to archive.
archiveFindings_findingIds :: Lens.Lens' ArchiveFindings [Prelude.Text]
archiveFindings_findingIds = Lens.lens (\ArchiveFindings' {findingIds} -> findingIds) (\s@ArchiveFindings' {} a -> s {findingIds = a} :: ArchiveFindings) Prelude.. Lens.coerced

instance Core.AWSRequest ArchiveFindings where
  type
    AWSResponse ArchiveFindings =
      ArchiveFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ArchiveFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ArchiveFindings where
  hashWithSalt _salt ArchiveFindings' {..} =
    _salt `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` findingIds

instance Prelude.NFData ArchiveFindings where
  rnf ArchiveFindings' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf findingIds

instance Core.ToHeaders ArchiveFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ArchiveFindings where
  toJSON ArchiveFindings' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("findingIds" Core..= findingIds)]
      )

instance Core.ToPath ArchiveFindings where
  toPath ArchiveFindings' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/findings/archive"
      ]

instance Core.ToQuery ArchiveFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newArchiveFindingsResponse' smart constructor.
data ArchiveFindingsResponse = ArchiveFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'archiveFindingsResponse_httpStatus' - The response's http status code.
newArchiveFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ArchiveFindingsResponse
newArchiveFindingsResponse pHttpStatus_ =
  ArchiveFindingsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
archiveFindingsResponse_httpStatus :: Lens.Lens' ArchiveFindingsResponse Prelude.Int
archiveFindingsResponse_httpStatus = Lens.lens (\ArchiveFindingsResponse' {httpStatus} -> httpStatus) (\s@ArchiveFindingsResponse' {} a -> s {httpStatus = a} :: ArchiveFindingsResponse)

instance Prelude.NFData ArchiveFindingsResponse where
  rnf ArchiveFindingsResponse' {..} =
    Prelude.rnf httpStatus
