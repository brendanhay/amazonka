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
-- Module      : Network.AWS.GuardDuty.ArchiveFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.GuardDuty.ArchiveFindings
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

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newArchiveFindings' smart constructor.
data ArchiveFindings = ArchiveFindings'
  { -- | The ID of the detector that specifies the GuardDuty service whose
    -- findings you want to archive.
    detectorId :: Core.Text,
    -- | The IDs of the findings that you want to archive.
    findingIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ArchiveFindings
newArchiveFindings pDetectorId_ =
  ArchiveFindings'
    { detectorId = pDetectorId_,
      findingIds = Core.mempty
    }

-- | The ID of the detector that specifies the GuardDuty service whose
-- findings you want to archive.
archiveFindings_detectorId :: Lens.Lens' ArchiveFindings Core.Text
archiveFindings_detectorId = Lens.lens (\ArchiveFindings' {detectorId} -> detectorId) (\s@ArchiveFindings' {} a -> s {detectorId = a} :: ArchiveFindings)

-- | The IDs of the findings that you want to archive.
archiveFindings_findingIds :: Lens.Lens' ArchiveFindings [Core.Text]
archiveFindings_findingIds = Lens.lens (\ArchiveFindings' {findingIds} -> findingIds) (\s@ArchiveFindings' {} a -> s {findingIds = a} :: ArchiveFindings) Core.. Lens._Coerce

instance Core.AWSRequest ArchiveFindings where
  type
    AWSResponse ArchiveFindings =
      ArchiveFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ArchiveFindingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ArchiveFindings

instance Core.NFData ArchiveFindings

instance Core.ToHeaders ArchiveFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ArchiveFindings where
  toJSON ArchiveFindings' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("findingIds" Core..= findingIds)]
      )

instance Core.ToPath ArchiveFindings where
  toPath ArchiveFindings' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/findings/archive"
      ]

instance Core.ToQuery ArchiveFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newArchiveFindingsResponse' smart constructor.
data ArchiveFindingsResponse = ArchiveFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ArchiveFindingsResponse
newArchiveFindingsResponse pHttpStatus_ =
  ArchiveFindingsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
archiveFindingsResponse_httpStatus :: Lens.Lens' ArchiveFindingsResponse Core.Int
archiveFindingsResponse_httpStatus = Lens.lens (\ArchiveFindingsResponse' {httpStatus} -> httpStatus) (\s@ArchiveFindingsResponse' {} a -> s {httpStatus = a} :: ArchiveFindingsResponse)

instance Core.NFData ArchiveFindingsResponse
