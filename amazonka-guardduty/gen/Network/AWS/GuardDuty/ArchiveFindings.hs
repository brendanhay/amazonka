{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newArchiveFindings' smart constructor.
data ArchiveFindings = ArchiveFindings'
  { -- | The ID of the detector that specifies the GuardDuty service whose
    -- findings you want to archive.
    detectorId :: Prelude.Text,
    -- | The IDs of the findings that you want to archive.
    findingIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
archiveFindings_findingIds = Lens.lens (\ArchiveFindings' {findingIds} -> findingIds) (\s@ArchiveFindings' {} a -> s {findingIds = a} :: ArchiveFindings) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest ArchiveFindings where
  type Rs ArchiveFindings = ArchiveFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ArchiveFindingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ArchiveFindings

instance Prelude.NFData ArchiveFindings

instance Prelude.ToHeaders ArchiveFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ArchiveFindings where
  toJSON ArchiveFindings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("findingIds" Prelude..= findingIds)]
      )

instance Prelude.ToPath ArchiveFindings where
  toPath ArchiveFindings' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/findings/archive"
      ]

instance Prelude.ToQuery ArchiveFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newArchiveFindingsResponse' smart constructor.
data ArchiveFindingsResponse = ArchiveFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ArchiveFindingsResponse
