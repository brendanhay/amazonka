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
-- Module      : Network.AWS.CloudHSMv2.CopyBackupToRegion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copy an AWS CloudHSM cluster backup to a different region.
module Network.AWS.CloudHSMv2.CopyBackupToRegion
  ( -- * Creating a Request
    CopyBackupToRegion (..),
    newCopyBackupToRegion,

    -- * Request Lenses
    copyBackupToRegion_tagList,
    copyBackupToRegion_destinationRegion,
    copyBackupToRegion_backupId,

    -- * Destructuring the Response
    CopyBackupToRegionResponse (..),
    newCopyBackupToRegionResponse,

    -- * Response Lenses
    copyBackupToRegionResponse_destinationBackup,
    copyBackupToRegionResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCopyBackupToRegion' smart constructor.
data CopyBackupToRegion = CopyBackupToRegion'
  { -- | Tags to apply to the destination backup during creation. If you specify
    -- tags, only these tags will be applied to the destination backup. If you
    -- do not specify tags, the service copies tags from the source backup to
    -- the destination backup.
    tagList :: Core.Maybe [Tag],
    -- | The AWS region that will contain your copied CloudHSM cluster backup.
    destinationRegion :: Core.Text,
    -- | The ID of the backup that will be copied to the destination region.
    backupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyBackupToRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'copyBackupToRegion_tagList' - Tags to apply to the destination backup during creation. If you specify
-- tags, only these tags will be applied to the destination backup. If you
-- do not specify tags, the service copies tags from the source backup to
-- the destination backup.
--
-- 'destinationRegion', 'copyBackupToRegion_destinationRegion' - The AWS region that will contain your copied CloudHSM cluster backup.
--
-- 'backupId', 'copyBackupToRegion_backupId' - The ID of the backup that will be copied to the destination region.
newCopyBackupToRegion ::
  -- | 'destinationRegion'
  Core.Text ->
  -- | 'backupId'
  Core.Text ->
  CopyBackupToRegion
newCopyBackupToRegion pDestinationRegion_ pBackupId_ =
  CopyBackupToRegion'
    { tagList = Core.Nothing,
      destinationRegion = pDestinationRegion_,
      backupId = pBackupId_
    }

-- | Tags to apply to the destination backup during creation. If you specify
-- tags, only these tags will be applied to the destination backup. If you
-- do not specify tags, the service copies tags from the source backup to
-- the destination backup.
copyBackupToRegion_tagList :: Lens.Lens' CopyBackupToRegion (Core.Maybe [Tag])
copyBackupToRegion_tagList = Lens.lens (\CopyBackupToRegion' {tagList} -> tagList) (\s@CopyBackupToRegion' {} a -> s {tagList = a} :: CopyBackupToRegion) Core.. Lens.mapping Lens._Coerce

-- | The AWS region that will contain your copied CloudHSM cluster backup.
copyBackupToRegion_destinationRegion :: Lens.Lens' CopyBackupToRegion Core.Text
copyBackupToRegion_destinationRegion = Lens.lens (\CopyBackupToRegion' {destinationRegion} -> destinationRegion) (\s@CopyBackupToRegion' {} a -> s {destinationRegion = a} :: CopyBackupToRegion)

-- | The ID of the backup that will be copied to the destination region.
copyBackupToRegion_backupId :: Lens.Lens' CopyBackupToRegion Core.Text
copyBackupToRegion_backupId = Lens.lens (\CopyBackupToRegion' {backupId} -> backupId) (\s@CopyBackupToRegion' {} a -> s {backupId = a} :: CopyBackupToRegion)

instance Core.AWSRequest CopyBackupToRegion where
  type
    AWSResponse CopyBackupToRegion =
      CopyBackupToRegionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyBackupToRegionResponse'
            Core.<$> (x Core..?> "DestinationBackup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyBackupToRegion

instance Core.NFData CopyBackupToRegion

instance Core.ToHeaders CopyBackupToRegion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.CopyBackupToRegion" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CopyBackupToRegion where
  toJSON CopyBackupToRegion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagList" Core..=) Core.<$> tagList,
            Core.Just
              ("DestinationRegion" Core..= destinationRegion),
            Core.Just ("BackupId" Core..= backupId)
          ]
      )

instance Core.ToPath CopyBackupToRegion where
  toPath = Core.const "/"

instance Core.ToQuery CopyBackupToRegion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCopyBackupToRegionResponse' smart constructor.
data CopyBackupToRegionResponse = CopyBackupToRegionResponse'
  { -- | Information on the backup that will be copied to the destination region,
    -- including CreateTimestamp, SourceBackup, SourceCluster, and Source
    -- Region. CreateTimestamp of the destination backup will be the same as
    -- that of the source backup.
    --
    -- You will need to use the @sourceBackupID@ returned in this operation to
    -- use the DescribeBackups operation on the backup that will be copied to
    -- the destination region.
    destinationBackup :: Core.Maybe DestinationBackup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyBackupToRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationBackup', 'copyBackupToRegionResponse_destinationBackup' - Information on the backup that will be copied to the destination region,
-- including CreateTimestamp, SourceBackup, SourceCluster, and Source
-- Region. CreateTimestamp of the destination backup will be the same as
-- that of the source backup.
--
-- You will need to use the @sourceBackupID@ returned in this operation to
-- use the DescribeBackups operation on the backup that will be copied to
-- the destination region.
--
-- 'httpStatus', 'copyBackupToRegionResponse_httpStatus' - The response's http status code.
newCopyBackupToRegionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CopyBackupToRegionResponse
newCopyBackupToRegionResponse pHttpStatus_ =
  CopyBackupToRegionResponse'
    { destinationBackup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the backup that will be copied to the destination region,
-- including CreateTimestamp, SourceBackup, SourceCluster, and Source
-- Region. CreateTimestamp of the destination backup will be the same as
-- that of the source backup.
--
-- You will need to use the @sourceBackupID@ returned in this operation to
-- use the DescribeBackups operation on the backup that will be copied to
-- the destination region.
copyBackupToRegionResponse_destinationBackup :: Lens.Lens' CopyBackupToRegionResponse (Core.Maybe DestinationBackup)
copyBackupToRegionResponse_destinationBackup = Lens.lens (\CopyBackupToRegionResponse' {destinationBackup} -> destinationBackup) (\s@CopyBackupToRegionResponse' {} a -> s {destinationBackup = a} :: CopyBackupToRegionResponse)

-- | The response's http status code.
copyBackupToRegionResponse_httpStatus :: Lens.Lens' CopyBackupToRegionResponse Core.Int
copyBackupToRegionResponse_httpStatus = Lens.lens (\CopyBackupToRegionResponse' {httpStatus} -> httpStatus) (\s@CopyBackupToRegionResponse' {} a -> s {httpStatus = a} :: CopyBackupToRegionResponse)

instance Core.NFData CopyBackupToRegionResponse
