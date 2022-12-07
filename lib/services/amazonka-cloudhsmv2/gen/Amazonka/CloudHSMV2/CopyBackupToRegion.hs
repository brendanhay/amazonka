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
-- Module      : Amazonka.CloudHSMV2.CopyBackupToRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copy an AWS CloudHSM cluster backup to a different region.
module Amazonka.CloudHSMV2.CopyBackupToRegion
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

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyBackupToRegion' smart constructor.
data CopyBackupToRegion = CopyBackupToRegion'
  { -- | Tags to apply to the destination backup during creation. If you specify
    -- tags, only these tags will be applied to the destination backup. If you
    -- do not specify tags, the service copies tags from the source backup to
    -- the destination backup.
    tagList :: Prelude.Maybe [Tag],
    -- | The AWS region that will contain your copied CloudHSM cluster backup.
    destinationRegion :: Prelude.Text,
    -- | The ID of the backup that will be copied to the destination region.
    backupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'backupId'
  Prelude.Text ->
  CopyBackupToRegion
newCopyBackupToRegion pDestinationRegion_ pBackupId_ =
  CopyBackupToRegion'
    { tagList = Prelude.Nothing,
      destinationRegion = pDestinationRegion_,
      backupId = pBackupId_
    }

-- | Tags to apply to the destination backup during creation. If you specify
-- tags, only these tags will be applied to the destination backup. If you
-- do not specify tags, the service copies tags from the source backup to
-- the destination backup.
copyBackupToRegion_tagList :: Lens.Lens' CopyBackupToRegion (Prelude.Maybe [Tag])
copyBackupToRegion_tagList = Lens.lens (\CopyBackupToRegion' {tagList} -> tagList) (\s@CopyBackupToRegion' {} a -> s {tagList = a} :: CopyBackupToRegion) Prelude.. Lens.mapping Lens.coerced

-- | The AWS region that will contain your copied CloudHSM cluster backup.
copyBackupToRegion_destinationRegion :: Lens.Lens' CopyBackupToRegion Prelude.Text
copyBackupToRegion_destinationRegion = Lens.lens (\CopyBackupToRegion' {destinationRegion} -> destinationRegion) (\s@CopyBackupToRegion' {} a -> s {destinationRegion = a} :: CopyBackupToRegion)

-- | The ID of the backup that will be copied to the destination region.
copyBackupToRegion_backupId :: Lens.Lens' CopyBackupToRegion Prelude.Text
copyBackupToRegion_backupId = Lens.lens (\CopyBackupToRegion' {backupId} -> backupId) (\s@CopyBackupToRegion' {} a -> s {backupId = a} :: CopyBackupToRegion)

instance Core.AWSRequest CopyBackupToRegion where
  type
    AWSResponse CopyBackupToRegion =
      CopyBackupToRegionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyBackupToRegionResponse'
            Prelude.<$> (x Data..?> "DestinationBackup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyBackupToRegion where
  hashWithSalt _salt CopyBackupToRegion' {..} =
    _salt `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` backupId

instance Prelude.NFData CopyBackupToRegion where
  rnf CopyBackupToRegion' {..} =
    Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf backupId

instance Data.ToHeaders CopyBackupToRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BaldrApiService.CopyBackupToRegion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CopyBackupToRegion where
  toJSON CopyBackupToRegion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TagList" Data..=) Prelude.<$> tagList,
            Prelude.Just
              ("DestinationRegion" Data..= destinationRegion),
            Prelude.Just ("BackupId" Data..= backupId)
          ]
      )

instance Data.ToPath CopyBackupToRegion where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyBackupToRegion where
  toQuery = Prelude.const Prelude.mempty

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
    destinationBackup :: Prelude.Maybe DestinationBackup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CopyBackupToRegionResponse
newCopyBackupToRegionResponse pHttpStatus_ =
  CopyBackupToRegionResponse'
    { destinationBackup =
        Prelude.Nothing,
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
copyBackupToRegionResponse_destinationBackup :: Lens.Lens' CopyBackupToRegionResponse (Prelude.Maybe DestinationBackup)
copyBackupToRegionResponse_destinationBackup = Lens.lens (\CopyBackupToRegionResponse' {destinationBackup} -> destinationBackup) (\s@CopyBackupToRegionResponse' {} a -> s {destinationBackup = a} :: CopyBackupToRegionResponse)

-- | The response's http status code.
copyBackupToRegionResponse_httpStatus :: Lens.Lens' CopyBackupToRegionResponse Prelude.Int
copyBackupToRegionResponse_httpStatus = Lens.lens (\CopyBackupToRegionResponse' {httpStatus} -> httpStatus) (\s@CopyBackupToRegionResponse' {} a -> s {httpStatus = a} :: CopyBackupToRegionResponse)

instance Prelude.NFData CopyBackupToRegionResponse where
  rnf CopyBackupToRegionResponse' {..} =
    Prelude.rnf destinationBackup
      `Prelude.seq` Prelude.rnf httpStatus
