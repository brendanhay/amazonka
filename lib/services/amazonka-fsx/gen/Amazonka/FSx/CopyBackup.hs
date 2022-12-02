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
-- Module      : Amazonka.FSx.CopyBackup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies an existing backup within the same Amazon Web Services account to
-- another Amazon Web Services Region (cross-Region copy) or within the
-- same Amazon Web Services Region (in-Region copy). You can have up to
-- five backup copy requests in progress to a single destination Region per
-- account.
--
-- You can use cross-Region backup copies for cross-Region disaster
-- recovery. You can periodically take backups and copy them to another
-- Region so that in the event of a disaster in the primary Region, you can
-- restore from backup and recover availability quickly in the other
-- Region. You can make cross-Region copies only within your Amazon Web
-- Services partition. A partition is a grouping of Regions. Amazon Web
-- Services currently has three partitions: @aws@ (Standard Regions),
-- @aws-cn@ (China Regions), and @aws-us-gov@ (Amazon Web Services GovCloud
-- [US] Regions).
--
-- You can also use backup copies to clone your file dataset to another
-- Region or within the same Region.
--
-- You can use the @SourceRegion@ parameter to specify the Amazon Web
-- Services Region from which the backup will be copied. For example, if
-- you make the call from the @us-west-1@ Region and want to copy a backup
-- from the @us-east-2@ Region, you specify @us-east-2@ in the
-- @SourceRegion@ parameter to make a cross-Region copy. If you don\'t
-- specify a Region, the backup copy is created in the same Region where
-- the request is sent from (in-Region copy).
--
-- For more information about creating backup copies, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/using-backups.html#copy-backups Copying backups>
-- in the /Amazon FSx for Windows User Guide/,
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/using-backups-fsx.html#copy-backups Copying backups>
-- in the /Amazon FSx for Lustre User Guide/, and
-- <https://docs.aws.amazon.com/fsx/latest/OpenZFSGuide/using-backups.html#copy-backups Copying backups>
-- in the /Amazon FSx for OpenZFS User Guide/.
module Amazonka.FSx.CopyBackup
  ( -- * Creating a Request
    CopyBackup (..),
    newCopyBackup,

    -- * Request Lenses
    copyBackup_tags,
    copyBackup_sourceRegion,
    copyBackup_clientRequestToken,
    copyBackup_copyTags,
    copyBackup_kmsKeyId,
    copyBackup_sourceBackupId,

    -- * Destructuring the Response
    CopyBackupResponse (..),
    newCopyBackupResponse,

    -- * Response Lenses
    copyBackupResponse_backup,
    copyBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyBackup' smart constructor.
data CopyBackup = CopyBackup'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The source Amazon Web Services Region of the backup. Specifies the
    -- Amazon Web Services Region from which the backup is being copied. The
    -- source and destination Regions must be in the same Amazon Web Services
    -- partition. If you don\'t specify a Region, @SourceRegion@ defaults to
    -- the Region where the request is sent from (in-Region copy).
    sourceRegion :: Prelude.Maybe Prelude.Text,
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A Boolean flag indicating whether tags from the source backup should be
    -- copied to the backup copy. This value defaults to @false@.
    --
    -- If you set @CopyTags@ to @true@ and the source backup has existing tags,
    -- you can use the @Tags@ parameter to create new tags, provided that the
    -- sum of the source backup tags and the new tags doesn\'t exceed 50. Both
    -- sets of tags are merged. If there are tag conflicts (for example, two
    -- tags with the same key but different values), the tags created with the
    -- @Tags@ parameter take precedence.
    copyTags :: Prelude.Maybe Prelude.Bool,
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the source backup. Specifies the ID of the backup that\'s
    -- being copied.
    sourceBackupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'copyBackup_tags' - Undocumented member.
--
-- 'sourceRegion', 'copyBackup_sourceRegion' - The source Amazon Web Services Region of the backup. Specifies the
-- Amazon Web Services Region from which the backup is being copied. The
-- source and destination Regions must be in the same Amazon Web Services
-- partition. If you don\'t specify a Region, @SourceRegion@ defaults to
-- the Region where the request is sent from (in-Region copy).
--
-- 'clientRequestToken', 'copyBackup_clientRequestToken' - Undocumented member.
--
-- 'copyTags', 'copyBackup_copyTags' - A Boolean flag indicating whether tags from the source backup should be
-- copied to the backup copy. This value defaults to @false@.
--
-- If you set @CopyTags@ to @true@ and the source backup has existing tags,
-- you can use the @Tags@ parameter to create new tags, provided that the
-- sum of the source backup tags and the new tags doesn\'t exceed 50. Both
-- sets of tags are merged. If there are tag conflicts (for example, two
-- tags with the same key but different values), the tags created with the
-- @Tags@ parameter take precedence.
--
-- 'kmsKeyId', 'copyBackup_kmsKeyId' - Undocumented member.
--
-- 'sourceBackupId', 'copyBackup_sourceBackupId' - The ID of the source backup. Specifies the ID of the backup that\'s
-- being copied.
newCopyBackup ::
  -- | 'sourceBackupId'
  Prelude.Text ->
  CopyBackup
newCopyBackup pSourceBackupId_ =
  CopyBackup'
    { tags = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      sourceBackupId = pSourceBackupId_
    }

-- | Undocumented member.
copyBackup_tags :: Lens.Lens' CopyBackup (Prelude.Maybe (Prelude.NonEmpty Tag))
copyBackup_tags = Lens.lens (\CopyBackup' {tags} -> tags) (\s@CopyBackup' {} a -> s {tags = a} :: CopyBackup) Prelude.. Lens.mapping Lens.coerced

-- | The source Amazon Web Services Region of the backup. Specifies the
-- Amazon Web Services Region from which the backup is being copied. The
-- source and destination Regions must be in the same Amazon Web Services
-- partition. If you don\'t specify a Region, @SourceRegion@ defaults to
-- the Region where the request is sent from (in-Region copy).
copyBackup_sourceRegion :: Lens.Lens' CopyBackup (Prelude.Maybe Prelude.Text)
copyBackup_sourceRegion = Lens.lens (\CopyBackup' {sourceRegion} -> sourceRegion) (\s@CopyBackup' {} a -> s {sourceRegion = a} :: CopyBackup)

-- | Undocumented member.
copyBackup_clientRequestToken :: Lens.Lens' CopyBackup (Prelude.Maybe Prelude.Text)
copyBackup_clientRequestToken = Lens.lens (\CopyBackup' {clientRequestToken} -> clientRequestToken) (\s@CopyBackup' {} a -> s {clientRequestToken = a} :: CopyBackup)

-- | A Boolean flag indicating whether tags from the source backup should be
-- copied to the backup copy. This value defaults to @false@.
--
-- If you set @CopyTags@ to @true@ and the source backup has existing tags,
-- you can use the @Tags@ parameter to create new tags, provided that the
-- sum of the source backup tags and the new tags doesn\'t exceed 50. Both
-- sets of tags are merged. If there are tag conflicts (for example, two
-- tags with the same key but different values), the tags created with the
-- @Tags@ parameter take precedence.
copyBackup_copyTags :: Lens.Lens' CopyBackup (Prelude.Maybe Prelude.Bool)
copyBackup_copyTags = Lens.lens (\CopyBackup' {copyTags} -> copyTags) (\s@CopyBackup' {} a -> s {copyTags = a} :: CopyBackup)

-- | Undocumented member.
copyBackup_kmsKeyId :: Lens.Lens' CopyBackup (Prelude.Maybe Prelude.Text)
copyBackup_kmsKeyId = Lens.lens (\CopyBackup' {kmsKeyId} -> kmsKeyId) (\s@CopyBackup' {} a -> s {kmsKeyId = a} :: CopyBackup)

-- | The ID of the source backup. Specifies the ID of the backup that\'s
-- being copied.
copyBackup_sourceBackupId :: Lens.Lens' CopyBackup Prelude.Text
copyBackup_sourceBackupId = Lens.lens (\CopyBackup' {sourceBackupId} -> sourceBackupId) (\s@CopyBackup' {} a -> s {sourceBackupId = a} :: CopyBackup)

instance Core.AWSRequest CopyBackup where
  type AWSResponse CopyBackup = CopyBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyBackupResponse'
            Prelude.<$> (x Data..?> "Backup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyBackup where
  hashWithSalt _salt CopyBackup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` sourceBackupId

instance Prelude.NFData CopyBackup where
  rnf CopyBackup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf sourceBackupId

instance Data.ToHeaders CopyBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CopyBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CopyBackup where
  toJSON CopyBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("SourceRegion" Data..=) Prelude.<$> sourceRegion,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("CopyTags" Data..=) Prelude.<$> copyTags,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("SourceBackupId" Data..= sourceBackupId)
          ]
      )

instance Data.ToPath CopyBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopyBackupResponse' smart constructor.
data CopyBackupResponse = CopyBackupResponse'
  { backup :: Prelude.Maybe Backup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backup', 'copyBackupResponse_backup' - Undocumented member.
--
-- 'httpStatus', 'copyBackupResponse_httpStatus' - The response's http status code.
newCopyBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyBackupResponse
newCopyBackupResponse pHttpStatus_ =
  CopyBackupResponse'
    { backup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyBackupResponse_backup :: Lens.Lens' CopyBackupResponse (Prelude.Maybe Backup)
copyBackupResponse_backup = Lens.lens (\CopyBackupResponse' {backup} -> backup) (\s@CopyBackupResponse' {} a -> s {backup = a} :: CopyBackupResponse)

-- | The response's http status code.
copyBackupResponse_httpStatus :: Lens.Lens' CopyBackupResponse Prelude.Int
copyBackupResponse_httpStatus = Lens.lens (\CopyBackupResponse' {httpStatus} -> httpStatus) (\s@CopyBackupResponse' {} a -> s {httpStatus = a} :: CopyBackupResponse)

instance Prelude.NFData CopyBackupResponse where
  rnf CopyBackupResponse' {..} =
    Prelude.rnf backup
      `Prelude.seq` Prelude.rnf httpStatus
