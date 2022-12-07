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
-- Module      : Amazonka.EFS.DescribeBackupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup policy for the specified EFS file system.
module Amazonka.EFS.DescribeBackupPolicy
  ( -- * Creating a Request
    DescribeBackupPolicy (..),
    newDescribeBackupPolicy,

    -- * Request Lenses
    describeBackupPolicy_fileSystemId,

    -- * Destructuring the Response
    BackupPolicyDescription (..),
    newBackupPolicyDescription,

    -- * Response Lenses
    backupPolicyDescription_backupPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBackupPolicy' smart constructor.
data DescribeBackupPolicy = DescribeBackupPolicy'
  { -- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'describeBackupPolicy_fileSystemId' - Specifies which EFS file system to retrieve the @BackupPolicy@ for.
newDescribeBackupPolicy ::
  -- | 'fileSystemId'
  Prelude.Text ->
  DescribeBackupPolicy
newDescribeBackupPolicy pFileSystemId_ =
  DescribeBackupPolicy'
    { fileSystemId =
        pFileSystemId_
    }

-- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
describeBackupPolicy_fileSystemId :: Lens.Lens' DescribeBackupPolicy Prelude.Text
describeBackupPolicy_fileSystemId = Lens.lens (\DescribeBackupPolicy' {fileSystemId} -> fileSystemId) (\s@DescribeBackupPolicy' {} a -> s {fileSystemId = a} :: DescribeBackupPolicy)

instance Core.AWSRequest DescribeBackupPolicy where
  type
    AWSResponse DescribeBackupPolicy =
      BackupPolicyDescription
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeBackupPolicy where
  hashWithSalt _salt DescribeBackupPolicy' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData DescribeBackupPolicy where
  rnf DescribeBackupPolicy' {..} =
    Prelude.rnf fileSystemId

instance Data.ToHeaders DescribeBackupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeBackupPolicy where
  toPath DescribeBackupPolicy' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Data.toBS fileSystemId,
        "/backup-policy"
      ]

instance Data.ToQuery DescribeBackupPolicy where
  toQuery = Prelude.const Prelude.mempty
