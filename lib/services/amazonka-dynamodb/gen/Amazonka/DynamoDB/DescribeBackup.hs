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
-- Module      : Amazonka.DynamoDB.DescribeBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing backup of a table.
--
-- You can call @DescribeBackup@ at a maximum rate of 10 times per second.
module Amazonka.DynamoDB.DescribeBackup
  ( -- * Creating a Request
    DescribeBackup (..),
    newDescribeBackup,

    -- * Request Lenses
    describeBackup_backupArn,

    -- * Destructuring the Response
    DescribeBackupResponse (..),
    newDescribeBackupResponse,

    -- * Response Lenses
    describeBackupResponse_backupDescription,
    describeBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBackup' smart constructor.
data DescribeBackup = DescribeBackup'
  { -- | The Amazon Resource Name (ARN) associated with the backup.
    backupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupArn', 'describeBackup_backupArn' - The Amazon Resource Name (ARN) associated with the backup.
newDescribeBackup ::
  -- | 'backupArn'
  Prelude.Text ->
  DescribeBackup
newDescribeBackup pBackupArn_ =
  DescribeBackup' {backupArn = pBackupArn_}

-- | The Amazon Resource Name (ARN) associated with the backup.
describeBackup_backupArn :: Lens.Lens' DescribeBackup Prelude.Text
describeBackup_backupArn = Lens.lens (\DescribeBackup' {backupArn} -> backupArn) (\s@DescribeBackup' {} a -> s {backupArn = a} :: DescribeBackup)

instance Core.AWSRequest DescribeBackup where
  type
    AWSResponse DescribeBackup =
      DescribeBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupResponse'
            Prelude.<$> (x Data..?> "BackupDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackup where
  hashWithSalt _salt DescribeBackup' {..} =
    _salt `Prelude.hashWithSalt` backupArn

instance Prelude.NFData DescribeBackup where
  rnf DescribeBackup' {..} = Prelude.rnf backupArn

instance Data.ToHeaders DescribeBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBackup where
  toJSON DescribeBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupArn" Data..= backupArn)]
      )

instance Data.ToPath DescribeBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBackupResponse' smart constructor.
data DescribeBackupResponse = DescribeBackupResponse'
  { -- | Contains the description of the backup created for the table.
    backupDescription :: Prelude.Maybe BackupDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupDescription', 'describeBackupResponse_backupDescription' - Contains the description of the backup created for the table.
--
-- 'httpStatus', 'describeBackupResponse_httpStatus' - The response's http status code.
newDescribeBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBackupResponse
newDescribeBackupResponse pHttpStatus_ =
  DescribeBackupResponse'
    { backupDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the description of the backup created for the table.
describeBackupResponse_backupDescription :: Lens.Lens' DescribeBackupResponse (Prelude.Maybe BackupDescription)
describeBackupResponse_backupDescription = Lens.lens (\DescribeBackupResponse' {backupDescription} -> backupDescription) (\s@DescribeBackupResponse' {} a -> s {backupDescription = a} :: DescribeBackupResponse)

-- | The response's http status code.
describeBackupResponse_httpStatus :: Lens.Lens' DescribeBackupResponse Prelude.Int
describeBackupResponse_httpStatus = Lens.lens (\DescribeBackupResponse' {httpStatus} -> httpStatus) (\s@DescribeBackupResponse' {} a -> s {httpStatus = a} :: DescribeBackupResponse)

instance Prelude.NFData DescribeBackupResponse where
  rnf DescribeBackupResponse' {..} =
    Prelude.rnf backupDescription
      `Prelude.seq` Prelude.rnf httpStatus
