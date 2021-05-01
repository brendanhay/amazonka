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
-- Module      : Network.AWS.DynamoDB.DescribeBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing backup of a table.
--
-- You can call @DescribeBackup@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DescribeBackup
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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBackup' smart constructor.
data DescribeBackup = DescribeBackup'
  { -- | The Amazon Resource Name (ARN) associated with the backup.
    backupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeBackup where
  type Rs DescribeBackup = DescribeBackupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupResponse'
            Prelude.<$> (x Prelude..?> "BackupDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackup

instance Prelude.NFData DescribeBackup

instance Prelude.ToHeaders DescribeBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.DescribeBackup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeBackup where
  toJSON DescribeBackup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupArn" Prelude..= backupArn)]
      )

instance Prelude.ToPath DescribeBackup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBackupResponse' smart constructor.
data DescribeBackupResponse = DescribeBackupResponse'
  { -- | Contains the description of the backup created for the table.
    backupDescription :: Prelude.Maybe BackupDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DescribeBackupResponse
