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
-- Module      : Amazonka.Backup.StopBackupJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel a job to create a one-time backup of a resource.
--
-- This action is not supported for the following services: Amazon FSx for
-- Windows File Server, Amazon FSx for Lustre, FSx for ONTAP , Amazon FSx
-- for OpenZFS, Amazon DocumentDB (with MongoDB compatibility), Amazon RDS,
-- Amazon Aurora, and Amazon Neptune.
module Amazonka.Backup.StopBackupJob
  ( -- * Creating a Request
    StopBackupJob (..),
    newStopBackupJob,

    -- * Request Lenses
    stopBackupJob_backupJobId,

    -- * Destructuring the Response
    StopBackupJobResponse (..),
    newStopBackupJobResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopBackupJob' smart constructor.
data StopBackupJob = StopBackupJob'
  { -- | Uniquely identifies a request to Backup to back up a resource.
    backupJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBackupJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupJobId', 'stopBackupJob_backupJobId' - Uniquely identifies a request to Backup to back up a resource.
newStopBackupJob ::
  -- | 'backupJobId'
  Prelude.Text ->
  StopBackupJob
newStopBackupJob pBackupJobId_ =
  StopBackupJob' {backupJobId = pBackupJobId_}

-- | Uniquely identifies a request to Backup to back up a resource.
stopBackupJob_backupJobId :: Lens.Lens' StopBackupJob Prelude.Text
stopBackupJob_backupJobId = Lens.lens (\StopBackupJob' {backupJobId} -> backupJobId) (\s@StopBackupJob' {} a -> s {backupJobId = a} :: StopBackupJob)

instance Core.AWSRequest StopBackupJob where
  type
    AWSResponse StopBackupJob =
      StopBackupJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopBackupJobResponse'

instance Prelude.Hashable StopBackupJob where
  hashWithSalt _salt StopBackupJob' {..} =
    _salt `Prelude.hashWithSalt` backupJobId

instance Prelude.NFData StopBackupJob where
  rnf StopBackupJob' {..} = Prelude.rnf backupJobId

instance Data.ToHeaders StopBackupJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopBackupJob where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopBackupJob where
  toPath StopBackupJob' {..} =
    Prelude.mconcat
      ["/backup-jobs/", Data.toBS backupJobId]

instance Data.ToQuery StopBackupJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopBackupJobResponse' smart constructor.
data StopBackupJobResponse = StopBackupJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBackupJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopBackupJobResponse ::
  StopBackupJobResponse
newStopBackupJobResponse = StopBackupJobResponse'

instance Prelude.NFData StopBackupJobResponse where
  rnf _ = ()
