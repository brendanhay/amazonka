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
-- Module      : Network.AWS.OpsWorksCM.DescribeBackups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes backups. The results are ordered by time, with newest backups
-- first. If you do not specify a BackupId or ServerName, the command
-- returns all backups.
--
-- This operation is synchronous.
--
-- A @ResourceNotFoundException@ is thrown when the backup does not exist.
-- A @ValidationException@ is raised when parameters of the request are not
-- valid.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeBackups
  ( -- * Creating a Request
    DescribeBackups (..),
    newDescribeBackups,

    -- * Request Lenses
    describeBackups_nextToken,
    describeBackups_maxResults,
    describeBackups_backupId,
    describeBackups_serverName,

    -- * Destructuring the Response
    DescribeBackupsResponse (..),
    newDescribeBackupsResponse,

    -- * Response Lenses
    describeBackupsResponse_nextToken,
    describeBackupsResponse_backups,
    describeBackupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { -- | This is not currently implemented for @DescribeBackups@ requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is not currently implemented for @DescribeBackups@ requests.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Describes a single backup.
    backupId :: Prelude.Maybe Prelude.Text,
    -- | Returns backups for the server with the specified ServerName.
    serverName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBackups_nextToken' - This is not currently implemented for @DescribeBackups@ requests.
--
-- 'maxResults', 'describeBackups_maxResults' - This is not currently implemented for @DescribeBackups@ requests.
--
-- 'backupId', 'describeBackups_backupId' - Describes a single backup.
--
-- 'serverName', 'describeBackups_serverName' - Returns backups for the server with the specified ServerName.
newDescribeBackups ::
  DescribeBackups
newDescribeBackups =
  DescribeBackups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      backupId = Prelude.Nothing,
      serverName = Prelude.Nothing
    }

-- | This is not currently implemented for @DescribeBackups@ requests.
describeBackups_nextToken :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Text)
describeBackups_nextToken = Lens.lens (\DescribeBackups' {nextToken} -> nextToken) (\s@DescribeBackups' {} a -> s {nextToken = a} :: DescribeBackups)

-- | This is not currently implemented for @DescribeBackups@ requests.
describeBackups_maxResults :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Natural)
describeBackups_maxResults = Lens.lens (\DescribeBackups' {maxResults} -> maxResults) (\s@DescribeBackups' {} a -> s {maxResults = a} :: DescribeBackups)

-- | Describes a single backup.
describeBackups_backupId :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Text)
describeBackups_backupId = Lens.lens (\DescribeBackups' {backupId} -> backupId) (\s@DescribeBackups' {} a -> s {backupId = a} :: DescribeBackups)

-- | Returns backups for the server with the specified ServerName.
describeBackups_serverName :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Text)
describeBackups_serverName = Lens.lens (\DescribeBackups' {serverName} -> serverName) (\s@DescribeBackups' {} a -> s {serverName = a} :: DescribeBackups)

instance Core.AWSPager DescribeBackups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBackupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBackupsResponse_backups Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBackups_nextToken
          Lens..~ rs
          Lens.^? describeBackupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeBackups where
  type
    AWSResponse DescribeBackups =
      DescribeBackupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Backups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackups

instance Prelude.NFData DescribeBackups

instance Core.ToHeaders DescribeBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.DescribeBackups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeBackups where
  toJSON DescribeBackups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("BackupId" Core..=) Prelude.<$> backupId,
            ("ServerName" Core..=) Prelude.<$> serverName
          ]
      )

instance Core.ToPath DescribeBackups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { -- | This is not currently implemented for @DescribeBackups@ requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains the response to a @DescribeBackups@ request.
    backups :: Prelude.Maybe [Backup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBackupsResponse_nextToken' - This is not currently implemented for @DescribeBackups@ requests.
--
-- 'backups', 'describeBackupsResponse_backups' - Contains the response to a @DescribeBackups@ request.
--
-- 'httpStatus', 'describeBackupsResponse_httpStatus' - The response's http status code.
newDescribeBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBackupsResponse
newDescribeBackupsResponse pHttpStatus_ =
  DescribeBackupsResponse'
    { nextToken =
        Prelude.Nothing,
      backups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is not currently implemented for @DescribeBackups@ requests.
describeBackupsResponse_nextToken :: Lens.Lens' DescribeBackupsResponse (Prelude.Maybe Prelude.Text)
describeBackupsResponse_nextToken = Lens.lens (\DescribeBackupsResponse' {nextToken} -> nextToken) (\s@DescribeBackupsResponse' {} a -> s {nextToken = a} :: DescribeBackupsResponse)

-- | Contains the response to a @DescribeBackups@ request.
describeBackupsResponse_backups :: Lens.Lens' DescribeBackupsResponse (Prelude.Maybe [Backup])
describeBackupsResponse_backups = Lens.lens (\DescribeBackupsResponse' {backups} -> backups) (\s@DescribeBackupsResponse' {} a -> s {backups = a} :: DescribeBackupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeBackupsResponse_httpStatus :: Lens.Lens' DescribeBackupsResponse Prelude.Int
describeBackupsResponse_httpStatus = Lens.lens (\DescribeBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeBackupsResponse' {} a -> s {httpStatus = a} :: DescribeBackupsResponse)

instance Prelude.NFData DescribeBackupsResponse
