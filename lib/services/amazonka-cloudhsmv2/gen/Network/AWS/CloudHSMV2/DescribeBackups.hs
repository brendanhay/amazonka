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
-- Module      : Network.AWS.CloudHSMV2.DescribeBackups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about backups of AWS CloudHSM clusters.
--
-- This is a paginated operation, which means that each response might
-- contain only a subset of all the backups. When the response contains
-- only a subset of backups, it includes a @NextToken@ value. Use this
-- value in a subsequent @DescribeBackups@ request to get more backups.
-- When you receive a response with no @NextToken@ (or an empty or null
-- value), that means there are no more backups to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMV2.DescribeBackups
  ( -- * Creating a Request
    DescribeBackups (..),
    newDescribeBackups,

    -- * Request Lenses
    describeBackups_sortAscending,
    describeBackups_filters,
    describeBackups_nextToken,
    describeBackups_maxResults,

    -- * Destructuring the Response
    DescribeBackupsResponse (..),
    newDescribeBackupsResponse,

    -- * Response Lenses
    describeBackupsResponse_backups,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { -- | Designates whether or not to sort the return backups by ascending
    -- chronological order of generation.
    sortAscending :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters to limit the items returned in the response.
    --
    -- Use the @backupIds@ filter to return only the specified backups. Specify
    -- backups by their backup identifier (ID).
    --
    -- Use the @sourceBackupIds@ filter to return only the backups created from
    -- a source backup. The @sourceBackupID@ of a source backup is returned by
    -- the CopyBackupToRegion operation.
    --
    -- Use the @clusterIds@ filter to return only the backups for the specified
    -- clusters. Specify clusters by their cluster identifier (ID).
    --
    -- Use the @states@ filter to return only backups that match the specified
    -- state.
    --
    -- Use the @neverExpires@ filter to return backups filtered by the value in
    -- the @neverExpires@ parameter. @True@ returns all backups exempt from the
    -- backup retention policy. @False@ returns all backups with a backup
    -- retention policy defined at the cluster.
    filters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The @NextToken@ value that you received in the previous response. Use
    -- this value to get more backups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of backups to return in the response. When there are
    -- more backups than the number you specify, the response contains a
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'sortAscending', 'describeBackups_sortAscending' - Designates whether or not to sort the return backups by ascending
-- chronological order of generation.
--
-- 'filters', 'describeBackups_filters' - One or more filters to limit the items returned in the response.
--
-- Use the @backupIds@ filter to return only the specified backups. Specify
-- backups by their backup identifier (ID).
--
-- Use the @sourceBackupIds@ filter to return only the backups created from
-- a source backup. The @sourceBackupID@ of a source backup is returned by
-- the CopyBackupToRegion operation.
--
-- Use the @clusterIds@ filter to return only the backups for the specified
-- clusters. Specify clusters by their cluster identifier (ID).
--
-- Use the @states@ filter to return only backups that match the specified
-- state.
--
-- Use the @neverExpires@ filter to return backups filtered by the value in
-- the @neverExpires@ parameter. @True@ returns all backups exempt from the
-- backup retention policy. @False@ returns all backups with a backup
-- retention policy defined at the cluster.
--
-- 'nextToken', 'describeBackups_nextToken' - The @NextToken@ value that you received in the previous response. Use
-- this value to get more backups.
--
-- 'maxResults', 'describeBackups_maxResults' - The maximum number of backups to return in the response. When there are
-- more backups than the number you specify, the response contains a
-- @NextToken@ value.
newDescribeBackups ::
  DescribeBackups
newDescribeBackups =
  DescribeBackups'
    { sortAscending = Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Designates whether or not to sort the return backups by ascending
-- chronological order of generation.
describeBackups_sortAscending :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Bool)
describeBackups_sortAscending = Lens.lens (\DescribeBackups' {sortAscending} -> sortAscending) (\s@DescribeBackups' {} a -> s {sortAscending = a} :: DescribeBackups)

-- | One or more filters to limit the items returned in the response.
--
-- Use the @backupIds@ filter to return only the specified backups. Specify
-- backups by their backup identifier (ID).
--
-- Use the @sourceBackupIds@ filter to return only the backups created from
-- a source backup. The @sourceBackupID@ of a source backup is returned by
-- the CopyBackupToRegion operation.
--
-- Use the @clusterIds@ filter to return only the backups for the specified
-- clusters. Specify clusters by their cluster identifier (ID).
--
-- Use the @states@ filter to return only backups that match the specified
-- state.
--
-- Use the @neverExpires@ filter to return backups filtered by the value in
-- the @neverExpires@ parameter. @True@ returns all backups exempt from the
-- backup retention policy. @False@ returns all backups with a backup
-- retention policy defined at the cluster.
describeBackups_filters :: Lens.Lens' DescribeBackups (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
describeBackups_filters = Lens.lens (\DescribeBackups' {filters} -> filters) (\s@DescribeBackups' {} a -> s {filters = a} :: DescribeBackups) Prelude.. Lens.mapping Lens.coerced

-- | The @NextToken@ value that you received in the previous response. Use
-- this value to get more backups.
describeBackups_nextToken :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Text)
describeBackups_nextToken = Lens.lens (\DescribeBackups' {nextToken} -> nextToken) (\s@DescribeBackups' {} a -> s {nextToken = a} :: DescribeBackups)

-- | The maximum number of backups to return in the response. When there are
-- more backups than the number you specify, the response contains a
-- @NextToken@ value.
describeBackups_maxResults :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Natural)
describeBackups_maxResults = Lens.lens (\DescribeBackups' {maxResults} -> maxResults) (\s@DescribeBackups' {} a -> s {maxResults = a} :: DescribeBackups)

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
            Prelude.<$> (x Core..?> "Backups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackups

instance Prelude.NFData DescribeBackups

instance Core.ToHeaders DescribeBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.DescribeBackups" ::
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
          [ ("SortAscending" Core..=) Prelude.<$> sortAscending,
            ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeBackups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { -- | A list of backups.
    backups :: Prelude.Maybe [Backup],
    -- | An opaque string that indicates that the response contains only a subset
    -- of backups. Use this value in a subsequent @DescribeBackups@ request to
    -- get more backups.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'backups', 'describeBackupsResponse_backups' - A list of backups.
--
-- 'nextToken', 'describeBackupsResponse_nextToken' - An opaque string that indicates that the response contains only a subset
-- of backups. Use this value in a subsequent @DescribeBackups@ request to
-- get more backups.
--
-- 'httpStatus', 'describeBackupsResponse_httpStatus' - The response's http status code.
newDescribeBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBackupsResponse
newDescribeBackupsResponse pHttpStatus_ =
  DescribeBackupsResponse'
    { backups = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of backups.
describeBackupsResponse_backups :: Lens.Lens' DescribeBackupsResponse (Prelude.Maybe [Backup])
describeBackupsResponse_backups = Lens.lens (\DescribeBackupsResponse' {backups} -> backups) (\s@DescribeBackupsResponse' {} a -> s {backups = a} :: DescribeBackupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque string that indicates that the response contains only a subset
-- of backups. Use this value in a subsequent @DescribeBackups@ request to
-- get more backups.
describeBackupsResponse_nextToken :: Lens.Lens' DescribeBackupsResponse (Prelude.Maybe Prelude.Text)
describeBackupsResponse_nextToken = Lens.lens (\DescribeBackupsResponse' {nextToken} -> nextToken) (\s@DescribeBackupsResponse' {} a -> s {nextToken = a} :: DescribeBackupsResponse)

-- | The response's http status code.
describeBackupsResponse_httpStatus :: Lens.Lens' DescribeBackupsResponse Prelude.Int
describeBackupsResponse_httpStatus = Lens.lens (\DescribeBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeBackupsResponse' {} a -> s {httpStatus = a} :: DescribeBackupsResponse)

instance Prelude.NFData DescribeBackupsResponse
