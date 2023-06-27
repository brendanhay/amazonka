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
-- Module      : Amazonka.FSx.DescribeBackups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon FSx backup, if a
-- @BackupIds@ value is provided for that backup. Otherwise, it returns all
-- backups owned by your Amazon Web Services account in the Amazon Web
-- Services Region of the endpoint that you\'re calling.
--
-- When retrieving all backups, you can optionally specify the @MaxResults@
-- parameter to limit the number of backups in a response. If more backups
-- remain, Amazon FSx returns a @NextToken@ value in the response. In this
-- case, send a later request with the @NextToken@ request parameter set to
-- the value of the @NextToken@ value from the last response.
--
-- This operation is used in an iterative process to retrieve a list of
-- your backups. @DescribeBackups@ is called first without a @NextToken@
-- value. Then the operation continues to be called with the @NextToken@
-- parameter set to the value of the last @NextToken@ value until a
-- response has no @NextToken@ value.
--
-- When using this operation, keep the following in mind:
--
-- -   The operation might return fewer than the @MaxResults@ value of
--     backup descriptions while still including a @NextToken@ value.
--
-- -   The order of the backups returned in the response of one
--     @DescribeBackups@ call and the order of the backups returned across
--     the responses of a multi-call iteration is unspecified.
--
-- This operation returns paginated results.
module Amazonka.FSx.DescribeBackups
  ( -- * Creating a Request
    DescribeBackups (..),
    newDescribeBackups,

    -- * Request Lenses
    describeBackups_backupIds,
    describeBackups_filters,
    describeBackups_maxResults,
    describeBackups_nextToken,

    -- * Destructuring the Response
    DescribeBackupsResponse (..),
    newDescribeBackupsResponse,

    -- * Response Lenses
    describeBackupsResponse_backups,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for the @DescribeBackups@ operation.
--
-- /See:/ 'newDescribeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { -- | The IDs of the backups that you want to retrieve. This parameter value
    -- overrides any filters. If any IDs aren\'t found, a @BackupNotFound@
    -- error occurs.
    backupIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters structure. The supported names are @file-system-id@,
    -- @backup-type@, @file-system-type@, and @volume-id@.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of backups to return in the response. This parameter
    -- value must be greater than 0. The number of items that Amazon FSx
    -- returns is the minimum of the @MaxResults@ parameter specified in the
    -- request and the service\'s internal maximum number of items per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque pagination token returned from a previous @DescribeBackups@
    -- operation. If a token is present, the operation continues the list from
    -- where the returning call left off.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'backupIds', 'describeBackups_backupIds' - The IDs of the backups that you want to retrieve. This parameter value
-- overrides any filters. If any IDs aren\'t found, a @BackupNotFound@
-- error occurs.
--
-- 'filters', 'describeBackups_filters' - The filters structure. The supported names are @file-system-id@,
-- @backup-type@, @file-system-type@, and @volume-id@.
--
-- 'maxResults', 'describeBackups_maxResults' - Maximum number of backups to return in the response. This parameter
-- value must be greater than 0. The number of items that Amazon FSx
-- returns is the minimum of the @MaxResults@ parameter specified in the
-- request and the service\'s internal maximum number of items per page.
--
-- 'nextToken', 'describeBackups_nextToken' - An opaque pagination token returned from a previous @DescribeBackups@
-- operation. If a token is present, the operation continues the list from
-- where the returning call left off.
newDescribeBackups ::
  DescribeBackups
newDescribeBackups =
  DescribeBackups'
    { backupIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The IDs of the backups that you want to retrieve. This parameter value
-- overrides any filters. If any IDs aren\'t found, a @BackupNotFound@
-- error occurs.
describeBackups_backupIds :: Lens.Lens' DescribeBackups (Prelude.Maybe [Prelude.Text])
describeBackups_backupIds = Lens.lens (\DescribeBackups' {backupIds} -> backupIds) (\s@DescribeBackups' {} a -> s {backupIds = a} :: DescribeBackups) Prelude.. Lens.mapping Lens.coerced

-- | The filters structure. The supported names are @file-system-id@,
-- @backup-type@, @file-system-type@, and @volume-id@.
describeBackups_filters :: Lens.Lens' DescribeBackups (Prelude.Maybe [Filter])
describeBackups_filters = Lens.lens (\DescribeBackups' {filters} -> filters) (\s@DescribeBackups' {} a -> s {filters = a} :: DescribeBackups) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of backups to return in the response. This parameter
-- value must be greater than 0. The number of items that Amazon FSx
-- returns is the minimum of the @MaxResults@ parameter specified in the
-- request and the service\'s internal maximum number of items per page.
describeBackups_maxResults :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Natural)
describeBackups_maxResults = Lens.lens (\DescribeBackups' {maxResults} -> maxResults) (\s@DescribeBackups' {} a -> s {maxResults = a} :: DescribeBackups)

-- | An opaque pagination token returned from a previous @DescribeBackups@
-- operation. If a token is present, the operation continues the list from
-- where the returning call left off.
describeBackups_nextToken :: Lens.Lens' DescribeBackups (Prelude.Maybe Prelude.Text)
describeBackups_nextToken = Lens.lens (\DescribeBackups' {nextToken} -> nextToken) (\s@DescribeBackups' {} a -> s {nextToken = a} :: DescribeBackups)

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
            Lens.^? describeBackupsResponse_backups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeBackups_nextToken
          Lens..~ rs
          Lens.^? describeBackupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeBackups where
  type
    AWSResponse DescribeBackups =
      DescribeBackupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBackupsResponse'
            Prelude.<$> (x Data..?> "Backups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBackups where
  hashWithSalt _salt DescribeBackups' {..} =
    _salt
      `Prelude.hashWithSalt` backupIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeBackups where
  rnf DescribeBackups' {..} =
    Prelude.rnf backupIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DescribeBackups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBackups where
  toJSON DescribeBackups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupIds" Data..=) Prelude.<$> backupIds,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeBackups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBackups where
  toQuery = Prelude.const Prelude.mempty

-- | Response object for the @DescribeBackups@ operation.
--
-- /See:/ 'newDescribeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { -- | An array of backups.
    backups :: Prelude.Maybe [Backup],
    -- | A @NextToken@ value is present if there are more backups than returned
    -- in the response. You can use the @NextToken@ value in the subsequent
    -- request to fetch the backups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backups', 'describeBackupsResponse_backups' - An array of backups.
--
-- 'nextToken', 'describeBackupsResponse_nextToken' - A @NextToken@ value is present if there are more backups than returned
-- in the response. You can use the @NextToken@ value in the subsequent
-- request to fetch the backups.
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

-- | An array of backups.
describeBackupsResponse_backups :: Lens.Lens' DescribeBackupsResponse (Prelude.Maybe [Backup])
describeBackupsResponse_backups = Lens.lens (\DescribeBackupsResponse' {backups} -> backups) (\s@DescribeBackupsResponse' {} a -> s {backups = a} :: DescribeBackupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A @NextToken@ value is present if there are more backups than returned
-- in the response. You can use the @NextToken@ value in the subsequent
-- request to fetch the backups.
describeBackupsResponse_nextToken :: Lens.Lens' DescribeBackupsResponse (Prelude.Maybe Prelude.Text)
describeBackupsResponse_nextToken = Lens.lens (\DescribeBackupsResponse' {nextToken} -> nextToken) (\s@DescribeBackupsResponse' {} a -> s {nextToken = a} :: DescribeBackupsResponse)

-- | The response's http status code.
describeBackupsResponse_httpStatus :: Lens.Lens' DescribeBackupsResponse Prelude.Int
describeBackupsResponse_httpStatus = Lens.lens (\DescribeBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeBackupsResponse' {} a -> s {httpStatus = a} :: DescribeBackupsResponse)

instance Prelude.NFData DescribeBackupsResponse where
  rnf DescribeBackupsResponse' {..} =
    Prelude.rnf backups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
