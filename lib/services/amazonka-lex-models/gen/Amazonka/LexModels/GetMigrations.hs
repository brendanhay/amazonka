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
-- Module      : Amazonka.LexModels.GetMigrations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of migrations between Amazon Lex V1 and Amazon Lex V2.
module Amazonka.LexModels.GetMigrations
  ( -- * Creating a Request
    GetMigrations (..),
    newGetMigrations,

    -- * Request Lenses
    getMigrations_sortByOrder,
    getMigrations_sortByAttribute,
    getMigrations_nextToken,
    getMigrations_migrationStatusEquals,
    getMigrations_v1BotNameContains,
    getMigrations_maxResults,

    -- * Destructuring the Response
    GetMigrationsResponse (..),
    newGetMigrationsResponse,

    -- * Response Lenses
    getMigrationsResponse_migrationSummaries,
    getMigrationsResponse_nextToken,
    getMigrationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMigrations' smart constructor.
data GetMigrations = GetMigrations'
  { -- | The order so sort the list.
    sortByOrder :: Prelude.Maybe SortOrder,
    -- | The field to sort the list of migrations by. You can sort by the Amazon
    -- Lex V1 bot name or the date and time that the migration was started.
    sortByAttribute :: Prelude.Maybe MigrationSortAttribute,
    -- | A pagination token that fetches the next page of migrations. If the
    -- response to this operation is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of migrations, specify the
    -- pagination token in the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the list to contain only migrations in the specified state.
    migrationStatusEquals :: Prelude.Maybe MigrationStatus,
    -- | Filters the list to contain only bots whose name contains the specified
    -- string. The string is matched anywhere in bot name.
    v1BotNameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of migrations to return in the response. The default
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMigrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortByOrder', 'getMigrations_sortByOrder' - The order so sort the list.
--
-- 'sortByAttribute', 'getMigrations_sortByAttribute' - The field to sort the list of migrations by. You can sort by the Amazon
-- Lex V1 bot name or the date and time that the migration was started.
--
-- 'nextToken', 'getMigrations_nextToken' - A pagination token that fetches the next page of migrations. If the
-- response to this operation is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of migrations, specify the
-- pagination token in the request.
--
-- 'migrationStatusEquals', 'getMigrations_migrationStatusEquals' - Filters the list to contain only migrations in the specified state.
--
-- 'v1BotNameContains', 'getMigrations_v1BotNameContains' - Filters the list to contain only bots whose name contains the specified
-- string. The string is matched anywhere in bot name.
--
-- 'maxResults', 'getMigrations_maxResults' - The maximum number of migrations to return in the response. The default
-- is 10.
newGetMigrations ::
  GetMigrations
newGetMigrations =
  GetMigrations'
    { sortByOrder = Prelude.Nothing,
      sortByAttribute = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      migrationStatusEquals = Prelude.Nothing,
      v1BotNameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The order so sort the list.
getMigrations_sortByOrder :: Lens.Lens' GetMigrations (Prelude.Maybe SortOrder)
getMigrations_sortByOrder = Lens.lens (\GetMigrations' {sortByOrder} -> sortByOrder) (\s@GetMigrations' {} a -> s {sortByOrder = a} :: GetMigrations)

-- | The field to sort the list of migrations by. You can sort by the Amazon
-- Lex V1 bot name or the date and time that the migration was started.
getMigrations_sortByAttribute :: Lens.Lens' GetMigrations (Prelude.Maybe MigrationSortAttribute)
getMigrations_sortByAttribute = Lens.lens (\GetMigrations' {sortByAttribute} -> sortByAttribute) (\s@GetMigrations' {} a -> s {sortByAttribute = a} :: GetMigrations)

-- | A pagination token that fetches the next page of migrations. If the
-- response to this operation is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of migrations, specify the
-- pagination token in the request.
getMigrations_nextToken :: Lens.Lens' GetMigrations (Prelude.Maybe Prelude.Text)
getMigrations_nextToken = Lens.lens (\GetMigrations' {nextToken} -> nextToken) (\s@GetMigrations' {} a -> s {nextToken = a} :: GetMigrations)

-- | Filters the list to contain only migrations in the specified state.
getMigrations_migrationStatusEquals :: Lens.Lens' GetMigrations (Prelude.Maybe MigrationStatus)
getMigrations_migrationStatusEquals = Lens.lens (\GetMigrations' {migrationStatusEquals} -> migrationStatusEquals) (\s@GetMigrations' {} a -> s {migrationStatusEquals = a} :: GetMigrations)

-- | Filters the list to contain only bots whose name contains the specified
-- string. The string is matched anywhere in bot name.
getMigrations_v1BotNameContains :: Lens.Lens' GetMigrations (Prelude.Maybe Prelude.Text)
getMigrations_v1BotNameContains = Lens.lens (\GetMigrations' {v1BotNameContains} -> v1BotNameContains) (\s@GetMigrations' {} a -> s {v1BotNameContains = a} :: GetMigrations)

-- | The maximum number of migrations to return in the response. The default
-- is 10.
getMigrations_maxResults :: Lens.Lens' GetMigrations (Prelude.Maybe Prelude.Natural)
getMigrations_maxResults = Lens.lens (\GetMigrations' {maxResults} -> maxResults) (\s@GetMigrations' {} a -> s {maxResults = a} :: GetMigrations)

instance Core.AWSRequest GetMigrations where
  type
    AWSResponse GetMigrations =
      GetMigrationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMigrationsResponse'
            Prelude.<$> ( x Core..?> "migrationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMigrations

instance Prelude.NFData GetMigrations

instance Core.ToHeaders GetMigrations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetMigrations where
  toPath = Prelude.const "/migrations"

instance Core.ToQuery GetMigrations where
  toQuery GetMigrations' {..} =
    Prelude.mconcat
      [ "sortByOrder" Core.=: sortByOrder,
        "sortByAttribute" Core.=: sortByAttribute,
        "nextToken" Core.=: nextToken,
        "migrationStatusEquals"
          Core.=: migrationStatusEquals,
        "v1BotNameContains" Core.=: v1BotNameContains,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetMigrationsResponse' smart constructor.
data GetMigrationsResponse = GetMigrationsResponse'
  { -- | An array of summaries for migrations from Amazon Lex V1 to Amazon Lex
    -- V2. To see details of the migration, use the @migrationId@ from the
    -- summary in a call to the operation.
    migrationSummaries :: Prelude.Maybe [MigrationSummary],
    -- | If the response is truncated, it includes a pagination token that you
    -- can specify in your next request to fetch the next page of migrations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMigrationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationSummaries', 'getMigrationsResponse_migrationSummaries' - An array of summaries for migrations from Amazon Lex V1 to Amazon Lex
-- V2. To see details of the migration, use the @migrationId@ from the
-- summary in a call to the operation.
--
-- 'nextToken', 'getMigrationsResponse_nextToken' - If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of migrations.
--
-- 'httpStatus', 'getMigrationsResponse_httpStatus' - The response's http status code.
newGetMigrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMigrationsResponse
newGetMigrationsResponse pHttpStatus_ =
  GetMigrationsResponse'
    { migrationSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of summaries for migrations from Amazon Lex V1 to Amazon Lex
-- V2. To see details of the migration, use the @migrationId@ from the
-- summary in a call to the operation.
getMigrationsResponse_migrationSummaries :: Lens.Lens' GetMigrationsResponse (Prelude.Maybe [MigrationSummary])
getMigrationsResponse_migrationSummaries = Lens.lens (\GetMigrationsResponse' {migrationSummaries} -> migrationSummaries) (\s@GetMigrationsResponse' {} a -> s {migrationSummaries = a} :: GetMigrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of migrations.
getMigrationsResponse_nextToken :: Lens.Lens' GetMigrationsResponse (Prelude.Maybe Prelude.Text)
getMigrationsResponse_nextToken = Lens.lens (\GetMigrationsResponse' {nextToken} -> nextToken) (\s@GetMigrationsResponse' {} a -> s {nextToken = a} :: GetMigrationsResponse)

-- | The response's http status code.
getMigrationsResponse_httpStatus :: Lens.Lens' GetMigrationsResponse Prelude.Int
getMigrationsResponse_httpStatus = Lens.lens (\GetMigrationsResponse' {httpStatus} -> httpStatus) (\s@GetMigrationsResponse' {} a -> s {httpStatus = a} :: GetMigrationsResponse)

instance Prelude.NFData GetMigrationsResponse
