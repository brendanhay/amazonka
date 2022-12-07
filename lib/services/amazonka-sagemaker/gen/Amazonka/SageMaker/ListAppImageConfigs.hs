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
-- Module      : Amazonka.SageMaker.ListAppImageConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AppImageConfigs in your account and their properties. The list
-- can be filtered by creation time or modified time, and whether the
-- AppImageConfig name contains a specified string.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListAppImageConfigs
  ( -- * Creating a Request
    ListAppImageConfigs (..),
    newListAppImageConfigs,

    -- * Request Lenses
    listAppImageConfigs_sortOrder,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_nameContains,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_sortBy,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_modifiedTimeAfter,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_creationTimeAfter,

    -- * Destructuring the Response
    ListAppImageConfigsResponse (..),
    newListAppImageConfigsResponse,

    -- * Response Lenses
    listAppImageConfigsResponse_nextToken,
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListAppImageConfigs' smart constructor.
data ListAppImageConfigs = ListAppImageConfigs'
  { -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the previous call to @ListImages@ didn\'t return the full set of
    -- AppImageConfigs, the call returns a token for getting the next set of
    -- AppImageConfigs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only AppImageConfigs whose name contains the
    -- specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only AppImageConfigs created on or before the
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe AppImageConfigSortKey,
    -- | A filter that returns only AppImageConfigs modified on or before the
    -- specified time.
    modifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only AppImageConfigs modified on or after the
    -- specified time.
    modifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of AppImageConfigs to return in the response. The
    -- default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only AppImageConfigs created on or after the
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppImageConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listAppImageConfigs_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listAppImageConfigs_nextToken' - If the previous call to @ListImages@ didn\'t return the full set of
-- AppImageConfigs, the call returns a token for getting the next set of
-- AppImageConfigs.
--
-- 'nameContains', 'listAppImageConfigs_nameContains' - A filter that returns only AppImageConfigs whose name contains the
-- specified string.
--
-- 'creationTimeBefore', 'listAppImageConfigs_creationTimeBefore' - A filter that returns only AppImageConfigs created on or before the
-- specified time.
--
-- 'sortBy', 'listAppImageConfigs_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'modifiedTimeBefore', 'listAppImageConfigs_modifiedTimeBefore' - A filter that returns only AppImageConfigs modified on or before the
-- specified time.
--
-- 'modifiedTimeAfter', 'listAppImageConfigs_modifiedTimeAfter' - A filter that returns only AppImageConfigs modified on or after the
-- specified time.
--
-- 'maxResults', 'listAppImageConfigs_maxResults' - The maximum number of AppImageConfigs to return in the response. The
-- default value is 10.
--
-- 'creationTimeAfter', 'listAppImageConfigs_creationTimeAfter' - A filter that returns only AppImageConfigs created on or after the
-- specified time.
newListAppImageConfigs ::
  ListAppImageConfigs
newListAppImageConfigs =
  ListAppImageConfigs'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      modifiedTimeBefore = Prelude.Nothing,
      modifiedTimeAfter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The sort order. The default value is @Descending@.
listAppImageConfigs_sortOrder :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe SortOrder)
listAppImageConfigs_sortOrder = Lens.lens (\ListAppImageConfigs' {sortOrder} -> sortOrder) (\s@ListAppImageConfigs' {} a -> s {sortOrder = a} :: ListAppImageConfigs)

-- | If the previous call to @ListImages@ didn\'t return the full set of
-- AppImageConfigs, the call returns a token for getting the next set of
-- AppImageConfigs.
listAppImageConfigs_nextToken :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.Text)
listAppImageConfigs_nextToken = Lens.lens (\ListAppImageConfigs' {nextToken} -> nextToken) (\s@ListAppImageConfigs' {} a -> s {nextToken = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs whose name contains the
-- specified string.
listAppImageConfigs_nameContains :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.Text)
listAppImageConfigs_nameContains = Lens.lens (\ListAppImageConfigs' {nameContains} -> nameContains) (\s@ListAppImageConfigs' {} a -> s {nameContains = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs created on or before the
-- specified time.
listAppImageConfigs_creationTimeBefore :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_creationTimeBefore = Lens.lens (\ListAppImageConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListAppImageConfigs' {} a -> s {creationTimeBefore = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | The property used to sort results. The default value is @CreationTime@.
listAppImageConfigs_sortBy :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe AppImageConfigSortKey)
listAppImageConfigs_sortBy = Lens.lens (\ListAppImageConfigs' {sortBy} -> sortBy) (\s@ListAppImageConfigs' {} a -> s {sortBy = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs modified on or before the
-- specified time.
listAppImageConfigs_modifiedTimeBefore :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_modifiedTimeBefore = Lens.lens (\ListAppImageConfigs' {modifiedTimeBefore} -> modifiedTimeBefore) (\s@ListAppImageConfigs' {} a -> s {modifiedTimeBefore = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only AppImageConfigs modified on or after the
-- specified time.
listAppImageConfigs_modifiedTimeAfter :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_modifiedTimeAfter = Lens.lens (\ListAppImageConfigs' {modifiedTimeAfter} -> modifiedTimeAfter) (\s@ListAppImageConfigs' {} a -> s {modifiedTimeAfter = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of AppImageConfigs to return in the response. The
-- default value is 10.
listAppImageConfigs_maxResults :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.Natural)
listAppImageConfigs_maxResults = Lens.lens (\ListAppImageConfigs' {maxResults} -> maxResults) (\s@ListAppImageConfigs' {} a -> s {maxResults = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs created on or after the
-- specified time.
listAppImageConfigs_creationTimeAfter :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_creationTimeAfter = Lens.lens (\ListAppImageConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListAppImageConfigs' {} a -> s {creationTimeAfter = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager ListAppImageConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppImageConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppImageConfigsResponse_appImageConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAppImageConfigs_nextToken
          Lens..~ rs
          Lens.^? listAppImageConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAppImageConfigs where
  type
    AWSResponse ListAppImageConfigs =
      ListAppImageConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppImageConfigsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "AppImageConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppImageConfigs where
  hashWithSalt _salt ListAppImageConfigs' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` modifiedTimeBefore
      `Prelude.hashWithSalt` modifiedTimeAfter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListAppImageConfigs where
  rnf ListAppImageConfigs' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf modifiedTimeBefore
      `Prelude.seq` Prelude.rnf modifiedTimeAfter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Data.ToHeaders ListAppImageConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListAppImageConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppImageConfigs where
  toJSON ListAppImageConfigs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("ModifiedTimeBefore" Data..=)
              Prelude.<$> modifiedTimeBefore,
            ("ModifiedTimeAfter" Data..=)
              Prelude.<$> modifiedTimeAfter,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Data.ToPath ListAppImageConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAppImageConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppImageConfigsResponse' smart constructor.
data ListAppImageConfigsResponse = ListAppImageConfigsResponse'
  { -- | A token for getting the next set of AppImageConfigs, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of AppImageConfigs and their properties.
    appImageConfigs :: Prelude.Maybe [AppImageConfigDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppImageConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppImageConfigsResponse_nextToken' - A token for getting the next set of AppImageConfigs, if there are any.
--
-- 'appImageConfigs', 'listAppImageConfigsResponse_appImageConfigs' - A list of AppImageConfigs and their properties.
--
-- 'httpStatus', 'listAppImageConfigsResponse_httpStatus' - The response's http status code.
newListAppImageConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppImageConfigsResponse
newListAppImageConfigsResponse pHttpStatus_ =
  ListAppImageConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      appImageConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of AppImageConfigs, if there are any.
listAppImageConfigsResponse_nextToken :: Lens.Lens' ListAppImageConfigsResponse (Prelude.Maybe Prelude.Text)
listAppImageConfigsResponse_nextToken = Lens.lens (\ListAppImageConfigsResponse' {nextToken} -> nextToken) (\s@ListAppImageConfigsResponse' {} a -> s {nextToken = a} :: ListAppImageConfigsResponse)

-- | A list of AppImageConfigs and their properties.
listAppImageConfigsResponse_appImageConfigs :: Lens.Lens' ListAppImageConfigsResponse (Prelude.Maybe [AppImageConfigDetails])
listAppImageConfigsResponse_appImageConfigs = Lens.lens (\ListAppImageConfigsResponse' {appImageConfigs} -> appImageConfigs) (\s@ListAppImageConfigsResponse' {} a -> s {appImageConfigs = a} :: ListAppImageConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAppImageConfigsResponse_httpStatus :: Lens.Lens' ListAppImageConfigsResponse Prelude.Int
listAppImageConfigsResponse_httpStatus = Lens.lens (\ListAppImageConfigsResponse' {httpStatus} -> httpStatus) (\s@ListAppImageConfigsResponse' {} a -> s {httpStatus = a} :: ListAppImageConfigsResponse)

instance Prelude.NFData ListAppImageConfigsResponse where
  rnf ListAppImageConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appImageConfigs
      `Prelude.seq` Prelude.rnf httpStatus
