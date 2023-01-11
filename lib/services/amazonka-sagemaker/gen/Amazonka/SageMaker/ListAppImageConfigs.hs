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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listAppImageConfigs_creationTimeAfter,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_modifiedTimeAfter,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_nameContains,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_sortBy,
    listAppImageConfigs_sortOrder,

    -- * Destructuring the Response
    ListAppImageConfigsResponse (..),
    newListAppImageConfigsResponse,

    -- * Response Lenses
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_nextToken,
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
  { -- | A filter that returns only AppImageConfigs created on or after the
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only AppImageConfigs created on or before the
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of AppImageConfigs to return in the response. The
    -- default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only AppImageConfigs modified on or after the
    -- specified time.
    modifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only AppImageConfigs modified on or before the
    -- specified time.
    modifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only AppImageConfigs whose name contains the
    -- specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the previous call to @ListImages@ didn\'t return the full set of
    -- AppImageConfigs, the call returns a token for getting the next set of
    -- AppImageConfigs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe AppImageConfigSortKey,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'creationTimeAfter', 'listAppImageConfigs_creationTimeAfter' - A filter that returns only AppImageConfigs created on or after the
-- specified time.
--
-- 'creationTimeBefore', 'listAppImageConfigs_creationTimeBefore' - A filter that returns only AppImageConfigs created on or before the
-- specified time.
--
-- 'maxResults', 'listAppImageConfigs_maxResults' - The maximum number of AppImageConfigs to return in the response. The
-- default value is 10.
--
-- 'modifiedTimeAfter', 'listAppImageConfigs_modifiedTimeAfter' - A filter that returns only AppImageConfigs modified on or after the
-- specified time.
--
-- 'modifiedTimeBefore', 'listAppImageConfigs_modifiedTimeBefore' - A filter that returns only AppImageConfigs modified on or before the
-- specified time.
--
-- 'nameContains', 'listAppImageConfigs_nameContains' - A filter that returns only AppImageConfigs whose name contains the
-- specified string.
--
-- 'nextToken', 'listAppImageConfigs_nextToken' - If the previous call to @ListImages@ didn\'t return the full set of
-- AppImageConfigs, the call returns a token for getting the next set of
-- AppImageConfigs.
--
-- 'sortBy', 'listAppImageConfigs_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'sortOrder', 'listAppImageConfigs_sortOrder' - The sort order. The default value is @Descending@.
newListAppImageConfigs ::
  ListAppImageConfigs
newListAppImageConfigs =
  ListAppImageConfigs'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modifiedTimeAfter = Prelude.Nothing,
      modifiedTimeBefore = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A filter that returns only AppImageConfigs created on or after the
-- specified time.
listAppImageConfigs_creationTimeAfter :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_creationTimeAfter = Lens.lens (\ListAppImageConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListAppImageConfigs' {} a -> s {creationTimeAfter = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only AppImageConfigs created on or before the
-- specified time.
listAppImageConfigs_creationTimeBefore :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_creationTimeBefore = Lens.lens (\ListAppImageConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListAppImageConfigs' {} a -> s {creationTimeBefore = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of AppImageConfigs to return in the response. The
-- default value is 10.
listAppImageConfigs_maxResults :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.Natural)
listAppImageConfigs_maxResults = Lens.lens (\ListAppImageConfigs' {maxResults} -> maxResults) (\s@ListAppImageConfigs' {} a -> s {maxResults = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs modified on or after the
-- specified time.
listAppImageConfigs_modifiedTimeAfter :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_modifiedTimeAfter = Lens.lens (\ListAppImageConfigs' {modifiedTimeAfter} -> modifiedTimeAfter) (\s@ListAppImageConfigs' {} a -> s {modifiedTimeAfter = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only AppImageConfigs modified on or before the
-- specified time.
listAppImageConfigs_modifiedTimeBefore :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.UTCTime)
listAppImageConfigs_modifiedTimeBefore = Lens.lens (\ListAppImageConfigs' {modifiedTimeBefore} -> modifiedTimeBefore) (\s@ListAppImageConfigs' {} a -> s {modifiedTimeBefore = a} :: ListAppImageConfigs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only AppImageConfigs whose name contains the
-- specified string.
listAppImageConfigs_nameContains :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.Text)
listAppImageConfigs_nameContains = Lens.lens (\ListAppImageConfigs' {nameContains} -> nameContains) (\s@ListAppImageConfigs' {} a -> s {nameContains = a} :: ListAppImageConfigs)

-- | If the previous call to @ListImages@ didn\'t return the full set of
-- AppImageConfigs, the call returns a token for getting the next set of
-- AppImageConfigs.
listAppImageConfigs_nextToken :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe Prelude.Text)
listAppImageConfigs_nextToken = Lens.lens (\ListAppImageConfigs' {nextToken} -> nextToken) (\s@ListAppImageConfigs' {} a -> s {nextToken = a} :: ListAppImageConfigs)

-- | The property used to sort results. The default value is @CreationTime@.
listAppImageConfigs_sortBy :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe AppImageConfigSortKey)
listAppImageConfigs_sortBy = Lens.lens (\ListAppImageConfigs' {sortBy} -> sortBy) (\s@ListAppImageConfigs' {} a -> s {sortBy = a} :: ListAppImageConfigs)

-- | The sort order. The default value is @Descending@.
listAppImageConfigs_sortOrder :: Lens.Lens' ListAppImageConfigs (Prelude.Maybe SortOrder)
listAppImageConfigs_sortOrder = Lens.lens (\ListAppImageConfigs' {sortOrder} -> sortOrder) (\s@ListAppImageConfigs' {} a -> s {sortOrder = a} :: ListAppImageConfigs)

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
            Prelude.<$> ( x Data..?> "AppImageConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppImageConfigs where
  hashWithSalt _salt ListAppImageConfigs' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modifiedTimeAfter
      `Prelude.hashWithSalt` modifiedTimeBefore
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListAppImageConfigs where
  rnf ListAppImageConfigs' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modifiedTimeAfter
      `Prelude.seq` Prelude.rnf modifiedTimeBefore
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

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
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModifiedTimeAfter" Data..=)
              Prelude.<$> modifiedTimeAfter,
            ("ModifiedTimeBefore" Data..=)
              Prelude.<$> modifiedTimeBefore,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListAppImageConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAppImageConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppImageConfigsResponse' smart constructor.
data ListAppImageConfigsResponse = ListAppImageConfigsResponse'
  { -- | A list of AppImageConfigs and their properties.
    appImageConfigs :: Prelude.Maybe [AppImageConfigDetails],
    -- | A token for getting the next set of AppImageConfigs, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'appImageConfigs', 'listAppImageConfigsResponse_appImageConfigs' - A list of AppImageConfigs and their properties.
--
-- 'nextToken', 'listAppImageConfigsResponse_nextToken' - A token for getting the next set of AppImageConfigs, if there are any.
--
-- 'httpStatus', 'listAppImageConfigsResponse_httpStatus' - The response's http status code.
newListAppImageConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppImageConfigsResponse
newListAppImageConfigsResponse pHttpStatus_ =
  ListAppImageConfigsResponse'
    { appImageConfigs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of AppImageConfigs and their properties.
listAppImageConfigsResponse_appImageConfigs :: Lens.Lens' ListAppImageConfigsResponse (Prelude.Maybe [AppImageConfigDetails])
listAppImageConfigsResponse_appImageConfigs = Lens.lens (\ListAppImageConfigsResponse' {appImageConfigs} -> appImageConfigs) (\s@ListAppImageConfigsResponse' {} a -> s {appImageConfigs = a} :: ListAppImageConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of AppImageConfigs, if there are any.
listAppImageConfigsResponse_nextToken :: Lens.Lens' ListAppImageConfigsResponse (Prelude.Maybe Prelude.Text)
listAppImageConfigsResponse_nextToken = Lens.lens (\ListAppImageConfigsResponse' {nextToken} -> nextToken) (\s@ListAppImageConfigsResponse' {} a -> s {nextToken = a} :: ListAppImageConfigsResponse)

-- | The response's http status code.
listAppImageConfigsResponse_httpStatus :: Lens.Lens' ListAppImageConfigsResponse Prelude.Int
listAppImageConfigsResponse_httpStatus = Lens.lens (\ListAppImageConfigsResponse' {httpStatus} -> httpStatus) (\s@ListAppImageConfigsResponse' {} a -> s {httpStatus = a} :: ListAppImageConfigsResponse)

instance Prelude.NFData ListAppImageConfigsResponse where
  rnf ListAppImageConfigsResponse' {..} =
    Prelude.rnf appImageConfigs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
