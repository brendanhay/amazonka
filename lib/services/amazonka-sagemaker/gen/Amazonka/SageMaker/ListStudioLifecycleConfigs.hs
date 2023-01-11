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
-- Module      : Amazonka.SageMaker.ListStudioLifecycleConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Studio Lifecycle Configurations in your Amazon Web Services
-- Account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListStudioLifecycleConfigs
  ( -- * Creating a Request
    ListStudioLifecycleConfigs (..),
    newListStudioLifecycleConfigs,

    -- * Request Lenses
    listStudioLifecycleConfigs_appTypeEquals,
    listStudioLifecycleConfigs_creationTimeAfter,
    listStudioLifecycleConfigs_creationTimeBefore,
    listStudioLifecycleConfigs_maxResults,
    listStudioLifecycleConfigs_modifiedTimeAfter,
    listStudioLifecycleConfigs_modifiedTimeBefore,
    listStudioLifecycleConfigs_nameContains,
    listStudioLifecycleConfigs_nextToken,
    listStudioLifecycleConfigs_sortBy,
    listStudioLifecycleConfigs_sortOrder,

    -- * Destructuring the Response
    ListStudioLifecycleConfigsResponse (..),
    newListStudioLifecycleConfigsResponse,

    -- * Response Lenses
    listStudioLifecycleConfigsResponse_nextToken,
    listStudioLifecycleConfigsResponse_studioLifecycleConfigs,
    listStudioLifecycleConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListStudioLifecycleConfigs' smart constructor.
data ListStudioLifecycleConfigs = ListStudioLifecycleConfigs'
  { -- | A parameter to search for the App Type to which the Lifecycle
    -- Configuration is attached.
    appTypeEquals :: Prelude.Maybe StudioLifecycleConfigAppType,
    -- | A filter that returns only Lifecycle Configurations created on or after
    -- the specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only Lifecycle Configurations created on or before
    -- the specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of Studio Lifecycle Configurations to return in the
    -- response. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only Lifecycle Configurations modified after the
    -- specified time.
    modifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only Lifecycle Configurations modified before the
    -- specified time.
    modifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A string in the Lifecycle Configuration name. This filter returns only
    -- Lifecycle Configurations whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the previous call to ListStudioLifecycleConfigs didn\'t return the
    -- full set of Lifecycle Configurations, the call returns a token for
    -- getting the next set of Lifecycle Configurations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is CreationTime.
    sortBy :: Prelude.Maybe StudioLifecycleConfigSortKey,
    -- | The sort order. The default value is Descending.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioLifecycleConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appTypeEquals', 'listStudioLifecycleConfigs_appTypeEquals' - A parameter to search for the App Type to which the Lifecycle
-- Configuration is attached.
--
-- 'creationTimeAfter', 'listStudioLifecycleConfigs_creationTimeAfter' - A filter that returns only Lifecycle Configurations created on or after
-- the specified time.
--
-- 'creationTimeBefore', 'listStudioLifecycleConfigs_creationTimeBefore' - A filter that returns only Lifecycle Configurations created on or before
-- the specified time.
--
-- 'maxResults', 'listStudioLifecycleConfigs_maxResults' - The maximum number of Studio Lifecycle Configurations to return in the
-- response. The default value is 10.
--
-- 'modifiedTimeAfter', 'listStudioLifecycleConfigs_modifiedTimeAfter' - A filter that returns only Lifecycle Configurations modified after the
-- specified time.
--
-- 'modifiedTimeBefore', 'listStudioLifecycleConfigs_modifiedTimeBefore' - A filter that returns only Lifecycle Configurations modified before the
-- specified time.
--
-- 'nameContains', 'listStudioLifecycleConfigs_nameContains' - A string in the Lifecycle Configuration name. This filter returns only
-- Lifecycle Configurations whose name contains the specified string.
--
-- 'nextToken', 'listStudioLifecycleConfigs_nextToken' - If the previous call to ListStudioLifecycleConfigs didn\'t return the
-- full set of Lifecycle Configurations, the call returns a token for
-- getting the next set of Lifecycle Configurations.
--
-- 'sortBy', 'listStudioLifecycleConfigs_sortBy' - The property used to sort results. The default value is CreationTime.
--
-- 'sortOrder', 'listStudioLifecycleConfigs_sortOrder' - The sort order. The default value is Descending.
newListStudioLifecycleConfigs ::
  ListStudioLifecycleConfigs
newListStudioLifecycleConfigs =
  ListStudioLifecycleConfigs'
    { appTypeEquals =
        Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modifiedTimeAfter = Prelude.Nothing,
      modifiedTimeBefore = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A parameter to search for the App Type to which the Lifecycle
-- Configuration is attached.
listStudioLifecycleConfigs_appTypeEquals :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe StudioLifecycleConfigAppType)
listStudioLifecycleConfigs_appTypeEquals = Lens.lens (\ListStudioLifecycleConfigs' {appTypeEquals} -> appTypeEquals) (\s@ListStudioLifecycleConfigs' {} a -> s {appTypeEquals = a} :: ListStudioLifecycleConfigs)

-- | A filter that returns only Lifecycle Configurations created on or after
-- the specified time.
listStudioLifecycleConfigs_creationTimeAfter :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_creationTimeAfter = Lens.lens (\ListStudioLifecycleConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListStudioLifecycleConfigs' {} a -> s {creationTimeAfter = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only Lifecycle Configurations created on or before
-- the specified time.
listStudioLifecycleConfigs_creationTimeBefore :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_creationTimeBefore = Lens.lens (\ListStudioLifecycleConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListStudioLifecycleConfigs' {} a -> s {creationTimeBefore = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of Studio Lifecycle Configurations to return in the
-- response. The default value is 10.
listStudioLifecycleConfigs_maxResults :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.Natural)
listStudioLifecycleConfigs_maxResults = Lens.lens (\ListStudioLifecycleConfigs' {maxResults} -> maxResults) (\s@ListStudioLifecycleConfigs' {} a -> s {maxResults = a} :: ListStudioLifecycleConfigs)

-- | A filter that returns only Lifecycle Configurations modified after the
-- specified time.
listStudioLifecycleConfigs_modifiedTimeAfter :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_modifiedTimeAfter = Lens.lens (\ListStudioLifecycleConfigs' {modifiedTimeAfter} -> modifiedTimeAfter) (\s@ListStudioLifecycleConfigs' {} a -> s {modifiedTimeAfter = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only Lifecycle Configurations modified before the
-- specified time.
listStudioLifecycleConfigs_modifiedTimeBefore :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_modifiedTimeBefore = Lens.lens (\ListStudioLifecycleConfigs' {modifiedTimeBefore} -> modifiedTimeBefore) (\s@ListStudioLifecycleConfigs' {} a -> s {modifiedTimeBefore = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Data._Time

-- | A string in the Lifecycle Configuration name. This filter returns only
-- Lifecycle Configurations whose name contains the specified string.
listStudioLifecycleConfigs_nameContains :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.Text)
listStudioLifecycleConfigs_nameContains = Lens.lens (\ListStudioLifecycleConfigs' {nameContains} -> nameContains) (\s@ListStudioLifecycleConfigs' {} a -> s {nameContains = a} :: ListStudioLifecycleConfigs)

-- | If the previous call to ListStudioLifecycleConfigs didn\'t return the
-- full set of Lifecycle Configurations, the call returns a token for
-- getting the next set of Lifecycle Configurations.
listStudioLifecycleConfigs_nextToken :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.Text)
listStudioLifecycleConfigs_nextToken = Lens.lens (\ListStudioLifecycleConfigs' {nextToken} -> nextToken) (\s@ListStudioLifecycleConfigs' {} a -> s {nextToken = a} :: ListStudioLifecycleConfigs)

-- | The property used to sort results. The default value is CreationTime.
listStudioLifecycleConfigs_sortBy :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe StudioLifecycleConfigSortKey)
listStudioLifecycleConfigs_sortBy = Lens.lens (\ListStudioLifecycleConfigs' {sortBy} -> sortBy) (\s@ListStudioLifecycleConfigs' {} a -> s {sortBy = a} :: ListStudioLifecycleConfigs)

-- | The sort order. The default value is Descending.
listStudioLifecycleConfigs_sortOrder :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe SortOrder)
listStudioLifecycleConfigs_sortOrder = Lens.lens (\ListStudioLifecycleConfigs' {sortOrder} -> sortOrder) (\s@ListStudioLifecycleConfigs' {} a -> s {sortOrder = a} :: ListStudioLifecycleConfigs)

instance Core.AWSPager ListStudioLifecycleConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudioLifecycleConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudioLifecycleConfigsResponse_studioLifecycleConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStudioLifecycleConfigs_nextToken
          Lens..~ rs
          Lens.^? listStudioLifecycleConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStudioLifecycleConfigs where
  type
    AWSResponse ListStudioLifecycleConfigs =
      ListStudioLifecycleConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioLifecycleConfigsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "StudioLifecycleConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioLifecycleConfigs where
  hashWithSalt _salt ListStudioLifecycleConfigs' {..} =
    _salt `Prelude.hashWithSalt` appTypeEquals
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modifiedTimeAfter
      `Prelude.hashWithSalt` modifiedTimeBefore
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListStudioLifecycleConfigs where
  rnf ListStudioLifecycleConfigs' {..} =
    Prelude.rnf appTypeEquals
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modifiedTimeAfter
      `Prelude.seq` Prelude.rnf modifiedTimeBefore
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListStudioLifecycleConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListStudioLifecycleConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStudioLifecycleConfigs where
  toJSON ListStudioLifecycleConfigs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppTypeEquals" Data..=) Prelude.<$> appTypeEquals,
            ("CreationTimeAfter" Data..=)
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

instance Data.ToPath ListStudioLifecycleConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStudioLifecycleConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStudioLifecycleConfigsResponse' smart constructor.
data ListStudioLifecycleConfigsResponse = ListStudioLifecycleConfigsResponse'
  { -- | A token for getting the next set of actions, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Lifecycle Configurations and their properties.
    studioLifecycleConfigs :: Prelude.Maybe [StudioLifecycleConfigDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioLifecycleConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStudioLifecycleConfigsResponse_nextToken' - A token for getting the next set of actions, if there are any.
--
-- 'studioLifecycleConfigs', 'listStudioLifecycleConfigsResponse_studioLifecycleConfigs' - A list of Lifecycle Configurations and their properties.
--
-- 'httpStatus', 'listStudioLifecycleConfigsResponse_httpStatus' - The response's http status code.
newListStudioLifecycleConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudioLifecycleConfigsResponse
newListStudioLifecycleConfigsResponse pHttpStatus_ =
  ListStudioLifecycleConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      studioLifecycleConfigs =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of actions, if there are any.
listStudioLifecycleConfigsResponse_nextToken :: Lens.Lens' ListStudioLifecycleConfigsResponse (Prelude.Maybe Prelude.Text)
listStudioLifecycleConfigsResponse_nextToken = Lens.lens (\ListStudioLifecycleConfigsResponse' {nextToken} -> nextToken) (\s@ListStudioLifecycleConfigsResponse' {} a -> s {nextToken = a} :: ListStudioLifecycleConfigsResponse)

-- | A list of Lifecycle Configurations and their properties.
listStudioLifecycleConfigsResponse_studioLifecycleConfigs :: Lens.Lens' ListStudioLifecycleConfigsResponse (Prelude.Maybe [StudioLifecycleConfigDetails])
listStudioLifecycleConfigsResponse_studioLifecycleConfigs = Lens.lens (\ListStudioLifecycleConfigsResponse' {studioLifecycleConfigs} -> studioLifecycleConfigs) (\s@ListStudioLifecycleConfigsResponse' {} a -> s {studioLifecycleConfigs = a} :: ListStudioLifecycleConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStudioLifecycleConfigsResponse_httpStatus :: Lens.Lens' ListStudioLifecycleConfigsResponse Prelude.Int
listStudioLifecycleConfigsResponse_httpStatus = Lens.lens (\ListStudioLifecycleConfigsResponse' {httpStatus} -> httpStatus) (\s@ListStudioLifecycleConfigsResponse' {} a -> s {httpStatus = a} :: ListStudioLifecycleConfigsResponse)

instance
  Prelude.NFData
    ListStudioLifecycleConfigsResponse
  where
  rnf ListStudioLifecycleConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf studioLifecycleConfigs
      `Prelude.seq` Prelude.rnf httpStatus
