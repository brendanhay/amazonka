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
-- Module      : Network.AWS.SageMaker.ListStudioLifecycleConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Studio Lifecycle Configurations in your Amazon Web Services
-- Account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListStudioLifecycleConfigs
  ( -- * Creating a Request
    ListStudioLifecycleConfigs (..),
    newListStudioLifecycleConfigs,

    -- * Request Lenses
    listStudioLifecycleConfigs_nextToken,
    listStudioLifecycleConfigs_sortOrder,
    listStudioLifecycleConfigs_nameContains,
    listStudioLifecycleConfigs_maxResults,
    listStudioLifecycleConfigs_creationTimeBefore,
    listStudioLifecycleConfigs_modifiedTimeBefore,
    listStudioLifecycleConfigs_sortBy,
    listStudioLifecycleConfigs_appTypeEquals,
    listStudioLifecycleConfigs_modifiedTimeAfter,
    listStudioLifecycleConfigs_creationTimeAfter,

    -- * Destructuring the Response
    ListStudioLifecycleConfigsResponse (..),
    newListStudioLifecycleConfigsResponse,

    -- * Response Lenses
    listStudioLifecycleConfigsResponse_nextToken,
    listStudioLifecycleConfigsResponse_studioLifecycleConfigs,
    listStudioLifecycleConfigsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListStudioLifecycleConfigs' smart constructor.
data ListStudioLifecycleConfigs = ListStudioLifecycleConfigs'
  { -- | If the previous call to ListStudioLifecycleConfigs didn\'t return the
    -- full set of Lifecycle Configurations, the call returns a token for
    -- getting the next set of Lifecycle Configurations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is Descending.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A string in the Lifecycle Configuration name. This filter returns only
    -- Lifecycle Configurations whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Studio Lifecycle Configurations to return in the
    -- response. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only Lifecycle Configurations created on or before
    -- the specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only Lifecycle Configurations modified before the
    -- specified time.
    modifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is CreationTime.
    sortBy :: Prelude.Maybe StudioLifecycleConfigSortKey,
    -- | A parameter to search for the App Type to which the Lifecycle
    -- Configuration is attached.
    appTypeEquals :: Prelude.Maybe StudioLifecycleConfigAppType,
    -- | A filter that returns only Lifecycle Configurations modified after the
    -- specified time.
    modifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only Lifecycle Configurations created on or after
    -- the specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX
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
-- 'nextToken', 'listStudioLifecycleConfigs_nextToken' - If the previous call to ListStudioLifecycleConfigs didn\'t return the
-- full set of Lifecycle Configurations, the call returns a token for
-- getting the next set of Lifecycle Configurations.
--
-- 'sortOrder', 'listStudioLifecycleConfigs_sortOrder' - The sort order. The default value is Descending.
--
-- 'nameContains', 'listStudioLifecycleConfigs_nameContains' - A string in the Lifecycle Configuration name. This filter returns only
-- Lifecycle Configurations whose name contains the specified string.
--
-- 'maxResults', 'listStudioLifecycleConfigs_maxResults' - The maximum number of Studio Lifecycle Configurations to return in the
-- response. The default value is 10.
--
-- 'creationTimeBefore', 'listStudioLifecycleConfigs_creationTimeBefore' - A filter that returns only Lifecycle Configurations created on or before
-- the specified time.
--
-- 'modifiedTimeBefore', 'listStudioLifecycleConfigs_modifiedTimeBefore' - A filter that returns only Lifecycle Configurations modified before the
-- specified time.
--
-- 'sortBy', 'listStudioLifecycleConfigs_sortBy' - The property used to sort results. The default value is CreationTime.
--
-- 'appTypeEquals', 'listStudioLifecycleConfigs_appTypeEquals' - A parameter to search for the App Type to which the Lifecycle
-- Configuration is attached.
--
-- 'modifiedTimeAfter', 'listStudioLifecycleConfigs_modifiedTimeAfter' - A filter that returns only Lifecycle Configurations modified after the
-- specified time.
--
-- 'creationTimeAfter', 'listStudioLifecycleConfigs_creationTimeAfter' - A filter that returns only Lifecycle Configurations created on or after
-- the specified time.
newListStudioLifecycleConfigs ::
  ListStudioLifecycleConfigs
newListStudioLifecycleConfigs =
  ListStudioLifecycleConfigs'
    { nextToken =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      modifiedTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      appTypeEquals = Prelude.Nothing,
      modifiedTimeAfter = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | If the previous call to ListStudioLifecycleConfigs didn\'t return the
-- full set of Lifecycle Configurations, the call returns a token for
-- getting the next set of Lifecycle Configurations.
listStudioLifecycleConfigs_nextToken :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.Text)
listStudioLifecycleConfigs_nextToken = Lens.lens (\ListStudioLifecycleConfigs' {nextToken} -> nextToken) (\s@ListStudioLifecycleConfigs' {} a -> s {nextToken = a} :: ListStudioLifecycleConfigs)

-- | The sort order. The default value is Descending.
listStudioLifecycleConfigs_sortOrder :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe SortOrder)
listStudioLifecycleConfigs_sortOrder = Lens.lens (\ListStudioLifecycleConfigs' {sortOrder} -> sortOrder) (\s@ListStudioLifecycleConfigs' {} a -> s {sortOrder = a} :: ListStudioLifecycleConfigs)

-- | A string in the Lifecycle Configuration name. This filter returns only
-- Lifecycle Configurations whose name contains the specified string.
listStudioLifecycleConfigs_nameContains :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.Text)
listStudioLifecycleConfigs_nameContains = Lens.lens (\ListStudioLifecycleConfigs' {nameContains} -> nameContains) (\s@ListStudioLifecycleConfigs' {} a -> s {nameContains = a} :: ListStudioLifecycleConfigs)

-- | The maximum number of Studio Lifecycle Configurations to return in the
-- response. The default value is 10.
listStudioLifecycleConfigs_maxResults :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.Natural)
listStudioLifecycleConfigs_maxResults = Lens.lens (\ListStudioLifecycleConfigs' {maxResults} -> maxResults) (\s@ListStudioLifecycleConfigs' {} a -> s {maxResults = a} :: ListStudioLifecycleConfigs)

-- | A filter that returns only Lifecycle Configurations created on or before
-- the specified time.
listStudioLifecycleConfigs_creationTimeBefore :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_creationTimeBefore = Lens.lens (\ListStudioLifecycleConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListStudioLifecycleConfigs' {} a -> s {creationTimeBefore = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only Lifecycle Configurations modified before the
-- specified time.
listStudioLifecycleConfigs_modifiedTimeBefore :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_modifiedTimeBefore = Lens.lens (\ListStudioLifecycleConfigs' {modifiedTimeBefore} -> modifiedTimeBefore) (\s@ListStudioLifecycleConfigs' {} a -> s {modifiedTimeBefore = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is CreationTime.
listStudioLifecycleConfigs_sortBy :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe StudioLifecycleConfigSortKey)
listStudioLifecycleConfigs_sortBy = Lens.lens (\ListStudioLifecycleConfigs' {sortBy} -> sortBy) (\s@ListStudioLifecycleConfigs' {} a -> s {sortBy = a} :: ListStudioLifecycleConfigs)

-- | A parameter to search for the App Type to which the Lifecycle
-- Configuration is attached.
listStudioLifecycleConfigs_appTypeEquals :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe StudioLifecycleConfigAppType)
listStudioLifecycleConfigs_appTypeEquals = Lens.lens (\ListStudioLifecycleConfigs' {appTypeEquals} -> appTypeEquals) (\s@ListStudioLifecycleConfigs' {} a -> s {appTypeEquals = a} :: ListStudioLifecycleConfigs)

-- | A filter that returns only Lifecycle Configurations modified after the
-- specified time.
listStudioLifecycleConfigs_modifiedTimeAfter :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_modifiedTimeAfter = Lens.lens (\ListStudioLifecycleConfigs' {modifiedTimeAfter} -> modifiedTimeAfter) (\s@ListStudioLifecycleConfigs' {} a -> s {modifiedTimeAfter = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only Lifecycle Configurations created on or after
-- the specified time.
listStudioLifecycleConfigs_creationTimeAfter :: Lens.Lens' ListStudioLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listStudioLifecycleConfigs_creationTimeAfter = Lens.lens (\ListStudioLifecycleConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListStudioLifecycleConfigs' {} a -> s {creationTimeAfter = a} :: ListStudioLifecycleConfigs) Prelude.. Lens.mapping Core._Time

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioLifecycleConfigsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "StudioLifecycleConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioLifecycleConfigs

instance Prelude.NFData ListStudioLifecycleConfigs

instance Core.ToHeaders ListStudioLifecycleConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListStudioLifecycleConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListStudioLifecycleConfigs where
  toJSON ListStudioLifecycleConfigs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("ModifiedTimeBefore" Core..=)
              Prelude.<$> modifiedTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("AppTypeEquals" Core..=) Prelude.<$> appTypeEquals,
            ("ModifiedTimeAfter" Core..=)
              Prelude.<$> modifiedTimeAfter,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListStudioLifecycleConfigs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListStudioLifecycleConfigs where
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
listStudioLifecycleConfigsResponse_studioLifecycleConfigs = Lens.lens (\ListStudioLifecycleConfigsResponse' {studioLifecycleConfigs} -> studioLifecycleConfigs) (\s@ListStudioLifecycleConfigsResponse' {} a -> s {studioLifecycleConfigs = a} :: ListStudioLifecycleConfigsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStudioLifecycleConfigsResponse_httpStatus :: Lens.Lens' ListStudioLifecycleConfigsResponse Prelude.Int
listStudioLifecycleConfigsResponse_httpStatus = Lens.lens (\ListStudioLifecycleConfigsResponse' {httpStatus} -> httpStatus) (\s@ListStudioLifecycleConfigsResponse' {} a -> s {httpStatus = a} :: ListStudioLifecycleConfigsResponse)

instance
  Prelude.NFData
    ListStudioLifecycleConfigsResponse
