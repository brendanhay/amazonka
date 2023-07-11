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
-- Module      : Amazonka.MGN.ListTemplateActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List template post migration custom actions.
--
-- This operation returns paginated results.
module Amazonka.MGN.ListTemplateActions
  ( -- * Creating a Request
    ListTemplateActions (..),
    newListTemplateActions,

    -- * Request Lenses
    listTemplateActions_filters,
    listTemplateActions_maxResults,
    listTemplateActions_nextToken,
    listTemplateActions_launchConfigurationTemplateID,

    -- * Destructuring the Response
    ListTemplateActionsResponse (..),
    newListTemplateActionsResponse,

    -- * Response Lenses
    listTemplateActionsResponse_items,
    listTemplateActionsResponse_nextToken,
    listTemplateActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplateActions' smart constructor.
data ListTemplateActions = ListTemplateActions'
  { -- | Filters to apply when listing template post migration custom actions.
    filters :: Prelude.Maybe TemplateActionsRequestFilters,
    -- | Maximum amount of items to return when listing template post migration
    -- custom actions.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token to use when listing template post migration custom actions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Launch configuration template ID.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTemplateActions_filters' - Filters to apply when listing template post migration custom actions.
--
-- 'maxResults', 'listTemplateActions_maxResults' - Maximum amount of items to return when listing template post migration
-- custom actions.
--
-- 'nextToken', 'listTemplateActions_nextToken' - Next token to use when listing template post migration custom actions.
--
-- 'launchConfigurationTemplateID', 'listTemplateActions_launchConfigurationTemplateID' - Launch configuration template ID.
newListTemplateActions ::
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  ListTemplateActions
newListTemplateActions
  pLaunchConfigurationTemplateID_ =
    ListTemplateActions'
      { filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | Filters to apply when listing template post migration custom actions.
listTemplateActions_filters :: Lens.Lens' ListTemplateActions (Prelude.Maybe TemplateActionsRequestFilters)
listTemplateActions_filters = Lens.lens (\ListTemplateActions' {filters} -> filters) (\s@ListTemplateActions' {} a -> s {filters = a} :: ListTemplateActions)

-- | Maximum amount of items to return when listing template post migration
-- custom actions.
listTemplateActions_maxResults :: Lens.Lens' ListTemplateActions (Prelude.Maybe Prelude.Natural)
listTemplateActions_maxResults = Lens.lens (\ListTemplateActions' {maxResults} -> maxResults) (\s@ListTemplateActions' {} a -> s {maxResults = a} :: ListTemplateActions)

-- | Next token to use when listing template post migration custom actions.
listTemplateActions_nextToken :: Lens.Lens' ListTemplateActions (Prelude.Maybe Prelude.Text)
listTemplateActions_nextToken = Lens.lens (\ListTemplateActions' {nextToken} -> nextToken) (\s@ListTemplateActions' {} a -> s {nextToken = a} :: ListTemplateActions)

-- | Launch configuration template ID.
listTemplateActions_launchConfigurationTemplateID :: Lens.Lens' ListTemplateActions Prelude.Text
listTemplateActions_launchConfigurationTemplateID = Lens.lens (\ListTemplateActions' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@ListTemplateActions' {} a -> s {launchConfigurationTemplateID = a} :: ListTemplateActions)

instance Core.AWSPager ListTemplateActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplateActionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTemplateActionsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTemplateActions_nextToken
          Lens..~ rs
          Lens.^? listTemplateActionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTemplateActions where
  type
    AWSResponse ListTemplateActions =
      ListTemplateActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateActionsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTemplateActions where
  hashWithSalt _salt ListTemplateActions' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` launchConfigurationTemplateID

instance Prelude.NFData ListTemplateActions where
  rnf ListTemplateActions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID

instance Data.ToHeaders ListTemplateActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTemplateActions where
  toJSON ListTemplateActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "launchConfigurationTemplateID"
                  Data..= launchConfigurationTemplateID
              )
          ]
      )

instance Data.ToPath ListTemplateActions where
  toPath = Prelude.const "/ListTemplateActions"

instance Data.ToQuery ListTemplateActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTemplateActionsResponse' smart constructor.
data ListTemplateActionsResponse = ListTemplateActionsResponse'
  { -- | List of template post migration custom actions.
    items :: Prelude.Maybe [TemplateActionDocument],
    -- | Next token returned when listing template post migration custom actions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listTemplateActionsResponse_items' - List of template post migration custom actions.
--
-- 'nextToken', 'listTemplateActionsResponse_nextToken' - Next token returned when listing template post migration custom actions.
--
-- 'httpStatus', 'listTemplateActionsResponse_httpStatus' - The response's http status code.
newListTemplateActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTemplateActionsResponse
newListTemplateActionsResponse pHttpStatus_ =
  ListTemplateActionsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of template post migration custom actions.
listTemplateActionsResponse_items :: Lens.Lens' ListTemplateActionsResponse (Prelude.Maybe [TemplateActionDocument])
listTemplateActionsResponse_items = Lens.lens (\ListTemplateActionsResponse' {items} -> items) (\s@ListTemplateActionsResponse' {} a -> s {items = a} :: ListTemplateActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next token returned when listing template post migration custom actions.
listTemplateActionsResponse_nextToken :: Lens.Lens' ListTemplateActionsResponse (Prelude.Maybe Prelude.Text)
listTemplateActionsResponse_nextToken = Lens.lens (\ListTemplateActionsResponse' {nextToken} -> nextToken) (\s@ListTemplateActionsResponse' {} a -> s {nextToken = a} :: ListTemplateActionsResponse)

-- | The response's http status code.
listTemplateActionsResponse_httpStatus :: Lens.Lens' ListTemplateActionsResponse Prelude.Int
listTemplateActionsResponse_httpStatus = Lens.lens (\ListTemplateActionsResponse' {httpStatus} -> httpStatus) (\s@ListTemplateActionsResponse' {} a -> s {httpStatus = a} :: ListTemplateActionsResponse)

instance Prelude.NFData ListTemplateActionsResponse where
  rnf ListTemplateActionsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
