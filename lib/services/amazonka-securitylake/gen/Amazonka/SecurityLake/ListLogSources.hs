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
-- Module      : Amazonka.SecurityLake.ListLogSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log sources in the current region.
--
-- This operation returns paginated results.
module Amazonka.SecurityLake.ListLogSources
  ( -- * Creating a Request
    ListLogSources (..),
    newListLogSources,

    -- * Request Lenses
    listLogSources_inputOrder,
    listLogSources_listAllDimensions,
    listLogSources_listSingleDimension,
    listLogSources_listTwoDimensions,
    listLogSources_maxResults,
    listLogSources_nextToken,

    -- * Destructuring the Response
    ListLogSourcesResponse (..),
    newListLogSourcesResponse,

    -- * Response Lenses
    listLogSourcesResponse_nextToken,
    listLogSourcesResponse_httpStatus,
    listLogSourcesResponse_regionSourceTypesAccountsList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newListLogSources' smart constructor.
data ListLogSources = ListLogSources'
  { -- | Lists the log sources in input order, namely Region, source type, and
    -- member account.
    inputOrder :: Prelude.Maybe [Dimension],
    -- | List the view of log sources for enabled Security Lake accounts in all
    -- Regions and source types.
    listAllDimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | List the view of log sources for enabled Security Lake accounts for the
    -- entire region.
    listSingleDimension :: Prelude.Maybe [Prelude.Text],
    -- | Lists the log sources for the specified source types in enabled Security
    -- Lake accounts for the entire Region, for selected member accounts.
    listTwoDimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The maximum number of accounts for which the configuration is displayed.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If nextToken is returned, there are more results available. You can make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputOrder', 'listLogSources_inputOrder' - Lists the log sources in input order, namely Region, source type, and
-- member account.
--
-- 'listAllDimensions', 'listLogSources_listAllDimensions' - List the view of log sources for enabled Security Lake accounts in all
-- Regions and source types.
--
-- 'listSingleDimension', 'listLogSources_listSingleDimension' - List the view of log sources for enabled Security Lake accounts for the
-- entire region.
--
-- 'listTwoDimensions', 'listLogSources_listTwoDimensions' - Lists the log sources for the specified source types in enabled Security
-- Lake accounts for the entire Region, for selected member accounts.
--
-- 'maxResults', 'listLogSources_maxResults' - The maximum number of accounts for which the configuration is displayed.
--
-- 'nextToken', 'listLogSources_nextToken' - If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
newListLogSources ::
  ListLogSources
newListLogSources =
  ListLogSources'
    { inputOrder = Prelude.Nothing,
      listAllDimensions = Prelude.Nothing,
      listSingleDimension = Prelude.Nothing,
      listTwoDimensions = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Lists the log sources in input order, namely Region, source type, and
-- member account.
listLogSources_inputOrder :: Lens.Lens' ListLogSources (Prelude.Maybe [Dimension])
listLogSources_inputOrder = Lens.lens (\ListLogSources' {inputOrder} -> inputOrder) (\s@ListLogSources' {} a -> s {inputOrder = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

-- | List the view of log sources for enabled Security Lake accounts in all
-- Regions and source types.
listLogSources_listAllDimensions :: Lens.Lens' ListLogSources (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text [Prelude.Text])))
listLogSources_listAllDimensions = Lens.lens (\ListLogSources' {listAllDimensions} -> listAllDimensions) (\s@ListLogSources' {} a -> s {listAllDimensions = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

-- | List the view of log sources for enabled Security Lake accounts for the
-- entire region.
listLogSources_listSingleDimension :: Lens.Lens' ListLogSources (Prelude.Maybe [Prelude.Text])
listLogSources_listSingleDimension = Lens.lens (\ListLogSources' {listSingleDimension} -> listSingleDimension) (\s@ListLogSources' {} a -> s {listSingleDimension = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

-- | Lists the log sources for the specified source types in enabled Security
-- Lake accounts for the entire Region, for selected member accounts.
listLogSources_listTwoDimensions :: Lens.Lens' ListLogSources (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
listLogSources_listTwoDimensions = Lens.lens (\ListLogSources' {listTwoDimensions} -> listTwoDimensions) (\s@ListLogSources' {} a -> s {listTwoDimensions = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of accounts for which the configuration is displayed.
listLogSources_maxResults :: Lens.Lens' ListLogSources (Prelude.Maybe Prelude.Int)
listLogSources_maxResults = Lens.lens (\ListLogSources' {maxResults} -> maxResults) (\s@ListLogSources' {} a -> s {maxResults = a} :: ListLogSources)

-- | If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
listLogSources_nextToken :: Lens.Lens' ListLogSources (Prelude.Maybe Prelude.Text)
listLogSources_nextToken = Lens.lens (\ListLogSources' {nextToken} -> nextToken) (\s@ListLogSources' {} a -> s {nextToken = a} :: ListLogSources)

instance Core.AWSPager ListLogSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLogSourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listLogSourcesResponse_regionSourceTypesAccountsList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLogSources_nextToken
          Lens..~ rs
          Lens.^? listLogSourcesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListLogSources where
  type
    AWSResponse ListLogSources =
      ListLogSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogSourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "regionSourceTypesAccountsList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListLogSources where
  hashWithSalt _salt ListLogSources' {..} =
    _salt `Prelude.hashWithSalt` inputOrder
      `Prelude.hashWithSalt` listAllDimensions
      `Prelude.hashWithSalt` listSingleDimension
      `Prelude.hashWithSalt` listTwoDimensions
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLogSources where
  rnf ListLogSources' {..} =
    Prelude.rnf inputOrder
      `Prelude.seq` Prelude.rnf listAllDimensions
      `Prelude.seq` Prelude.rnf listSingleDimension
      `Prelude.seq` Prelude.rnf listTwoDimensions
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLogSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLogSources where
  toJSON ListLogSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputOrder" Data..=) Prelude.<$> inputOrder,
            ("listAllDimensions" Data..=)
              Prelude.<$> listAllDimensions,
            ("listSingleDimension" Data..=)
              Prelude.<$> listSingleDimension,
            ("listTwoDimensions" Data..=)
              Prelude.<$> listTwoDimensions,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLogSources where
  toPath = Prelude.const "/v1/logsources/list"

instance Data.ToQuery ListLogSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLogSourcesResponse' smart constructor.
data ListLogSourcesResponse = ListLogSourcesResponse'
  { -- | If nextToken is returned, there are more results available. You can make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists the log sources in the Regions for enabled Security Lake accounts.
    regionSourceTypesAccountsList :: [Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text [Prelude.Text])]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLogSourcesResponse_nextToken' - If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'listLogSourcesResponse_httpStatus' - The response's http status code.
--
-- 'regionSourceTypesAccountsList', 'listLogSourcesResponse_regionSourceTypesAccountsList' - Lists the log sources in the Regions for enabled Security Lake accounts.
newListLogSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLogSourcesResponse
newListLogSourcesResponse pHttpStatus_ =
  ListLogSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      regionSourceTypesAccountsList = Prelude.mempty
    }

-- | If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
listLogSourcesResponse_nextToken :: Lens.Lens' ListLogSourcesResponse (Prelude.Maybe Prelude.Text)
listLogSourcesResponse_nextToken = Lens.lens (\ListLogSourcesResponse' {nextToken} -> nextToken) (\s@ListLogSourcesResponse' {} a -> s {nextToken = a} :: ListLogSourcesResponse)

-- | The response's http status code.
listLogSourcesResponse_httpStatus :: Lens.Lens' ListLogSourcesResponse Prelude.Int
listLogSourcesResponse_httpStatus = Lens.lens (\ListLogSourcesResponse' {httpStatus} -> httpStatus) (\s@ListLogSourcesResponse' {} a -> s {httpStatus = a} :: ListLogSourcesResponse)

-- | Lists the log sources in the Regions for enabled Security Lake accounts.
listLogSourcesResponse_regionSourceTypesAccountsList :: Lens.Lens' ListLogSourcesResponse [Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text [Prelude.Text])]
listLogSourcesResponse_regionSourceTypesAccountsList = Lens.lens (\ListLogSourcesResponse' {regionSourceTypesAccountsList} -> regionSourceTypesAccountsList) (\s@ListLogSourcesResponse' {} a -> s {regionSourceTypesAccountsList = a} :: ListLogSourcesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListLogSourcesResponse where
  rnf ListLogSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf regionSourceTypesAccountsList
