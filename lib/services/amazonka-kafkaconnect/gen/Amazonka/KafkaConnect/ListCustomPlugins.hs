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
-- Module      : Amazonka.KafkaConnect.ListCustomPlugins
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all of the custom plugins in this account and Region.
--
-- This operation returns paginated results.
module Amazonka.KafkaConnect.ListCustomPlugins
  ( -- * Creating a Request
    ListCustomPlugins (..),
    newListCustomPlugins,

    -- * Request Lenses
    listCustomPlugins_maxResults,
    listCustomPlugins_nextToken,

    -- * Destructuring the Response
    ListCustomPluginsResponse (..),
    newListCustomPluginsResponse,

    -- * Response Lenses
    listCustomPluginsResponse_customPlugins,
    listCustomPluginsResponse_nextToken,
    listCustomPluginsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomPlugins' smart constructor.
data ListCustomPlugins = ListCustomPlugins'
  { -- | The maximum number of custom plugins to list in one response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response of a ListCustomPlugins operation is truncated, it will
    -- include a NextToken. Send this NextToken in a subsequent request to
    -- continue listing from where the previous operation left off.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomPlugins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCustomPlugins_maxResults' - The maximum number of custom plugins to list in one response.
--
-- 'nextToken', 'listCustomPlugins_nextToken' - If the response of a ListCustomPlugins operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
newListCustomPlugins ::
  ListCustomPlugins
newListCustomPlugins =
  ListCustomPlugins'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of custom plugins to list in one response.
listCustomPlugins_maxResults :: Lens.Lens' ListCustomPlugins (Prelude.Maybe Prelude.Natural)
listCustomPlugins_maxResults = Lens.lens (\ListCustomPlugins' {maxResults} -> maxResults) (\s@ListCustomPlugins' {} a -> s {maxResults = a} :: ListCustomPlugins)

-- | If the response of a ListCustomPlugins operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
listCustomPlugins_nextToken :: Lens.Lens' ListCustomPlugins (Prelude.Maybe Prelude.Text)
listCustomPlugins_nextToken = Lens.lens (\ListCustomPlugins' {nextToken} -> nextToken) (\s@ListCustomPlugins' {} a -> s {nextToken = a} :: ListCustomPlugins)

instance Core.AWSPager ListCustomPlugins where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomPluginsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomPluginsResponse_customPlugins
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCustomPlugins_nextToken
          Lens..~ rs
          Lens.^? listCustomPluginsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCustomPlugins where
  type
    AWSResponse ListCustomPlugins =
      ListCustomPluginsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomPluginsResponse'
            Prelude.<$> (x Data..?> "customPlugins" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomPlugins where
  hashWithSalt _salt ListCustomPlugins' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCustomPlugins where
  rnf ListCustomPlugins' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCustomPlugins where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCustomPlugins where
  toPath = Prelude.const "/v1/custom-plugins"

instance Data.ToQuery ListCustomPlugins where
  toQuery ListCustomPlugins' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCustomPluginsResponse' smart constructor.
data ListCustomPluginsResponse = ListCustomPluginsResponse'
  { -- | An array of custom plugin descriptions.
    customPlugins :: Prelude.Maybe [CustomPluginSummary],
    -- | If the response of a ListCustomPlugins operation is truncated, it will
    -- include a NextToken. Send this NextToken in a subsequent request to
    -- continue listing from where the previous operation left off.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomPluginsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPlugins', 'listCustomPluginsResponse_customPlugins' - An array of custom plugin descriptions.
--
-- 'nextToken', 'listCustomPluginsResponse_nextToken' - If the response of a ListCustomPlugins operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
--
-- 'httpStatus', 'listCustomPluginsResponse_httpStatus' - The response's http status code.
newListCustomPluginsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomPluginsResponse
newListCustomPluginsResponse pHttpStatus_ =
  ListCustomPluginsResponse'
    { customPlugins =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of custom plugin descriptions.
listCustomPluginsResponse_customPlugins :: Lens.Lens' ListCustomPluginsResponse (Prelude.Maybe [CustomPluginSummary])
listCustomPluginsResponse_customPlugins = Lens.lens (\ListCustomPluginsResponse' {customPlugins} -> customPlugins) (\s@ListCustomPluginsResponse' {} a -> s {customPlugins = a} :: ListCustomPluginsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response of a ListCustomPlugins operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
listCustomPluginsResponse_nextToken :: Lens.Lens' ListCustomPluginsResponse (Prelude.Maybe Prelude.Text)
listCustomPluginsResponse_nextToken = Lens.lens (\ListCustomPluginsResponse' {nextToken} -> nextToken) (\s@ListCustomPluginsResponse' {} a -> s {nextToken = a} :: ListCustomPluginsResponse)

-- | The response's http status code.
listCustomPluginsResponse_httpStatus :: Lens.Lens' ListCustomPluginsResponse Prelude.Int
listCustomPluginsResponse_httpStatus = Lens.lens (\ListCustomPluginsResponse' {httpStatus} -> httpStatus) (\s@ListCustomPluginsResponse' {} a -> s {httpStatus = a} :: ListCustomPluginsResponse)

instance Prelude.NFData ListCustomPluginsResponse where
  rnf ListCustomPluginsResponse' {..} =
    Prelude.rnf customPlugins
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
