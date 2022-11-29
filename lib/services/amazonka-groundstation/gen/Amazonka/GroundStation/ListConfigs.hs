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
-- Module      : Amazonka.GroundStation.ListConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @Config@ objects.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListConfigs
  ( -- * Creating a Request
    ListConfigs (..),
    newListConfigs,

    -- * Request Lenses
    listConfigs_nextToken,
    listConfigs_maxResults,

    -- * Destructuring the Response
    ListConfigsResponse (..),
    newListConfigsResponse,

    -- * Response Lenses
    listConfigsResponse_nextToken,
    listConfigsResponse_configList,
    listConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListConfigs' smart constructor.
data ListConfigs = ListConfigs'
  { -- | Next token returned in the request of a previous @ListConfigs@ call.
    -- Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of @Configs@ returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigs_nextToken' - Next token returned in the request of a previous @ListConfigs@ call.
-- Used to get the next page of results.
--
-- 'maxResults', 'listConfigs_maxResults' - Maximum number of @Configs@ returned.
newListConfigs ::
  ListConfigs
newListConfigs =
  ListConfigs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Next token returned in the request of a previous @ListConfigs@ call.
-- Used to get the next page of results.
listConfigs_nextToken :: Lens.Lens' ListConfigs (Prelude.Maybe Prelude.Text)
listConfigs_nextToken = Lens.lens (\ListConfigs' {nextToken} -> nextToken) (\s@ListConfigs' {} a -> s {nextToken = a} :: ListConfigs)

-- | Maximum number of @Configs@ returned.
listConfigs_maxResults :: Lens.Lens' ListConfigs (Prelude.Maybe Prelude.Natural)
listConfigs_maxResults = Lens.lens (\ListConfigs' {maxResults} -> maxResults) (\s@ListConfigs' {} a -> s {maxResults = a} :: ListConfigs)

instance Core.AWSPager ListConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigsResponse_configList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConfigs_nextToken
          Lens..~ rs
          Lens.^? listConfigsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListConfigs where
  type AWSResponse ListConfigs = ListConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "configList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConfigs where
  hashWithSalt _salt ListConfigs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListConfigs where
  rnf ListConfigs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListConfigs where
  toPath = Prelude.const "/config"

instance Core.ToQuery ListConfigs where
  toQuery ListConfigs' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- |
--
-- /See:/ 'newListConfigsResponse' smart constructor.
data ListConfigsResponse = ListConfigsResponse'
  { -- | Next token returned in the response of a previous @ListConfigs@ call.
    -- Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of @Config@ items.
    configList :: Prelude.Maybe [ConfigListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigsResponse_nextToken' - Next token returned in the response of a previous @ListConfigs@ call.
-- Used to get the next page of results.
--
-- 'configList', 'listConfigsResponse_configList' - List of @Config@ items.
--
-- 'httpStatus', 'listConfigsResponse_httpStatus' - The response's http status code.
newListConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfigsResponse
newListConfigsResponse pHttpStatus_ =
  ListConfigsResponse'
    { nextToken = Prelude.Nothing,
      configList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Next token returned in the response of a previous @ListConfigs@ call.
-- Used to get the next page of results.
listConfigsResponse_nextToken :: Lens.Lens' ListConfigsResponse (Prelude.Maybe Prelude.Text)
listConfigsResponse_nextToken = Lens.lens (\ListConfigsResponse' {nextToken} -> nextToken) (\s@ListConfigsResponse' {} a -> s {nextToken = a} :: ListConfigsResponse)

-- | List of @Config@ items.
listConfigsResponse_configList :: Lens.Lens' ListConfigsResponse (Prelude.Maybe [ConfigListItem])
listConfigsResponse_configList = Lens.lens (\ListConfigsResponse' {configList} -> configList) (\s@ListConfigsResponse' {} a -> s {configList = a} :: ListConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listConfigsResponse_httpStatus :: Lens.Lens' ListConfigsResponse Prelude.Int
listConfigsResponse_httpStatus = Lens.lens (\ListConfigsResponse' {httpStatus} -> httpStatus) (\s@ListConfigsResponse' {} a -> s {httpStatus = a} :: ListConfigsResponse)

instance Prelude.NFData ListConfigsResponse where
  rnf ListConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf configList
      `Prelude.seq` Prelude.rnf httpStatus
