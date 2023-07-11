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
-- Module      : Amazonka.WorkSpacesWeb.ListPortals
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list or web portals.
module Amazonka.WorkSpacesWeb.ListPortals
  ( -- * Creating a Request
    ListPortals (..),
    newListPortals,

    -- * Request Lenses
    listPortals_maxResults,
    listPortals_nextToken,

    -- * Destructuring the Response
    ListPortalsResponse (..),
    newListPortalsResponse,

    -- * Response Lenses
    listPortalsResponse_nextToken,
    listPortalsResponse_portals,
    listPortalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListPortals' smart constructor.
data ListPortals = ListPortals'
  { -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPortals_maxResults' - The maximum number of results to be included in the next page.
--
-- 'nextToken', 'listPortals_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newListPortals ::
  ListPortals
newListPortals =
  ListPortals'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be included in the next page.
listPortals_maxResults :: Lens.Lens' ListPortals (Prelude.Maybe Prelude.Natural)
listPortals_maxResults = Lens.lens (\ListPortals' {maxResults} -> maxResults) (\s@ListPortals' {} a -> s {maxResults = a} :: ListPortals)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listPortals_nextToken :: Lens.Lens' ListPortals (Prelude.Maybe Prelude.Text)
listPortals_nextToken = Lens.lens (\ListPortals' {nextToken} -> nextToken) (\s@ListPortals' {} a -> s {nextToken = a} :: ListPortals)

instance Core.AWSRequest ListPortals where
  type AWSResponse ListPortals = ListPortalsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortalsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "portals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortals where
  hashWithSalt _salt ListPortals' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPortals where
  rnf ListPortals' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPortals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPortals where
  toPath = Prelude.const "/portals"

instance Data.ToQuery ListPortals where
  toQuery ListPortals' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListPortalsResponse' smart constructor.
data ListPortalsResponse = ListPortalsResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The portals in the list.
    portals :: Prelude.Maybe [PortalSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPortalsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'portals', 'listPortalsResponse_portals' - The portals in the list.
--
-- 'httpStatus', 'listPortalsResponse_httpStatus' - The response's http status code.
newListPortalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPortalsResponse
newListPortalsResponse pHttpStatus_ =
  ListPortalsResponse'
    { nextToken = Prelude.Nothing,
      portals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listPortalsResponse_nextToken :: Lens.Lens' ListPortalsResponse (Prelude.Maybe Prelude.Text)
listPortalsResponse_nextToken = Lens.lens (\ListPortalsResponse' {nextToken} -> nextToken) (\s@ListPortalsResponse' {} a -> s {nextToken = a} :: ListPortalsResponse)

-- | The portals in the list.
listPortalsResponse_portals :: Lens.Lens' ListPortalsResponse (Prelude.Maybe [PortalSummary])
listPortalsResponse_portals = Lens.lens (\ListPortalsResponse' {portals} -> portals) (\s@ListPortalsResponse' {} a -> s {portals = a} :: ListPortalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPortalsResponse_httpStatus :: Lens.Lens' ListPortalsResponse Prelude.Int
listPortalsResponse_httpStatus = Lens.lens (\ListPortalsResponse' {httpStatus} -> httpStatus) (\s@ListPortalsResponse' {} a -> s {httpStatus = a} :: ListPortalsResponse)

instance Prelude.NFData ListPortalsResponse where
  rnf ListPortalsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf portals
      `Prelude.seq` Prelude.rnf httpStatus
