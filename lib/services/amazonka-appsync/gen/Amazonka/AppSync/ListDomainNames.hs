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
-- Module      : Amazonka.AppSync.ListDomainNames
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists multiple custom domain names.
module Amazonka.AppSync.ListDomainNames
  ( -- * Creating a Request
    ListDomainNames (..),
    newListDomainNames,

    -- * Request Lenses
    listDomainNames_nextToken,
    listDomainNames_maxResults,

    -- * Destructuring the Response
    ListDomainNamesResponse (..),
    newListDomainNamesResponse,

    -- * Response Lenses
    listDomainNamesResponse_nextToken,
    listDomainNamesResponse_domainNameConfigs,
    listDomainNamesResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDomainNames' smart constructor.
data ListDomainNames = ListDomainNames'
  { -- | The API token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainNames_nextToken' - The API token.
--
-- 'maxResults', 'listDomainNames_maxResults' - The maximum number of results that you want the request to return.
newListDomainNames ::
  ListDomainNames
newListDomainNames =
  ListDomainNames'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The API token.
listDomainNames_nextToken :: Lens.Lens' ListDomainNames (Prelude.Maybe Prelude.Text)
listDomainNames_nextToken = Lens.lens (\ListDomainNames' {nextToken} -> nextToken) (\s@ListDomainNames' {} a -> s {nextToken = a} :: ListDomainNames)

-- | The maximum number of results that you want the request to return.
listDomainNames_maxResults :: Lens.Lens' ListDomainNames (Prelude.Maybe Prelude.Natural)
listDomainNames_maxResults = Lens.lens (\ListDomainNames' {maxResults} -> maxResults) (\s@ListDomainNames' {} a -> s {maxResults = a} :: ListDomainNames)

instance Core.AWSRequest ListDomainNames where
  type
    AWSResponse ListDomainNames =
      ListDomainNamesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainNamesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "domainNameConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomainNames where
  hashWithSalt _salt ListDomainNames' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDomainNames where
  rnf ListDomainNames' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListDomainNames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDomainNames where
  toPath = Prelude.const "/v1/domainnames"

instance Data.ToQuery ListDomainNames where
  toQuery ListDomainNames' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
  { -- | The API token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists configurations for multiple domain names.
    domainNameConfigs :: Prelude.Maybe [DomainNameConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainNamesResponse_nextToken' - The API token.
--
-- 'domainNameConfigs', 'listDomainNamesResponse_domainNameConfigs' - Lists configurations for multiple domain names.
--
-- 'httpStatus', 'listDomainNamesResponse_httpStatus' - The response's http status code.
newListDomainNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainNamesResponse
newListDomainNamesResponse pHttpStatus_ =
  ListDomainNamesResponse'
    { nextToken =
        Prelude.Nothing,
      domainNameConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API token.
listDomainNamesResponse_nextToken :: Lens.Lens' ListDomainNamesResponse (Prelude.Maybe Prelude.Text)
listDomainNamesResponse_nextToken = Lens.lens (\ListDomainNamesResponse' {nextToken} -> nextToken) (\s@ListDomainNamesResponse' {} a -> s {nextToken = a} :: ListDomainNamesResponse)

-- | Lists configurations for multiple domain names.
listDomainNamesResponse_domainNameConfigs :: Lens.Lens' ListDomainNamesResponse (Prelude.Maybe [DomainNameConfig])
listDomainNamesResponse_domainNameConfigs = Lens.lens (\ListDomainNamesResponse' {domainNameConfigs} -> domainNameConfigs) (\s@ListDomainNamesResponse' {} a -> s {domainNameConfigs = a} :: ListDomainNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDomainNamesResponse_httpStatus :: Lens.Lens' ListDomainNamesResponse Prelude.Int
listDomainNamesResponse_httpStatus = Lens.lens (\ListDomainNamesResponse' {httpStatus} -> httpStatus) (\s@ListDomainNamesResponse' {} a -> s {httpStatus = a} :: ListDomainNamesResponse)

instance Prelude.NFData ListDomainNamesResponse where
  rnf ListDomainNamesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainNameConfigs
      `Prelude.seq` Prelude.rnf httpStatus
