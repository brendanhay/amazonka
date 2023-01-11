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
-- Module      : Amazonka.OpenSearch.ListDomainNames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all Amazon OpenSearch Service domains owned by the
-- current user in the active Region.
module Amazonka.OpenSearch.ListDomainNames
  ( -- * Creating a Request
    ListDomainNames (..),
    newListDomainNames,

    -- * Request Lenses
    listDomainNames_engineType,

    -- * Destructuring the Response
    ListDomainNamesResponse (..),
    newListDomainNamesResponse,

    -- * Response Lenses
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @ListDomainNames@ operation.
--
-- /See:/ 'newListDomainNames' smart constructor.
data ListDomainNames = ListDomainNames'
  { -- | Filters the output by domain engine type.
    engineType :: Prelude.Maybe EngineType
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
-- 'engineType', 'listDomainNames_engineType' - Filters the output by domain engine type.
newListDomainNames ::
  ListDomainNames
newListDomainNames =
  ListDomainNames' {engineType = Prelude.Nothing}

-- | Filters the output by domain engine type.
listDomainNames_engineType :: Lens.Lens' ListDomainNames (Prelude.Maybe EngineType)
listDomainNames_engineType = Lens.lens (\ListDomainNames' {engineType} -> engineType) (\s@ListDomainNames' {} a -> s {engineType = a} :: ListDomainNames)

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
            Prelude.<$> (x Data..?> "DomainNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomainNames where
  hashWithSalt _salt ListDomainNames' {..} =
    _salt `Prelude.hashWithSalt` engineType

instance Prelude.NFData ListDomainNames where
  rnf ListDomainNames' {..} = Prelude.rnf engineType

instance Data.ToHeaders ListDomainNames where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDomainNames where
  toPath = Prelude.const "/2021-01-01/domain"

instance Data.ToQuery ListDomainNames where
  toQuery ListDomainNames' {..} =
    Prelude.mconcat ["engineType" Data.=: engineType]

-- | The results of a @ListDomainNames@ operation. Contains the names of all
-- domains owned by this account and their respective engine types.
--
-- /See:/ 'newListDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
  { -- | The names of all OpenSearch Service domains owned by the current user
    -- and their respective engine types.
    domainNames :: Prelude.Maybe [DomainInfo],
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
-- 'domainNames', 'listDomainNamesResponse_domainNames' - The names of all OpenSearch Service domains owned by the current user
-- and their respective engine types.
--
-- 'httpStatus', 'listDomainNamesResponse_httpStatus' - The response's http status code.
newListDomainNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainNamesResponse
newListDomainNamesResponse pHttpStatus_ =
  ListDomainNamesResponse'
    { domainNames =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of all OpenSearch Service domains owned by the current user
-- and their respective engine types.
listDomainNamesResponse_domainNames :: Lens.Lens' ListDomainNamesResponse (Prelude.Maybe [DomainInfo])
listDomainNamesResponse_domainNames = Lens.lens (\ListDomainNamesResponse' {domainNames} -> domainNames) (\s@ListDomainNamesResponse' {} a -> s {domainNames = a} :: ListDomainNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDomainNamesResponse_httpStatus :: Lens.Lens' ListDomainNamesResponse Prelude.Int
listDomainNamesResponse_httpStatus = Lens.lens (\ListDomainNamesResponse' {httpStatus} -> httpStatus) (\s@ListDomainNamesResponse' {} a -> s {httpStatus = a} :: ListDomainNamesResponse)

instance Prelude.NFData ListDomainNamesResponse where
  rnf ListDomainNamesResponse' {..} =
    Prelude.rnf domainNames
      `Prelude.seq` Prelude.rnf httpStatus
