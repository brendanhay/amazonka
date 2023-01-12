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
-- Module      : Amazonka.SimSpaceWeaver.ListSimulations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SimSpace Weaver simulations in the Amazon Web Services account
-- used to make the API call.
module Amazonka.SimSpaceWeaver.ListSimulations
  ( -- * Creating a Request
    ListSimulations (..),
    newListSimulations,

    -- * Request Lenses
    listSimulations_maxResults,
    listSimulations_nextToken,

    -- * Destructuring the Response
    ListSimulationsResponse (..),
    newListSimulationsResponse,

    -- * Response Lenses
    listSimulationsResponse_nextToken,
    listSimulationsResponse_simulations,
    listSimulationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newListSimulations' smart constructor.
data ListSimulations = ListSimulations'
  { -- | The maximum number of simulations to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If SimSpace Weaver returns @nextToken@, there are more results
    -- available. The value of @nextToken@ is a unique pagination token for
    -- each page. To retrieve the next page, call the operation again using the
    -- returned token. Keep all other arguments unchanged. If no results
    -- remain, @nextToken@ is set to @null@. Each pagination token expires
    -- after 24 hours. If you provide a token that isn\'t valid, you receive an
    -- /HTTP 400 ValidationException/ error.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSimulations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSimulations_maxResults' - The maximum number of simulations to list.
--
-- 'nextToken', 'listSimulations_nextToken' - If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
newListSimulations ::
  ListSimulations
newListSimulations =
  ListSimulations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of simulations to list.
listSimulations_maxResults :: Lens.Lens' ListSimulations (Prelude.Maybe Prelude.Natural)
listSimulations_maxResults = Lens.lens (\ListSimulations' {maxResults} -> maxResults) (\s@ListSimulations' {} a -> s {maxResults = a} :: ListSimulations)

-- | If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
listSimulations_nextToken :: Lens.Lens' ListSimulations (Prelude.Maybe Prelude.Text)
listSimulations_nextToken = Lens.lens (\ListSimulations' {nextToken} -> nextToken) (\s@ListSimulations' {} a -> s {nextToken = a} :: ListSimulations)

instance Core.AWSRequest ListSimulations where
  type
    AWSResponse ListSimulations =
      ListSimulationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSimulationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Simulations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSimulations where
  hashWithSalt _salt ListSimulations' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSimulations where
  rnf ListSimulations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSimulations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSimulations where
  toPath = Prelude.const "/listsimulations"

instance Data.ToQuery ListSimulations where
  toQuery ListSimulations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSimulationsResponse' smart constructor.
data ListSimulationsResponse = ListSimulationsResponse'
  { -- | If SimSpace Weaver returns @nextToken@, there are more results
    -- available. The value of @nextToken@ is a unique pagination token for
    -- each page. To retrieve the next page, call the operation again using the
    -- returned token. Keep all other arguments unchanged. If no results
    -- remain, @nextToken@ is set to @null@. Each pagination token expires
    -- after 24 hours. If you provide a token that isn\'t valid, you receive an
    -- /HTTP 400 ValidationException/ error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of simulations.
    simulations :: Prelude.Maybe [SimulationMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSimulationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSimulationsResponse_nextToken' - If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
--
-- 'simulations', 'listSimulationsResponse_simulations' - The list of simulations.
--
-- 'httpStatus', 'listSimulationsResponse_httpStatus' - The response's http status code.
newListSimulationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSimulationsResponse
newListSimulationsResponse pHttpStatus_ =
  ListSimulationsResponse'
    { nextToken =
        Prelude.Nothing,
      simulations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
listSimulationsResponse_nextToken :: Lens.Lens' ListSimulationsResponse (Prelude.Maybe Prelude.Text)
listSimulationsResponse_nextToken = Lens.lens (\ListSimulationsResponse' {nextToken} -> nextToken) (\s@ListSimulationsResponse' {} a -> s {nextToken = a} :: ListSimulationsResponse)

-- | The list of simulations.
listSimulationsResponse_simulations :: Lens.Lens' ListSimulationsResponse (Prelude.Maybe [SimulationMetadata])
listSimulationsResponse_simulations = Lens.lens (\ListSimulationsResponse' {simulations} -> simulations) (\s@ListSimulationsResponse' {} a -> s {simulations = a} :: ListSimulationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSimulationsResponse_httpStatus :: Lens.Lens' ListSimulationsResponse Prelude.Int
listSimulationsResponse_httpStatus = Lens.lens (\ListSimulationsResponse' {httpStatus} -> httpStatus) (\s@ListSimulationsResponse' {} a -> s {httpStatus = a} :: ListSimulationsResponse)

instance Prelude.NFData ListSimulationsResponse where
  rnf ListSimulationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf simulations
      `Prelude.seq` Prelude.rnf httpStatus
