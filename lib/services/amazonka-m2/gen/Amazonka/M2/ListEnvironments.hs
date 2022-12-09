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
-- Module      : Amazonka.M2.ListEnvironments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the runtime environments.
--
-- This operation returns paginated results.
module Amazonka.M2.ListEnvironments
  ( -- * Creating a Request
    ListEnvironments (..),
    newListEnvironments,

    -- * Request Lenses
    listEnvironments_engineType,
    listEnvironments_maxResults,
    listEnvironments_names,
    listEnvironments_nextToken,

    -- * Destructuring the Response
    ListEnvironmentsResponse (..),
    newListEnvironmentsResponse,

    -- * Response Lenses
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,
    listEnvironmentsResponse_environments,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
  { -- | The engine type for the environment.
    engineType :: Prelude.Maybe EngineType,
    -- | The maximum number of environments to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the environment.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A pagination token to control the number of environments displayed in
    -- the list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'listEnvironments_engineType' - The engine type for the environment.
--
-- 'maxResults', 'listEnvironments_maxResults' - The maximum number of environments to return.
--
-- 'names', 'listEnvironments_names' - The name of the environment.
--
-- 'nextToken', 'listEnvironments_nextToken' - A pagination token to control the number of environments displayed in
-- the list.
newListEnvironments ::
  ListEnvironments
newListEnvironments =
  ListEnvironments'
    { engineType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      names = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The engine type for the environment.
listEnvironments_engineType :: Lens.Lens' ListEnvironments (Prelude.Maybe EngineType)
listEnvironments_engineType = Lens.lens (\ListEnvironments' {engineType} -> engineType) (\s@ListEnvironments' {} a -> s {engineType = a} :: ListEnvironments)

-- | The maximum number of environments to return.
listEnvironments_maxResults :: Lens.Lens' ListEnvironments (Prelude.Maybe Prelude.Natural)
listEnvironments_maxResults = Lens.lens (\ListEnvironments' {maxResults} -> maxResults) (\s@ListEnvironments' {} a -> s {maxResults = a} :: ListEnvironments)

-- | The name of the environment.
listEnvironments_names :: Lens.Lens' ListEnvironments (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listEnvironments_names = Lens.lens (\ListEnvironments' {names} -> names) (\s@ListEnvironments' {} a -> s {names = a} :: ListEnvironments) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token to control the number of environments displayed in
-- the list.
listEnvironments_nextToken :: Lens.Lens' ListEnvironments (Prelude.Maybe Prelude.Text)
listEnvironments_nextToken = Lens.lens (\ListEnvironments' {nextToken} -> nextToken) (\s@ListEnvironments' {} a -> s {nextToken = a} :: ListEnvironments)

instance Core.AWSPager ListEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listEnvironmentsResponse_environments) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEnvironments_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEnvironments where
  type
    AWSResponse ListEnvironments =
      ListEnvironmentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "environments" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEnvironments where
  hashWithSalt _salt ListEnvironments' {..} =
    _salt `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEnvironments where
  rnf ListEnvironments' {..} =
    Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEnvironments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEnvironments where
  toPath = Prelude.const "/environments"

instance Data.ToQuery ListEnvironments where
  toQuery ListEnvironments' {..} =
    Prelude.mconcat
      [ "engineType" Data.=: engineType,
        "maxResults" Data.=: maxResults,
        "names"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> names),
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { -- | A pagination token that\'s returned when the response doesn\'t contain
    -- all the environments.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns a list of summary details for all the environments in your
    -- account.
    environments :: [EnvironmentSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentsResponse_nextToken' - A pagination token that\'s returned when the response doesn\'t contain
-- all the environments.
--
-- 'httpStatus', 'listEnvironmentsResponse_httpStatus' - The response's http status code.
--
-- 'environments', 'listEnvironmentsResponse_environments' - Returns a list of summary details for all the environments in your
-- account.
newListEnvironmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentsResponse
newListEnvironmentsResponse pHttpStatus_ =
  ListEnvironmentsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      environments = Prelude.mempty
    }

-- | A pagination token that\'s returned when the response doesn\'t contain
-- all the environments.
listEnvironmentsResponse_nextToken :: Lens.Lens' ListEnvironmentsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentsResponse_nextToken = Lens.lens (\ListEnvironmentsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentsResponse' {} a -> s {nextToken = a} :: ListEnvironmentsResponse)

-- | The response's http status code.
listEnvironmentsResponse_httpStatus :: Lens.Lens' ListEnvironmentsResponse Prelude.Int
listEnvironmentsResponse_httpStatus = Lens.lens (\ListEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentsResponse)

-- | Returns a list of summary details for all the environments in your
-- account.
listEnvironmentsResponse_environments :: Lens.Lens' ListEnvironmentsResponse [EnvironmentSummary]
listEnvironmentsResponse_environments = Lens.lens (\ListEnvironmentsResponse' {environments} -> environments) (\s@ListEnvironmentsResponse' {} a -> s {environments = a} :: ListEnvironmentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEnvironmentsResponse where
  rnf ListEnvironmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environments
