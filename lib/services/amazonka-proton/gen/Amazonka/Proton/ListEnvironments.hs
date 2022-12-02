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
-- Module      : Amazonka.Proton.ListEnvironments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List environments with detail data summaries.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListEnvironments
  ( -- * Creating a Request
    ListEnvironments (..),
    newListEnvironments,

    -- * Request Lenses
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironments_environmentTemplates,

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
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
  { -- | A token that indicates the location of the next environment in the array
    -- of environments, after the list of environments that was previously
    -- requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of environments to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An array of the versions of the environment template.
    environmentTemplates :: Prelude.Maybe [EnvironmentTemplateFilter]
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
-- 'nextToken', 'listEnvironments_nextToken' - A token that indicates the location of the next environment in the array
-- of environments, after the list of environments that was previously
-- requested.
--
-- 'maxResults', 'listEnvironments_maxResults' - The maximum number of environments to list.
--
-- 'environmentTemplates', 'listEnvironments_environmentTemplates' - An array of the versions of the environment template.
newListEnvironments ::
  ListEnvironments
newListEnvironments =
  ListEnvironments'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      environmentTemplates = Prelude.Nothing
    }

-- | A token that indicates the location of the next environment in the array
-- of environments, after the list of environments that was previously
-- requested.
listEnvironments_nextToken :: Lens.Lens' ListEnvironments (Prelude.Maybe Prelude.Text)
listEnvironments_nextToken = Lens.lens (\ListEnvironments' {nextToken} -> nextToken) (\s@ListEnvironments' {} a -> s {nextToken = a} :: ListEnvironments)

-- | The maximum number of environments to list.
listEnvironments_maxResults :: Lens.Lens' ListEnvironments (Prelude.Maybe Prelude.Natural)
listEnvironments_maxResults = Lens.lens (\ListEnvironments' {maxResults} -> maxResults) (\s@ListEnvironments' {} a -> s {maxResults = a} :: ListEnvironments)

-- | An array of the versions of the environment template.
listEnvironments_environmentTemplates :: Lens.Lens' ListEnvironments (Prelude.Maybe [EnvironmentTemplateFilter])
listEnvironments_environmentTemplates = Lens.lens (\ListEnvironments' {environmentTemplates} -> environmentTemplates) (\s@ListEnvironments' {} a -> s {environmentTemplates = a} :: ListEnvironments) Prelude.. Lens.mapping Lens.coerced

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
    Request.postJSON (overrides defaultService)
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
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` environmentTemplates

instance Prelude.NFData ListEnvironments where
  rnf ListEnvironments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf environmentTemplates

instance Data.ToHeaders ListEnvironments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListEnvironments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEnvironments where
  toJSON ListEnvironments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("environmentTemplates" Data..=)
              Prelude.<$> environmentTemplates
          ]
      )

instance Data.ToPath ListEnvironments where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEnvironments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { -- | A token that indicates the location of the next environment in the array
    -- of environments, after the current requested list of environments.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of environment detail data summaries.
    environments :: [EnvironmentSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentsResponse_nextToken' - A token that indicates the location of the next environment in the array
-- of environments, after the current requested list of environments.
--
-- 'httpStatus', 'listEnvironmentsResponse_httpStatus' - The response's http status code.
--
-- 'environments', 'listEnvironmentsResponse_environments' - An array of environment detail data summaries.
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

-- | A token that indicates the location of the next environment in the array
-- of environments, after the current requested list of environments.
listEnvironmentsResponse_nextToken :: Lens.Lens' ListEnvironmentsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentsResponse_nextToken = Lens.lens (\ListEnvironmentsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentsResponse' {} a -> s {nextToken = a} :: ListEnvironmentsResponse)

-- | The response's http status code.
listEnvironmentsResponse_httpStatus :: Lens.Lens' ListEnvironmentsResponse Prelude.Int
listEnvironmentsResponse_httpStatus = Lens.lens (\ListEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentsResponse)

-- | An array of environment detail data summaries.
listEnvironmentsResponse_environments :: Lens.Lens' ListEnvironmentsResponse [EnvironmentSummary]
listEnvironmentsResponse_environments = Lens.lens (\ListEnvironmentsResponse' {environments} -> environments) (\s@ListEnvironmentsResponse' {} a -> s {environments = a} :: ListEnvironmentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEnvironmentsResponse where
  rnf ListEnvironmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environments
