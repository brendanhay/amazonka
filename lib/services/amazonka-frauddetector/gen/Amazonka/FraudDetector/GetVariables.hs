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
-- Module      : Amazonka.FraudDetector.GetVariables
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all of the variables or the specific variable. This is a paginated
-- API. Providing null @maxSizePerPage@ results in retrieving maximum of
-- 100 records per page. If you provide @maxSizePerPage@ the value must be
-- between 50 and 100. To get the next page result, a provide a pagination
-- token from @GetVariablesResult@ as part of your request. Null pagination
-- token fetches the records from the beginning.
module Amazonka.FraudDetector.GetVariables
  ( -- * Creating a Request
    GetVariables (..),
    newGetVariables,

    -- * Request Lenses
    getVariables_nextToken,
    getVariables_name,
    getVariables_maxResults,

    -- * Destructuring the Response
    GetVariablesResponse (..),
    newGetVariablesResponse,

    -- * Response Lenses
    getVariablesResponse_variables,
    getVariablesResponse_nextToken,
    getVariablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVariables' smart constructor.
data GetVariables = GetVariables'
  { -- | The next page token of the get variable request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The max size per page determined for the get variable request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVariables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getVariables_nextToken' - The next page token of the get variable request.
--
-- 'name', 'getVariables_name' - The name of the variable.
--
-- 'maxResults', 'getVariables_maxResults' - The max size per page determined for the get variable request.
newGetVariables ::
  GetVariables
newGetVariables =
  GetVariables'
    { nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next page token of the get variable request.
getVariables_nextToken :: Lens.Lens' GetVariables (Prelude.Maybe Prelude.Text)
getVariables_nextToken = Lens.lens (\GetVariables' {nextToken} -> nextToken) (\s@GetVariables' {} a -> s {nextToken = a} :: GetVariables)

-- | The name of the variable.
getVariables_name :: Lens.Lens' GetVariables (Prelude.Maybe Prelude.Text)
getVariables_name = Lens.lens (\GetVariables' {name} -> name) (\s@GetVariables' {} a -> s {name = a} :: GetVariables)

-- | The max size per page determined for the get variable request.
getVariables_maxResults :: Lens.Lens' GetVariables (Prelude.Maybe Prelude.Natural)
getVariables_maxResults = Lens.lens (\GetVariables' {maxResults} -> maxResults) (\s@GetVariables' {} a -> s {maxResults = a} :: GetVariables)

instance Core.AWSRequest GetVariables where
  type AWSResponse GetVariables = GetVariablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVariablesResponse'
            Prelude.<$> (x Core..?> "variables" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVariables

instance Prelude.NFData GetVariables

instance Core.ToHeaders GetVariables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetVariables" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetVariables where
  toJSON GetVariables' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("name" Core..=) Prelude.<$> name,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetVariables where
  toPath = Prelude.const "/"

instance Core.ToQuery GetVariables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVariablesResponse' smart constructor.
data GetVariablesResponse = GetVariablesResponse'
  { -- | The names of the variables returned.
    variables :: Prelude.Maybe [Variable],
    -- | The next page token to be used in subsequent requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVariablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variables', 'getVariablesResponse_variables' - The names of the variables returned.
--
-- 'nextToken', 'getVariablesResponse_nextToken' - The next page token to be used in subsequent requests.
--
-- 'httpStatus', 'getVariablesResponse_httpStatus' - The response's http status code.
newGetVariablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVariablesResponse
newGetVariablesResponse pHttpStatus_ =
  GetVariablesResponse'
    { variables = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the variables returned.
getVariablesResponse_variables :: Lens.Lens' GetVariablesResponse (Prelude.Maybe [Variable])
getVariablesResponse_variables = Lens.lens (\GetVariablesResponse' {variables} -> variables) (\s@GetVariablesResponse' {} a -> s {variables = a} :: GetVariablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token to be used in subsequent requests.
getVariablesResponse_nextToken :: Lens.Lens' GetVariablesResponse (Prelude.Maybe Prelude.Text)
getVariablesResponse_nextToken = Lens.lens (\GetVariablesResponse' {nextToken} -> nextToken) (\s@GetVariablesResponse' {} a -> s {nextToken = a} :: GetVariablesResponse)

-- | The response's http status code.
getVariablesResponse_httpStatus :: Lens.Lens' GetVariablesResponse Prelude.Int
getVariablesResponse_httpStatus = Lens.lens (\GetVariablesResponse' {httpStatus} -> httpStatus) (\s@GetVariablesResponse' {} a -> s {httpStatus = a} :: GetVariablesResponse)

instance Prelude.NFData GetVariablesResponse
