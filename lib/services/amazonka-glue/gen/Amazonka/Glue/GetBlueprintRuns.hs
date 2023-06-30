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
-- Module      : Amazonka.Glue.GetBlueprintRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of blueprint runs for a specified blueprint.
module Amazonka.Glue.GetBlueprintRuns
  ( -- * Creating a Request
    GetBlueprintRuns (..),
    newGetBlueprintRuns,

    -- * Request Lenses
    getBlueprintRuns_maxResults,
    getBlueprintRuns_nextToken,
    getBlueprintRuns_blueprintName,

    -- * Destructuring the Response
    GetBlueprintRunsResponse (..),
    newGetBlueprintRunsResponse,

    -- * Response Lenses
    getBlueprintRunsResponse_blueprintRuns,
    getBlueprintRunsResponse_nextToken,
    getBlueprintRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBlueprintRuns' smart constructor.
data GetBlueprintRuns = GetBlueprintRuns'
  { -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the blueprint.
    blueprintName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprintRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getBlueprintRuns_maxResults' - The maximum size of a list to return.
--
-- 'nextToken', 'getBlueprintRuns_nextToken' - A continuation token, if this is a continuation request.
--
-- 'blueprintName', 'getBlueprintRuns_blueprintName' - The name of the blueprint.
newGetBlueprintRuns ::
  -- | 'blueprintName'
  Prelude.Text ->
  GetBlueprintRuns
newGetBlueprintRuns pBlueprintName_ =
  GetBlueprintRuns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      blueprintName = pBlueprintName_
    }

-- | The maximum size of a list to return.
getBlueprintRuns_maxResults :: Lens.Lens' GetBlueprintRuns (Prelude.Maybe Prelude.Natural)
getBlueprintRuns_maxResults = Lens.lens (\GetBlueprintRuns' {maxResults} -> maxResults) (\s@GetBlueprintRuns' {} a -> s {maxResults = a} :: GetBlueprintRuns)

-- | A continuation token, if this is a continuation request.
getBlueprintRuns_nextToken :: Lens.Lens' GetBlueprintRuns (Prelude.Maybe Prelude.Text)
getBlueprintRuns_nextToken = Lens.lens (\GetBlueprintRuns' {nextToken} -> nextToken) (\s@GetBlueprintRuns' {} a -> s {nextToken = a} :: GetBlueprintRuns)

-- | The name of the blueprint.
getBlueprintRuns_blueprintName :: Lens.Lens' GetBlueprintRuns Prelude.Text
getBlueprintRuns_blueprintName = Lens.lens (\GetBlueprintRuns' {blueprintName} -> blueprintName) (\s@GetBlueprintRuns' {} a -> s {blueprintName = a} :: GetBlueprintRuns)

instance Core.AWSRequest GetBlueprintRuns where
  type
    AWSResponse GetBlueprintRuns =
      GetBlueprintRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlueprintRunsResponse'
            Prelude.<$> (x Data..?> "BlueprintRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBlueprintRuns where
  hashWithSalt _salt GetBlueprintRuns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` blueprintName

instance Prelude.NFData GetBlueprintRuns where
  rnf GetBlueprintRuns' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf blueprintName

instance Data.ToHeaders GetBlueprintRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetBlueprintRuns" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBlueprintRuns where
  toJSON GetBlueprintRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("BlueprintName" Data..= blueprintName)
          ]
      )

instance Data.ToPath GetBlueprintRuns where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBlueprintRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBlueprintRunsResponse' smart constructor.
data GetBlueprintRunsResponse = GetBlueprintRunsResponse'
  { -- | Returns a list of @BlueprintRun@ objects.
    blueprintRuns :: Prelude.Maybe [BlueprintRun],
    -- | A continuation token, if not all blueprint runs have been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprintRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintRuns', 'getBlueprintRunsResponse_blueprintRuns' - Returns a list of @BlueprintRun@ objects.
--
-- 'nextToken', 'getBlueprintRunsResponse_nextToken' - A continuation token, if not all blueprint runs have been returned.
--
-- 'httpStatus', 'getBlueprintRunsResponse_httpStatus' - The response's http status code.
newGetBlueprintRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBlueprintRunsResponse
newGetBlueprintRunsResponse pHttpStatus_ =
  GetBlueprintRunsResponse'
    { blueprintRuns =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of @BlueprintRun@ objects.
getBlueprintRunsResponse_blueprintRuns :: Lens.Lens' GetBlueprintRunsResponse (Prelude.Maybe [BlueprintRun])
getBlueprintRunsResponse_blueprintRuns = Lens.lens (\GetBlueprintRunsResponse' {blueprintRuns} -> blueprintRuns) (\s@GetBlueprintRunsResponse' {} a -> s {blueprintRuns = a} :: GetBlueprintRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all blueprint runs have been returned.
getBlueprintRunsResponse_nextToken :: Lens.Lens' GetBlueprintRunsResponse (Prelude.Maybe Prelude.Text)
getBlueprintRunsResponse_nextToken = Lens.lens (\GetBlueprintRunsResponse' {nextToken} -> nextToken) (\s@GetBlueprintRunsResponse' {} a -> s {nextToken = a} :: GetBlueprintRunsResponse)

-- | The response's http status code.
getBlueprintRunsResponse_httpStatus :: Lens.Lens' GetBlueprintRunsResponse Prelude.Int
getBlueprintRunsResponse_httpStatus = Lens.lens (\GetBlueprintRunsResponse' {httpStatus} -> httpStatus) (\s@GetBlueprintRunsResponse' {} a -> s {httpStatus = a} :: GetBlueprintRunsResponse)

instance Prelude.NFData GetBlueprintRunsResponse where
  rnf GetBlueprintRunsResponse' {..} =
    Prelude.rnf blueprintRuns
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
