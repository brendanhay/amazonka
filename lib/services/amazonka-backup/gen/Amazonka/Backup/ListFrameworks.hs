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
-- Module      : Amazonka.Backup.ListFrameworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all frameworks for an Amazon Web Services account and
-- Amazon Web Services Region.
module Amazonka.Backup.ListFrameworks
  ( -- * Creating a Request
    ListFrameworks (..),
    newListFrameworks,

    -- * Request Lenses
    listFrameworks_maxResults,
    listFrameworks_nextToken,

    -- * Destructuring the Response
    ListFrameworksResponse (..),
    newListFrameworksResponse,

    -- * Response Lenses
    listFrameworksResponse_frameworks,
    listFrameworksResponse_nextToken,
    listFrameworksResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFrameworks' smart constructor.
data ListFrameworks = ListFrameworks'
  { -- | The number of desired results from 1 to 1000. Optional. If unspecified,
    -- the query will return 1 MB of data.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFrameworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFrameworks_maxResults' - The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
--
-- 'nextToken', 'listFrameworks_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListFrameworks ::
  ListFrameworks
newListFrameworks =
  ListFrameworks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
listFrameworks_maxResults :: Lens.Lens' ListFrameworks (Prelude.Maybe Prelude.Natural)
listFrameworks_maxResults = Lens.lens (\ListFrameworks' {maxResults} -> maxResults) (\s@ListFrameworks' {} a -> s {maxResults = a} :: ListFrameworks)

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listFrameworks_nextToken :: Lens.Lens' ListFrameworks (Prelude.Maybe Prelude.Text)
listFrameworks_nextToken = Lens.lens (\ListFrameworks' {nextToken} -> nextToken) (\s@ListFrameworks' {} a -> s {nextToken = a} :: ListFrameworks)

instance Core.AWSRequest ListFrameworks where
  type
    AWSResponse ListFrameworks =
      ListFrameworksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFrameworksResponse'
            Prelude.<$> (x Data..?> "Frameworks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFrameworks where
  hashWithSalt _salt ListFrameworks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFrameworks where
  rnf ListFrameworks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFrameworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFrameworks where
  toPath = Prelude.const "/audit/frameworks"

instance Data.ToQuery ListFrameworks where
  toQuery ListFrameworks' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFrameworksResponse' smart constructor.
data ListFrameworksResponse = ListFrameworksResponse'
  { -- | A list of frameworks with details for each framework, including the
    -- framework name, Amazon Resource Name (ARN), description, number of
    -- controls, creation time, and deployment status.
    frameworks :: Prelude.Maybe [Framework],
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFrameworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworks', 'listFrameworksResponse_frameworks' - A list of frameworks with details for each framework, including the
-- framework name, Amazon Resource Name (ARN), description, number of
-- controls, creation time, and deployment status.
--
-- 'nextToken', 'listFrameworksResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'httpStatus', 'listFrameworksResponse_httpStatus' - The response's http status code.
newListFrameworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFrameworksResponse
newListFrameworksResponse pHttpStatus_ =
  ListFrameworksResponse'
    { frameworks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of frameworks with details for each framework, including the
-- framework name, Amazon Resource Name (ARN), description, number of
-- controls, creation time, and deployment status.
listFrameworksResponse_frameworks :: Lens.Lens' ListFrameworksResponse (Prelude.Maybe [Framework])
listFrameworksResponse_frameworks = Lens.lens (\ListFrameworksResponse' {frameworks} -> frameworks) (\s@ListFrameworksResponse' {} a -> s {frameworks = a} :: ListFrameworksResponse) Prelude.. Lens.mapping Lens.coerced

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listFrameworksResponse_nextToken :: Lens.Lens' ListFrameworksResponse (Prelude.Maybe Prelude.Text)
listFrameworksResponse_nextToken = Lens.lens (\ListFrameworksResponse' {nextToken} -> nextToken) (\s@ListFrameworksResponse' {} a -> s {nextToken = a} :: ListFrameworksResponse)

-- | The response's http status code.
listFrameworksResponse_httpStatus :: Lens.Lens' ListFrameworksResponse Prelude.Int
listFrameworksResponse_httpStatus = Lens.lens (\ListFrameworksResponse' {httpStatus} -> httpStatus) (\s@ListFrameworksResponse' {} a -> s {httpStatus = a} :: ListFrameworksResponse)

instance Prelude.NFData ListFrameworksResponse where
  rnf ListFrameworksResponse' {..} =
    Prelude.rnf frameworks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
