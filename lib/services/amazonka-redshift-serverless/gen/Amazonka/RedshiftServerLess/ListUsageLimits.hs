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
-- Module      : Amazonka.RedshiftServerLess.ListUsageLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all usage limits within Amazon Redshift Serverless.
--
-- This operation returns paginated results.
module Amazonka.RedshiftServerLess.ListUsageLimits
  ( -- * Creating a Request
    ListUsageLimits (..),
    newListUsageLimits,

    -- * Request Lenses
    listUsageLimits_maxResults,
    listUsageLimits_nextToken,
    listUsageLimits_resourceArn,
    listUsageLimits_usageType,

    -- * Destructuring the Response
    ListUsageLimitsResponse (..),
    newListUsageLimitsResponse,

    -- * Response Lenses
    listUsageLimitsResponse_nextToken,
    listUsageLimitsResponse_usageLimits,
    listUsageLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUsageLimits' smart constructor.
data ListUsageLimits = ListUsageLimits'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results. The
    -- default is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListUsageLimits@ operation returns a @nextToken@, you
    -- can include the returned @nextToken@ in following @ListUsageLimits@
    -- operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the resource whose usage
    -- limits you want to list.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Redshift Serverless feature whose limits you want to see.
    usageType :: Prelude.Maybe UsageLimitUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsageLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUsageLimits_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 100.
--
-- 'nextToken', 'listUsageLimits_nextToken' - If your initial @ListUsageLimits@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in following @ListUsageLimits@
-- operations, which returns results in the next page.
--
-- 'resourceArn', 'listUsageLimits_resourceArn' - The Amazon Resource Name (ARN) associated with the resource whose usage
-- limits you want to list.
--
-- 'usageType', 'listUsageLimits_usageType' - The Amazon Redshift Serverless feature whose limits you want to see.
newListUsageLimits ::
  ListUsageLimits
newListUsageLimits =
  ListUsageLimits'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      usageType = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 100.
listUsageLimits_maxResults :: Lens.Lens' ListUsageLimits (Prelude.Maybe Prelude.Natural)
listUsageLimits_maxResults = Lens.lens (\ListUsageLimits' {maxResults} -> maxResults) (\s@ListUsageLimits' {} a -> s {maxResults = a} :: ListUsageLimits)

-- | If your initial @ListUsageLimits@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in following @ListUsageLimits@
-- operations, which returns results in the next page.
listUsageLimits_nextToken :: Lens.Lens' ListUsageLimits (Prelude.Maybe Prelude.Text)
listUsageLimits_nextToken = Lens.lens (\ListUsageLimits' {nextToken} -> nextToken) (\s@ListUsageLimits' {} a -> s {nextToken = a} :: ListUsageLimits)

-- | The Amazon Resource Name (ARN) associated with the resource whose usage
-- limits you want to list.
listUsageLimits_resourceArn :: Lens.Lens' ListUsageLimits (Prelude.Maybe Prelude.Text)
listUsageLimits_resourceArn = Lens.lens (\ListUsageLimits' {resourceArn} -> resourceArn) (\s@ListUsageLimits' {} a -> s {resourceArn = a} :: ListUsageLimits)

-- | The Amazon Redshift Serverless feature whose limits you want to see.
listUsageLimits_usageType :: Lens.Lens' ListUsageLimits (Prelude.Maybe UsageLimitUsageType)
listUsageLimits_usageType = Lens.lens (\ListUsageLimits' {usageType} -> usageType) (\s@ListUsageLimits' {} a -> s {usageType = a} :: ListUsageLimits)

instance Core.AWSPager ListUsageLimits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsageLimitsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsageLimitsResponse_usageLimits
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUsageLimits_nextToken
          Lens..~ rs
          Lens.^? listUsageLimitsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListUsageLimits where
  type
    AWSResponse ListUsageLimits =
      ListUsageLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsageLimitsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "usageLimits")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUsageLimits where
  hashWithSalt _salt ListUsageLimits' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` usageType

instance Prelude.NFData ListUsageLimits where
  rnf ListUsageLimits' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf usageType

instance Data.ToHeaders ListUsageLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.ListUsageLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUsageLimits where
  toJSON ListUsageLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resourceArn" Data..=) Prelude.<$> resourceArn,
            ("usageType" Data..=) Prelude.<$> usageType
          ]
      )

instance Data.ToPath ListUsageLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUsageLimits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUsageLimitsResponse' smart constructor.
data ListUsageLimitsResponse = ListUsageLimitsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of returned usage limit objects.
    usageLimits :: Prelude.Maybe (Prelude.NonEmpty UsageLimit),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsageLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsageLimitsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'usageLimits', 'listUsageLimitsResponse_usageLimits' - An array of returned usage limit objects.
--
-- 'httpStatus', 'listUsageLimitsResponse_httpStatus' - The response's http status code.
newListUsageLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUsageLimitsResponse
newListUsageLimitsResponse pHttpStatus_ =
  ListUsageLimitsResponse'
    { nextToken =
        Prelude.Nothing,
      usageLimits = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listUsageLimitsResponse_nextToken :: Lens.Lens' ListUsageLimitsResponse (Prelude.Maybe Prelude.Text)
listUsageLimitsResponse_nextToken = Lens.lens (\ListUsageLimitsResponse' {nextToken} -> nextToken) (\s@ListUsageLimitsResponse' {} a -> s {nextToken = a} :: ListUsageLimitsResponse)

-- | An array of returned usage limit objects.
listUsageLimitsResponse_usageLimits :: Lens.Lens' ListUsageLimitsResponse (Prelude.Maybe (Prelude.NonEmpty UsageLimit))
listUsageLimitsResponse_usageLimits = Lens.lens (\ListUsageLimitsResponse' {usageLimits} -> usageLimits) (\s@ListUsageLimitsResponse' {} a -> s {usageLimits = a} :: ListUsageLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUsageLimitsResponse_httpStatus :: Lens.Lens' ListUsageLimitsResponse Prelude.Int
listUsageLimitsResponse_httpStatus = Lens.lens (\ListUsageLimitsResponse' {httpStatus} -> httpStatus) (\s@ListUsageLimitsResponse' {} a -> s {httpStatus = a} :: ListUsageLimitsResponse)

instance Prelude.NFData ListUsageLimitsResponse where
  rnf ListUsageLimitsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf usageLimits
      `Prelude.seq` Prelude.rnf httpStatus
