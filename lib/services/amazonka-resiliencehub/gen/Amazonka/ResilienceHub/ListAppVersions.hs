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
-- Module      : Amazonka.ResilienceHub.ListAppVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the different versions for the Resilience Hub applications.
module Amazonka.ResilienceHub.ListAppVersions
  ( -- * Creating a Request
    ListAppVersions (..),
    newListAppVersions,

    -- * Request Lenses
    listAppVersions_nextToken,
    listAppVersions_maxResults,
    listAppVersions_appArn,

    -- * Destructuring the Response
    ListAppVersionsResponse (..),
    newListAppVersionsResponse,

    -- * Response Lenses
    listAppVersionsResponse_nextToken,
    listAppVersionsResponse_httpStatus,
    listAppVersionsResponse_appVersions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppVersions' smart constructor.
data ListAppVersions = ListAppVersions'
  { -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppVersions_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'maxResults', 'listAppVersions_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'appArn', 'listAppVersions_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListAppVersions ::
  -- | 'appArn'
  Prelude.Text ->
  ListAppVersions
newListAppVersions pAppArn_ =
  ListAppVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | Null, or the token from a previous call to get the next set of results.
listAppVersions_nextToken :: Lens.Lens' ListAppVersions (Prelude.Maybe Prelude.Text)
listAppVersions_nextToken = Lens.lens (\ListAppVersions' {nextToken} -> nextToken) (\s@ListAppVersions' {} a -> s {nextToken = a} :: ListAppVersions)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listAppVersions_maxResults :: Lens.Lens' ListAppVersions (Prelude.Maybe Prelude.Natural)
listAppVersions_maxResults = Lens.lens (\ListAppVersions' {maxResults} -> maxResults) (\s@ListAppVersions' {} a -> s {maxResults = a} :: ListAppVersions)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listAppVersions_appArn :: Lens.Lens' ListAppVersions Prelude.Text
listAppVersions_appArn = Lens.lens (\ListAppVersions' {appArn} -> appArn) (\s@ListAppVersions' {} a -> s {appArn = a} :: ListAppVersions)

instance Core.AWSRequest ListAppVersions where
  type
    AWSResponse ListAppVersions =
      ListAppVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "appVersions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListAppVersions where
  hashWithSalt _salt ListAppVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData ListAppVersions where
  rnf ListAppVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders ListAppVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppVersions where
  toJSON ListAppVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath ListAppVersions where
  toPath = Prelude.const "/list-app-versions"

instance Data.ToQuery ListAppVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppVersionsResponse' smart constructor.
data ListAppVersionsResponse = ListAppVersionsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The version of the application.
    appVersions :: [AppVersionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppVersionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppVersionsResponse_httpStatus' - The response's http status code.
--
-- 'appVersions', 'listAppVersionsResponse_appVersions' - The version of the application.
newListAppVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppVersionsResponse
newListAppVersionsResponse pHttpStatus_ =
  ListAppVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      appVersions = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppVersionsResponse_nextToken :: Lens.Lens' ListAppVersionsResponse (Prelude.Maybe Prelude.Text)
listAppVersionsResponse_nextToken = Lens.lens (\ListAppVersionsResponse' {nextToken} -> nextToken) (\s@ListAppVersionsResponse' {} a -> s {nextToken = a} :: ListAppVersionsResponse)

-- | The response's http status code.
listAppVersionsResponse_httpStatus :: Lens.Lens' ListAppVersionsResponse Prelude.Int
listAppVersionsResponse_httpStatus = Lens.lens (\ListAppVersionsResponse' {httpStatus} -> httpStatus) (\s@ListAppVersionsResponse' {} a -> s {httpStatus = a} :: ListAppVersionsResponse)

-- | The version of the application.
listAppVersionsResponse_appVersions :: Lens.Lens' ListAppVersionsResponse [AppVersionSummary]
listAppVersionsResponse_appVersions = Lens.lens (\ListAppVersionsResponse' {appVersions} -> appVersions) (\s@ListAppVersionsResponse' {} a -> s {appVersions = a} :: ListAppVersionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAppVersionsResponse where
  rnf ListAppVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appVersions
