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
-- Module      : Amazonka.ResilienceHub.ListApps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Resilience Hub applications.
module Amazonka.ResilienceHub.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_appArn,
    listApps_maxResults,
    listApps_name,
    listApps_nextToken,

    -- * Destructuring the Response
    ListAppsResponse (..),
    newListAppsResponse,

    -- * Response Lenses
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
    listAppsResponse_appSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name for the one of the listed applications.
    name :: Prelude.Maybe Prelude.Text,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'listApps_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'maxResults', 'listApps_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'name', 'listApps_name' - The name for the one of the listed applications.
--
-- 'nextToken', 'listApps_nextToken' - Null, or the token from a previous call to get the next set of results.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { appArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listApps_appArn :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_appArn = Lens.lens (\ListApps' {appArn} -> appArn) (\s@ListApps' {} a -> s {appArn = a} :: ListApps)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listApps_maxResults :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Natural)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

-- | The name for the one of the listed applications.
listApps_name :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_name = Lens.lens (\ListApps' {name} -> name) (\s@ListApps' {} a -> s {name = a} :: ListApps)

-- | Null, or the token from a previous call to get the next set of results.
listApps_nextToken :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "appSummaries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListApps where
  hashWithSalt _salt ListApps' {..} =
    _salt
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApps where
  rnf ListApps' {..} =
    Prelude.rnf appArn `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf nextToken

instance Data.ToHeaders ListApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApps where
  toPath = Prelude.const "/list-apps"

instance Data.ToQuery ListApps where
  toQuery ListApps' {..} =
    Prelude.mconcat
      [ "appArn" Data.=: appArn,
        "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summaries for the Resilience Hub application.
    appSummaries :: [AppSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppsResponse_httpStatus' - The response's http status code.
--
-- 'appSummaries', 'listAppsResponse_appSummaries' - Summaries for the Resilience Hub application.
newListAppsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppsResponse
newListAppsResponse pHttpStatus_ =
  ListAppsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      appSummaries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppsResponse_nextToken :: Lens.Lens' ListAppsResponse (Prelude.Maybe Prelude.Text)
listAppsResponse_nextToken = Lens.lens (\ListAppsResponse' {nextToken} -> nextToken) (\s@ListAppsResponse' {} a -> s {nextToken = a} :: ListAppsResponse)

-- | The response's http status code.
listAppsResponse_httpStatus :: Lens.Lens' ListAppsResponse Prelude.Int
listAppsResponse_httpStatus = Lens.lens (\ListAppsResponse' {httpStatus} -> httpStatus) (\s@ListAppsResponse' {} a -> s {httpStatus = a} :: ListAppsResponse)

-- | Summaries for the Resilience Hub application.
listAppsResponse_appSummaries :: Lens.Lens' ListAppsResponse [AppSummary]
listAppsResponse_appSummaries = Lens.lens (\ListAppsResponse' {appSummaries} -> appSummaries) (\s@ListAppsResponse' {} a -> s {appSummaries = a} :: ListAppsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAppsResponse where
  rnf ListAppsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf appSummaries
