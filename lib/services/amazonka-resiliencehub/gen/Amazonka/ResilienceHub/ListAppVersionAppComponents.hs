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
-- Module      : Amazonka.ResilienceHub.ListAppVersionAppComponents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the Application Components in the Resilience Hub application.
module Amazonka.ResilienceHub.ListAppVersionAppComponents
  ( -- * Creating a Request
    ListAppVersionAppComponents (..),
    newListAppVersionAppComponents,

    -- * Request Lenses
    listAppVersionAppComponents_maxResults,
    listAppVersionAppComponents_nextToken,
    listAppVersionAppComponents_appArn,
    listAppVersionAppComponents_appVersion,

    -- * Destructuring the Response
    ListAppVersionAppComponentsResponse (..),
    newListAppVersionAppComponentsResponse,

    -- * Response Lenses
    listAppVersionAppComponentsResponse_appComponents,
    listAppVersionAppComponentsResponse_nextToken,
    listAppVersionAppComponentsResponse_httpStatus,
    listAppVersionAppComponentsResponse_appArn,
    listAppVersionAppComponentsResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppVersionAppComponents' smart constructor.
data ListAppVersionAppComponents = ListAppVersionAppComponents'
  { -- | Maximum number of Application Components to be displayed per Resilience
    -- Hub application version.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The version of the Application Component.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionAppComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppVersionAppComponents_maxResults' - Maximum number of Application Components to be displayed per Resilience
-- Hub application version.
--
-- 'nextToken', 'listAppVersionAppComponents_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'appArn', 'listAppVersionAppComponents_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'listAppVersionAppComponents_appVersion' - The version of the Application Component.
newListAppVersionAppComponents ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ListAppVersionAppComponents
newListAppVersionAppComponents pAppArn_ pAppVersion_ =
  ListAppVersionAppComponents'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | Maximum number of Application Components to be displayed per Resilience
-- Hub application version.
listAppVersionAppComponents_maxResults :: Lens.Lens' ListAppVersionAppComponents (Prelude.Maybe Prelude.Natural)
listAppVersionAppComponents_maxResults = Lens.lens (\ListAppVersionAppComponents' {maxResults} -> maxResults) (\s@ListAppVersionAppComponents' {} a -> s {maxResults = a} :: ListAppVersionAppComponents)

-- | Null, or the token from a previous call to get the next set of results.
listAppVersionAppComponents_nextToken :: Lens.Lens' ListAppVersionAppComponents (Prelude.Maybe Prelude.Text)
listAppVersionAppComponents_nextToken = Lens.lens (\ListAppVersionAppComponents' {nextToken} -> nextToken) (\s@ListAppVersionAppComponents' {} a -> s {nextToken = a} :: ListAppVersionAppComponents)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
listAppVersionAppComponents_appArn :: Lens.Lens' ListAppVersionAppComponents Prelude.Text
listAppVersionAppComponents_appArn = Lens.lens (\ListAppVersionAppComponents' {appArn} -> appArn) (\s@ListAppVersionAppComponents' {} a -> s {appArn = a} :: ListAppVersionAppComponents)

-- | The version of the Application Component.
listAppVersionAppComponents_appVersion :: Lens.Lens' ListAppVersionAppComponents Prelude.Text
listAppVersionAppComponents_appVersion = Lens.lens (\ListAppVersionAppComponents' {appVersion} -> appVersion) (\s@ListAppVersionAppComponents' {} a -> s {appVersion = a} :: ListAppVersionAppComponents)

instance Core.AWSRequest ListAppVersionAppComponents where
  type
    AWSResponse ListAppVersionAppComponents =
      ListAppVersionAppComponentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppVersionAppComponentsResponse'
            Prelude.<$> (x Data..?> "appComponents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable ListAppVersionAppComponents where
  hashWithSalt _salt ListAppVersionAppComponents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData ListAppVersionAppComponents where
  rnf ListAppVersionAppComponents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders ListAppVersionAppComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppVersionAppComponents where
  toJSON ListAppVersionAppComponents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath ListAppVersionAppComponents where
  toPath =
    Prelude.const "/list-app-version-app-components"

instance Data.ToQuery ListAppVersionAppComponents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppVersionAppComponentsResponse' smart constructor.
data ListAppVersionAppComponentsResponse = ListAppVersionAppComponentsResponse'
  { -- | Defines an Application Component.
    appComponents :: Prelude.Maybe [AppComponent],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The Resilience Hub application version.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionAppComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appComponents', 'listAppVersionAppComponentsResponse_appComponents' - Defines an Application Component.
--
-- 'nextToken', 'listAppVersionAppComponentsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppVersionAppComponentsResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'listAppVersionAppComponentsResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'listAppVersionAppComponentsResponse_appVersion' - The Resilience Hub application version.
newListAppVersionAppComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ListAppVersionAppComponentsResponse
newListAppVersionAppComponentsResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_ =
    ListAppVersionAppComponentsResponse'
      { appComponents =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | Defines an Application Component.
listAppVersionAppComponentsResponse_appComponents :: Lens.Lens' ListAppVersionAppComponentsResponse (Prelude.Maybe [AppComponent])
listAppVersionAppComponentsResponse_appComponents = Lens.lens (\ListAppVersionAppComponentsResponse' {appComponents} -> appComponents) (\s@ListAppVersionAppComponentsResponse' {} a -> s {appComponents = a} :: ListAppVersionAppComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listAppVersionAppComponentsResponse_nextToken :: Lens.Lens' ListAppVersionAppComponentsResponse (Prelude.Maybe Prelude.Text)
listAppVersionAppComponentsResponse_nextToken = Lens.lens (\ListAppVersionAppComponentsResponse' {nextToken} -> nextToken) (\s@ListAppVersionAppComponentsResponse' {} a -> s {nextToken = a} :: ListAppVersionAppComponentsResponse)

-- | The response's http status code.
listAppVersionAppComponentsResponse_httpStatus :: Lens.Lens' ListAppVersionAppComponentsResponse Prelude.Int
listAppVersionAppComponentsResponse_httpStatus = Lens.lens (\ListAppVersionAppComponentsResponse' {httpStatus} -> httpStatus) (\s@ListAppVersionAppComponentsResponse' {} a -> s {httpStatus = a} :: ListAppVersionAppComponentsResponse)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
listAppVersionAppComponentsResponse_appArn :: Lens.Lens' ListAppVersionAppComponentsResponse Prelude.Text
listAppVersionAppComponentsResponse_appArn = Lens.lens (\ListAppVersionAppComponentsResponse' {appArn} -> appArn) (\s@ListAppVersionAppComponentsResponse' {} a -> s {appArn = a} :: ListAppVersionAppComponentsResponse)

-- | The Resilience Hub application version.
listAppVersionAppComponentsResponse_appVersion :: Lens.Lens' ListAppVersionAppComponentsResponse Prelude.Text
listAppVersionAppComponentsResponse_appVersion = Lens.lens (\ListAppVersionAppComponentsResponse' {appVersion} -> appVersion) (\s@ListAppVersionAppComponentsResponse' {} a -> s {appVersion = a} :: ListAppVersionAppComponentsResponse)

instance
  Prelude.NFData
    ListAppVersionAppComponentsResponse
  where
  rnf ListAppVersionAppComponentsResponse' {..} =
    Prelude.rnf appComponents
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
