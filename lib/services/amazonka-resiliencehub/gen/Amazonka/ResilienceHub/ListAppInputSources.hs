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
-- Module      : Amazonka.ResilienceHub.ListAppInputSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the input sources of the Resilience Hub application. For more
-- information about the input sources supported by Resilience Hub, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/discover-structure.html Discover the structure and describe your Resilience Hub application>.
module Amazonka.ResilienceHub.ListAppInputSources
  ( -- * Creating a Request
    ListAppInputSources (..),
    newListAppInputSources,

    -- * Request Lenses
    listAppInputSources_maxResults,
    listAppInputSources_nextToken,
    listAppInputSources_appArn,
    listAppInputSources_appVersion,

    -- * Destructuring the Response
    ListAppInputSourcesResponse (..),
    newListAppInputSourcesResponse,

    -- * Response Lenses
    listAppInputSourcesResponse_nextToken,
    listAppInputSourcesResponse_httpStatus,
    listAppInputSourcesResponse_appInputSources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppInputSources' smart constructor.
data ListAppInputSources = ListAppInputSources'
  { -- | Maximum number of input sources to be displayed per Resilience Hub
    -- application.
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
    -- | The Resilience Hub application version.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInputSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppInputSources_maxResults' - Maximum number of input sources to be displayed per Resilience Hub
-- application.
--
-- 'nextToken', 'listAppInputSources_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'appArn', 'listAppInputSources_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'listAppInputSources_appVersion' - The Resilience Hub application version.
newListAppInputSources ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ListAppInputSources
newListAppInputSources pAppArn_ pAppVersion_ =
  ListAppInputSources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | Maximum number of input sources to be displayed per Resilience Hub
-- application.
listAppInputSources_maxResults :: Lens.Lens' ListAppInputSources (Prelude.Maybe Prelude.Natural)
listAppInputSources_maxResults = Lens.lens (\ListAppInputSources' {maxResults} -> maxResults) (\s@ListAppInputSources' {} a -> s {maxResults = a} :: ListAppInputSources)

-- | Null, or the token from a previous call to get the next set of results.
listAppInputSources_nextToken :: Lens.Lens' ListAppInputSources (Prelude.Maybe Prelude.Text)
listAppInputSources_nextToken = Lens.lens (\ListAppInputSources' {nextToken} -> nextToken) (\s@ListAppInputSources' {} a -> s {nextToken = a} :: ListAppInputSources)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
listAppInputSources_appArn :: Lens.Lens' ListAppInputSources Prelude.Text
listAppInputSources_appArn = Lens.lens (\ListAppInputSources' {appArn} -> appArn) (\s@ListAppInputSources' {} a -> s {appArn = a} :: ListAppInputSources)

-- | The Resilience Hub application version.
listAppInputSources_appVersion :: Lens.Lens' ListAppInputSources Prelude.Text
listAppInputSources_appVersion = Lens.lens (\ListAppInputSources' {appVersion} -> appVersion) (\s@ListAppInputSources' {} a -> s {appVersion = a} :: ListAppInputSources)

instance Core.AWSRequest ListAppInputSources where
  type
    AWSResponse ListAppInputSources =
      ListAppInputSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppInputSourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "appInputSources"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAppInputSources where
  hashWithSalt _salt ListAppInputSources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData ListAppInputSources where
  rnf ListAppInputSources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders ListAppInputSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppInputSources where
  toJSON ListAppInputSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath ListAppInputSources where
  toPath = Prelude.const "/list-app-input-sources"

instance Data.ToQuery ListAppInputSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppInputSourcesResponse' smart constructor.
data ListAppInputSourcesResponse = ListAppInputSourcesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of Resilience Hub application input sources.
    appInputSources :: [AppInputSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInputSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInputSourcesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppInputSourcesResponse_httpStatus' - The response's http status code.
--
-- 'appInputSources', 'listAppInputSourcesResponse_appInputSources' - The list of Resilience Hub application input sources.
newListAppInputSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppInputSourcesResponse
newListAppInputSourcesResponse pHttpStatus_ =
  ListAppInputSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      appInputSources = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppInputSourcesResponse_nextToken :: Lens.Lens' ListAppInputSourcesResponse (Prelude.Maybe Prelude.Text)
listAppInputSourcesResponse_nextToken = Lens.lens (\ListAppInputSourcesResponse' {nextToken} -> nextToken) (\s@ListAppInputSourcesResponse' {} a -> s {nextToken = a} :: ListAppInputSourcesResponse)

-- | The response's http status code.
listAppInputSourcesResponse_httpStatus :: Lens.Lens' ListAppInputSourcesResponse Prelude.Int
listAppInputSourcesResponse_httpStatus = Lens.lens (\ListAppInputSourcesResponse' {httpStatus} -> httpStatus) (\s@ListAppInputSourcesResponse' {} a -> s {httpStatus = a} :: ListAppInputSourcesResponse)

-- | The list of Resilience Hub application input sources.
listAppInputSourcesResponse_appInputSources :: Lens.Lens' ListAppInputSourcesResponse [AppInputSource]
listAppInputSourcesResponse_appInputSources = Lens.lens (\ListAppInputSourcesResponse' {appInputSources} -> appInputSources) (\s@ListAppInputSourcesResponse' {} a -> s {appInputSources = a} :: ListAppInputSourcesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAppInputSourcesResponse where
  rnf ListAppInputSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appInputSources
