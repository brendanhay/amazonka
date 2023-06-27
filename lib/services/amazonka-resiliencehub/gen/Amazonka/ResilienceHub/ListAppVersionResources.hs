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
-- Module      : Amazonka.ResilienceHub.ListAppVersionResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the resources in an Resilience Hub application.
module Amazonka.ResilienceHub.ListAppVersionResources
  ( -- * Creating a Request
    ListAppVersionResources (..),
    newListAppVersionResources,

    -- * Request Lenses
    listAppVersionResources_maxResults,
    listAppVersionResources_nextToken,
    listAppVersionResources_resolutionId,
    listAppVersionResources_appArn,
    listAppVersionResources_appVersion,

    -- * Destructuring the Response
    ListAppVersionResourcesResponse (..),
    newListAppVersionResourcesResponse,

    -- * Response Lenses
    listAppVersionResourcesResponse_nextToken,
    listAppVersionResourcesResponse_httpStatus,
    listAppVersionResourcesResponse_physicalResources,
    listAppVersionResourcesResponse_resolutionId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppVersionResources' smart constructor.
data ListAppVersionResources = ListAppVersionResources'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for a specific resolution.
    resolutionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppVersionResources_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listAppVersionResources_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'resolutionId', 'listAppVersionResources_resolutionId' - The identifier for a specific resolution.
--
-- 'appArn', 'listAppVersionResources_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'listAppVersionResources_appVersion' - The version of the application.
newListAppVersionResources ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ListAppVersionResources
newListAppVersionResources pAppArn_ pAppVersion_ =
  ListAppVersionResources'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resolutionId = Prelude.Nothing,
      appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listAppVersionResources_maxResults :: Lens.Lens' ListAppVersionResources (Prelude.Maybe Prelude.Natural)
listAppVersionResources_maxResults = Lens.lens (\ListAppVersionResources' {maxResults} -> maxResults) (\s@ListAppVersionResources' {} a -> s {maxResults = a} :: ListAppVersionResources)

-- | Null, or the token from a previous call to get the next set of results.
listAppVersionResources_nextToken :: Lens.Lens' ListAppVersionResources (Prelude.Maybe Prelude.Text)
listAppVersionResources_nextToken = Lens.lens (\ListAppVersionResources' {nextToken} -> nextToken) (\s@ListAppVersionResources' {} a -> s {nextToken = a} :: ListAppVersionResources)

-- | The identifier for a specific resolution.
listAppVersionResources_resolutionId :: Lens.Lens' ListAppVersionResources (Prelude.Maybe Prelude.Text)
listAppVersionResources_resolutionId = Lens.lens (\ListAppVersionResources' {resolutionId} -> resolutionId) (\s@ListAppVersionResources' {} a -> s {resolutionId = a} :: ListAppVersionResources)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
listAppVersionResources_appArn :: Lens.Lens' ListAppVersionResources Prelude.Text
listAppVersionResources_appArn = Lens.lens (\ListAppVersionResources' {appArn} -> appArn) (\s@ListAppVersionResources' {} a -> s {appArn = a} :: ListAppVersionResources)

-- | The version of the application.
listAppVersionResources_appVersion :: Lens.Lens' ListAppVersionResources Prelude.Text
listAppVersionResources_appVersion = Lens.lens (\ListAppVersionResources' {appVersion} -> appVersion) (\s@ListAppVersionResources' {} a -> s {appVersion = a} :: ListAppVersionResources)

instance Core.AWSRequest ListAppVersionResources where
  type
    AWSResponse ListAppVersionResources =
      ListAppVersionResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppVersionResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "physicalResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "resolutionId")
      )

instance Prelude.Hashable ListAppVersionResources where
  hashWithSalt _salt ListAppVersionResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resolutionId
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData ListAppVersionResources where
  rnf ListAppVersionResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resolutionId
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders ListAppVersionResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppVersionResources where
  toJSON ListAppVersionResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resolutionId" Data..=) Prelude.<$> resolutionId,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath ListAppVersionResources where
  toPath = Prelude.const "/list-app-version-resources"

instance Data.ToQuery ListAppVersionResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppVersionResourcesResponse' smart constructor.
data ListAppVersionResourcesResponse = ListAppVersionResourcesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The physical resources in the application version.
    physicalResources :: [PhysicalResource],
    -- | The ID for a specific resolution.
    resolutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppVersionResourcesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppVersionResourcesResponse_httpStatus' - The response's http status code.
--
-- 'physicalResources', 'listAppVersionResourcesResponse_physicalResources' - The physical resources in the application version.
--
-- 'resolutionId', 'listAppVersionResourcesResponse_resolutionId' - The ID for a specific resolution.
newListAppVersionResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resolutionId'
  Prelude.Text ->
  ListAppVersionResourcesResponse
newListAppVersionResourcesResponse
  pHttpStatus_
  pResolutionId_ =
    ListAppVersionResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        physicalResources = Prelude.mempty,
        resolutionId = pResolutionId_
      }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppVersionResourcesResponse_nextToken :: Lens.Lens' ListAppVersionResourcesResponse (Prelude.Maybe Prelude.Text)
listAppVersionResourcesResponse_nextToken = Lens.lens (\ListAppVersionResourcesResponse' {nextToken} -> nextToken) (\s@ListAppVersionResourcesResponse' {} a -> s {nextToken = a} :: ListAppVersionResourcesResponse)

-- | The response's http status code.
listAppVersionResourcesResponse_httpStatus :: Lens.Lens' ListAppVersionResourcesResponse Prelude.Int
listAppVersionResourcesResponse_httpStatus = Lens.lens (\ListAppVersionResourcesResponse' {httpStatus} -> httpStatus) (\s@ListAppVersionResourcesResponse' {} a -> s {httpStatus = a} :: ListAppVersionResourcesResponse)

-- | The physical resources in the application version.
listAppVersionResourcesResponse_physicalResources :: Lens.Lens' ListAppVersionResourcesResponse [PhysicalResource]
listAppVersionResourcesResponse_physicalResources = Lens.lens (\ListAppVersionResourcesResponse' {physicalResources} -> physicalResources) (\s@ListAppVersionResourcesResponse' {} a -> s {physicalResources = a} :: ListAppVersionResourcesResponse) Prelude.. Lens.coerced

-- | The ID for a specific resolution.
listAppVersionResourcesResponse_resolutionId :: Lens.Lens' ListAppVersionResourcesResponse Prelude.Text
listAppVersionResourcesResponse_resolutionId = Lens.lens (\ListAppVersionResourcesResponse' {resolutionId} -> resolutionId) (\s@ListAppVersionResourcesResponse' {} a -> s {resolutionId = a} :: ListAppVersionResourcesResponse)

instance
  Prelude.NFData
    ListAppVersionResourcesResponse
  where
  rnf ListAppVersionResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf physicalResources
      `Prelude.seq` Prelude.rnf resolutionId
