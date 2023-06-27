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
-- Module      : Amazonka.ResilienceHub.ListUnsupportedAppVersionResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources that are not currently supported in Resilience Hub.
-- An unsupported resource is a resource that exists in the object that was
-- used to create an app, but is not supported by Resilience Hub.
module Amazonka.ResilienceHub.ListUnsupportedAppVersionResources
  ( -- * Creating a Request
    ListUnsupportedAppVersionResources (..),
    newListUnsupportedAppVersionResources,

    -- * Request Lenses
    listUnsupportedAppVersionResources_maxResults,
    listUnsupportedAppVersionResources_nextToken,
    listUnsupportedAppVersionResources_resolutionId,
    listUnsupportedAppVersionResources_appArn,
    listUnsupportedAppVersionResources_appVersion,

    -- * Destructuring the Response
    ListUnsupportedAppVersionResourcesResponse (..),
    newListUnsupportedAppVersionResourcesResponse,

    -- * Response Lenses
    listUnsupportedAppVersionResourcesResponse_nextToken,
    listUnsupportedAppVersionResourcesResponse_httpStatus,
    listUnsupportedAppVersionResourcesResponse_resolutionId,
    listUnsupportedAppVersionResourcesResponse_unsupportedResources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUnsupportedAppVersionResources' smart constructor.
data ListUnsupportedAppVersionResources = ListUnsupportedAppVersionResources'
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
-- Create a value of 'ListUnsupportedAppVersionResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUnsupportedAppVersionResources_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listUnsupportedAppVersionResources_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'resolutionId', 'listUnsupportedAppVersionResources_resolutionId' - The identifier for a specific resolution.
--
-- 'appArn', 'listUnsupportedAppVersionResources_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'listUnsupportedAppVersionResources_appVersion' - The version of the application.
newListUnsupportedAppVersionResources ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ListUnsupportedAppVersionResources
newListUnsupportedAppVersionResources
  pAppArn_
  pAppVersion_ =
    ListUnsupportedAppVersionResources'
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
listUnsupportedAppVersionResources_maxResults :: Lens.Lens' ListUnsupportedAppVersionResources (Prelude.Maybe Prelude.Natural)
listUnsupportedAppVersionResources_maxResults = Lens.lens (\ListUnsupportedAppVersionResources' {maxResults} -> maxResults) (\s@ListUnsupportedAppVersionResources' {} a -> s {maxResults = a} :: ListUnsupportedAppVersionResources)

-- | Null, or the token from a previous call to get the next set of results.
listUnsupportedAppVersionResources_nextToken :: Lens.Lens' ListUnsupportedAppVersionResources (Prelude.Maybe Prelude.Text)
listUnsupportedAppVersionResources_nextToken = Lens.lens (\ListUnsupportedAppVersionResources' {nextToken} -> nextToken) (\s@ListUnsupportedAppVersionResources' {} a -> s {nextToken = a} :: ListUnsupportedAppVersionResources)

-- | The identifier for a specific resolution.
listUnsupportedAppVersionResources_resolutionId :: Lens.Lens' ListUnsupportedAppVersionResources (Prelude.Maybe Prelude.Text)
listUnsupportedAppVersionResources_resolutionId = Lens.lens (\ListUnsupportedAppVersionResources' {resolutionId} -> resolutionId) (\s@ListUnsupportedAppVersionResources' {} a -> s {resolutionId = a} :: ListUnsupportedAppVersionResources)

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
listUnsupportedAppVersionResources_appArn :: Lens.Lens' ListUnsupportedAppVersionResources Prelude.Text
listUnsupportedAppVersionResources_appArn = Lens.lens (\ListUnsupportedAppVersionResources' {appArn} -> appArn) (\s@ListUnsupportedAppVersionResources' {} a -> s {appArn = a} :: ListUnsupportedAppVersionResources)

-- | The version of the application.
listUnsupportedAppVersionResources_appVersion :: Lens.Lens' ListUnsupportedAppVersionResources Prelude.Text
listUnsupportedAppVersionResources_appVersion = Lens.lens (\ListUnsupportedAppVersionResources' {appVersion} -> appVersion) (\s@ListUnsupportedAppVersionResources' {} a -> s {appVersion = a} :: ListUnsupportedAppVersionResources)

instance
  Core.AWSRequest
    ListUnsupportedAppVersionResources
  where
  type
    AWSResponse ListUnsupportedAppVersionResources =
      ListUnsupportedAppVersionResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUnsupportedAppVersionResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "resolutionId")
            Prelude.<*> ( x
                            Data..?> "unsupportedResources"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListUnsupportedAppVersionResources
  where
  hashWithSalt
    _salt
    ListUnsupportedAppVersionResources' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` resolutionId
        `Prelude.hashWithSalt` appArn
        `Prelude.hashWithSalt` appVersion

instance
  Prelude.NFData
    ListUnsupportedAppVersionResources
  where
  rnf ListUnsupportedAppVersionResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resolutionId
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance
  Data.ToHeaders
    ListUnsupportedAppVersionResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListUnsupportedAppVersionResources
  where
  toJSON ListUnsupportedAppVersionResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resolutionId" Data..=) Prelude.<$> resolutionId,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance
  Data.ToPath
    ListUnsupportedAppVersionResources
  where
  toPath =
    Prelude.const
      "/list-unsupported-app-version-resources"

instance
  Data.ToQuery
    ListUnsupportedAppVersionResources
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUnsupportedAppVersionResourcesResponse' smart constructor.
data ListUnsupportedAppVersionResourcesResponse = ListUnsupportedAppVersionResourcesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier for a specific resolution.
    resolutionId :: Prelude.Text,
    -- | The unsupported resources for the application.
    unsupportedResources :: [UnsupportedResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUnsupportedAppVersionResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUnsupportedAppVersionResourcesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listUnsupportedAppVersionResourcesResponse_httpStatus' - The response's http status code.
--
-- 'resolutionId', 'listUnsupportedAppVersionResourcesResponse_resolutionId' - The identifier for a specific resolution.
--
-- 'unsupportedResources', 'listUnsupportedAppVersionResourcesResponse_unsupportedResources' - The unsupported resources for the application.
newListUnsupportedAppVersionResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resolutionId'
  Prelude.Text ->
  ListUnsupportedAppVersionResourcesResponse
newListUnsupportedAppVersionResourcesResponse
  pHttpStatus_
  pResolutionId_ =
    ListUnsupportedAppVersionResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        resolutionId = pResolutionId_,
        unsupportedResources =
          Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no more
-- results.
listUnsupportedAppVersionResourcesResponse_nextToken :: Lens.Lens' ListUnsupportedAppVersionResourcesResponse (Prelude.Maybe Prelude.Text)
listUnsupportedAppVersionResourcesResponse_nextToken = Lens.lens (\ListUnsupportedAppVersionResourcesResponse' {nextToken} -> nextToken) (\s@ListUnsupportedAppVersionResourcesResponse' {} a -> s {nextToken = a} :: ListUnsupportedAppVersionResourcesResponse)

-- | The response's http status code.
listUnsupportedAppVersionResourcesResponse_httpStatus :: Lens.Lens' ListUnsupportedAppVersionResourcesResponse Prelude.Int
listUnsupportedAppVersionResourcesResponse_httpStatus = Lens.lens (\ListUnsupportedAppVersionResourcesResponse' {httpStatus} -> httpStatus) (\s@ListUnsupportedAppVersionResourcesResponse' {} a -> s {httpStatus = a} :: ListUnsupportedAppVersionResourcesResponse)

-- | The identifier for a specific resolution.
listUnsupportedAppVersionResourcesResponse_resolutionId :: Lens.Lens' ListUnsupportedAppVersionResourcesResponse Prelude.Text
listUnsupportedAppVersionResourcesResponse_resolutionId = Lens.lens (\ListUnsupportedAppVersionResourcesResponse' {resolutionId} -> resolutionId) (\s@ListUnsupportedAppVersionResourcesResponse' {} a -> s {resolutionId = a} :: ListUnsupportedAppVersionResourcesResponse)

-- | The unsupported resources for the application.
listUnsupportedAppVersionResourcesResponse_unsupportedResources :: Lens.Lens' ListUnsupportedAppVersionResourcesResponse [UnsupportedResource]
listUnsupportedAppVersionResourcesResponse_unsupportedResources = Lens.lens (\ListUnsupportedAppVersionResourcesResponse' {unsupportedResources} -> unsupportedResources) (\s@ListUnsupportedAppVersionResourcesResponse' {} a -> s {unsupportedResources = a} :: ListUnsupportedAppVersionResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListUnsupportedAppVersionResourcesResponse
  where
  rnf ListUnsupportedAppVersionResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resolutionId
      `Prelude.seq` Prelude.rnf unsupportedResources
