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
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the
-- specified self-service action.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
  ( -- * Creating a Request
    ListProvisioningArtifactsForServiceAction (..),
    newListProvisioningArtifactsForServiceAction,

    -- * Request Lenses
    listProvisioningArtifactsForServiceAction_pageSize,
    listProvisioningArtifactsForServiceAction_pageToken,
    listProvisioningArtifactsForServiceAction_acceptLanguage,
    listProvisioningArtifactsForServiceAction_serviceActionId,

    -- * Destructuring the Response
    ListProvisioningArtifactsForServiceActionResponse (..),
    newListProvisioningArtifactsForServiceActionResponse,

    -- * Response Lenses
    listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews,
    listProvisioningArtifactsForServiceActionResponse_nextPageToken,
    listProvisioningArtifactsForServiceActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListProvisioningArtifactsForServiceAction' smart constructor.
data ListProvisioningArtifactsForServiceAction = ListProvisioningArtifactsForServiceAction'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProvisioningArtifactsForServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listProvisioningArtifactsForServiceAction_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listProvisioningArtifactsForServiceAction_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listProvisioningArtifactsForServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'serviceActionId', 'listProvisioningArtifactsForServiceAction_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
newListProvisioningArtifactsForServiceAction ::
  -- | 'serviceActionId'
  Core.Text ->
  ListProvisioningArtifactsForServiceAction
newListProvisioningArtifactsForServiceAction
  pServiceActionId_ =
    ListProvisioningArtifactsForServiceAction'
      { pageSize =
          Core.Nothing,
        pageToken = Core.Nothing,
        acceptLanguage = Core.Nothing,
        serviceActionId =
          pServiceActionId_
      }

-- | The maximum number of items to return with this call.
listProvisioningArtifactsForServiceAction_pageSize :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Core.Maybe Core.Natural)
listProvisioningArtifactsForServiceAction_pageSize = Lens.lens (\ListProvisioningArtifactsForServiceAction' {pageSize} -> pageSize) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {pageSize = a} :: ListProvisioningArtifactsForServiceAction)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listProvisioningArtifactsForServiceAction_pageToken :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Core.Maybe Core.Text)
listProvisioningArtifactsForServiceAction_pageToken = Lens.lens (\ListProvisioningArtifactsForServiceAction' {pageToken} -> pageToken) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {pageToken = a} :: ListProvisioningArtifactsForServiceAction)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listProvisioningArtifactsForServiceAction_acceptLanguage :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Core.Maybe Core.Text)
listProvisioningArtifactsForServiceAction_acceptLanguage = Lens.lens (\ListProvisioningArtifactsForServiceAction' {acceptLanguage} -> acceptLanguage) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {acceptLanguage = a} :: ListProvisioningArtifactsForServiceAction)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
listProvisioningArtifactsForServiceAction_serviceActionId :: Lens.Lens' ListProvisioningArtifactsForServiceAction Core.Text
listProvisioningArtifactsForServiceAction_serviceActionId = Lens.lens (\ListProvisioningArtifactsForServiceAction' {serviceActionId} -> serviceActionId) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {serviceActionId = a} :: ListProvisioningArtifactsForServiceAction)

instance
  Core.AWSPager
    ListProvisioningArtifactsForServiceAction
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisioningArtifactsForServiceActionResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProvisioningArtifactsForServiceAction_pageToken
          Lens..~ rs
            Lens.^? listProvisioningArtifactsForServiceActionResponse_nextPageToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    ListProvisioningArtifactsForServiceAction
  where
  type
    AWSResponse
      ListProvisioningArtifactsForServiceAction =
      ListProvisioningArtifactsForServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsForServiceActionResponse'
            Core.<$> ( x Core..?> "ProvisioningArtifactViews"
                         Core..!@ Core.mempty
                     )
              Core.<*> (x Core..?> "NextPageToken")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListProvisioningArtifactsForServiceAction

instance
  Core.NFData
    ListProvisioningArtifactsForServiceAction

instance
  Core.ToHeaders
    ListProvisioningArtifactsForServiceAction
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListProvisioningArtifactsForServiceAction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListProvisioningArtifactsForServiceAction
  where
  toJSON ListProvisioningArtifactsForServiceAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just
              ("ServiceActionId" Core..= serviceActionId)
          ]
      )

instance
  Core.ToPath
    ListProvisioningArtifactsForServiceAction
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListProvisioningArtifactsForServiceAction
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProvisioningArtifactsForServiceActionResponse' smart constructor.
data ListProvisioningArtifactsForServiceActionResponse = ListProvisioningArtifactsForServiceActionResponse'
  { -- | An array of objects with information about product views and
    -- provisioning artifacts.
    provisioningArtifactViews :: Core.Maybe [ProvisioningArtifactView],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProvisioningArtifactsForServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifactViews', 'listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews' - An array of objects with information about product views and
-- provisioning artifacts.
--
-- 'nextPageToken', 'listProvisioningArtifactsForServiceActionResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listProvisioningArtifactsForServiceActionResponse_httpStatus' - The response's http status code.
newListProvisioningArtifactsForServiceActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProvisioningArtifactsForServiceActionResponse
newListProvisioningArtifactsForServiceActionResponse
  pHttpStatus_ =
    ListProvisioningArtifactsForServiceActionResponse'
      { provisioningArtifactViews =
          Core.Nothing,
        nextPageToken =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An array of objects with information about product views and
-- provisioning artifacts.
listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Core.Maybe [ProvisioningArtifactView])
listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews = Lens.lens (\ListProvisioningArtifactsForServiceActionResponse' {provisioningArtifactViews} -> provisioningArtifactViews) (\s@ListProvisioningArtifactsForServiceActionResponse' {} a -> s {provisioningArtifactViews = a} :: ListProvisioningArtifactsForServiceActionResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listProvisioningArtifactsForServiceActionResponse_nextPageToken :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Core.Maybe Core.Text)
listProvisioningArtifactsForServiceActionResponse_nextPageToken = Lens.lens (\ListProvisioningArtifactsForServiceActionResponse' {nextPageToken} -> nextPageToken) (\s@ListProvisioningArtifactsForServiceActionResponse' {} a -> s {nextPageToken = a} :: ListProvisioningArtifactsForServiceActionResponse)

-- | The response's http status code.
listProvisioningArtifactsForServiceActionResponse_httpStatus :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse Core.Int
listProvisioningArtifactsForServiceActionResponse_httpStatus = Lens.lens (\ListProvisioningArtifactsForServiceActionResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningArtifactsForServiceActionResponse' {} a -> s {httpStatus = a} :: ListProvisioningArtifactsForServiceActionResponse)

instance
  Core.NFData
    ListProvisioningArtifactsForServiceActionResponse
