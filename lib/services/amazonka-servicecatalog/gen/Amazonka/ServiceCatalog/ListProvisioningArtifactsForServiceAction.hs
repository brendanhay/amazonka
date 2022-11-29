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
-- Module      : Amazonka.ServiceCatalog.ListProvisioningArtifactsForServiceAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the
-- specified self-service action.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListProvisioningArtifactsForServiceAction
  ( -- * Creating a Request
    ListProvisioningArtifactsForServiceAction (..),
    newListProvisioningArtifactsForServiceAction,

    -- * Request Lenses
    listProvisioningArtifactsForServiceAction_pageToken,
    listProvisioningArtifactsForServiceAction_pageSize,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListProvisioningArtifactsForServiceAction' smart constructor.
data ListProvisioningArtifactsForServiceAction = ListProvisioningArtifactsForServiceAction'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisioningArtifactsForServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'listProvisioningArtifactsForServiceAction_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'listProvisioningArtifactsForServiceAction_pageSize' - The maximum number of items to return with this call.
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
  Prelude.Text ->
  ListProvisioningArtifactsForServiceAction
newListProvisioningArtifactsForServiceAction
  pServiceActionId_ =
    ListProvisioningArtifactsForServiceAction'
      { pageToken =
          Prelude.Nothing,
        pageSize = Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        serviceActionId =
          pServiceActionId_
      }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listProvisioningArtifactsForServiceAction_pageToken :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Prelude.Maybe Prelude.Text)
listProvisioningArtifactsForServiceAction_pageToken = Lens.lens (\ListProvisioningArtifactsForServiceAction' {pageToken} -> pageToken) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {pageToken = a} :: ListProvisioningArtifactsForServiceAction)

-- | The maximum number of items to return with this call.
listProvisioningArtifactsForServiceAction_pageSize :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Prelude.Maybe Prelude.Natural)
listProvisioningArtifactsForServiceAction_pageSize = Lens.lens (\ListProvisioningArtifactsForServiceAction' {pageSize} -> pageSize) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {pageSize = a} :: ListProvisioningArtifactsForServiceAction)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listProvisioningArtifactsForServiceAction_acceptLanguage :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Prelude.Maybe Prelude.Text)
listProvisioningArtifactsForServiceAction_acceptLanguage = Lens.lens (\ListProvisioningArtifactsForServiceAction' {acceptLanguage} -> acceptLanguage) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {acceptLanguage = a} :: ListProvisioningArtifactsForServiceAction)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
listProvisioningArtifactsForServiceAction_serviceActionId :: Lens.Lens' ListProvisioningArtifactsForServiceAction Prelude.Text
listProvisioningArtifactsForServiceAction_serviceActionId = Lens.lens (\ListProvisioningArtifactsForServiceAction' {serviceActionId} -> serviceActionId) (\s@ListProvisioningArtifactsForServiceAction' {} a -> s {serviceActionId = a} :: ListProvisioningArtifactsForServiceAction)

instance
  Core.AWSPager
    ListProvisioningArtifactsForServiceAction
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisioningArtifactsForServiceActionResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProvisioningArtifactsForServiceAction_pageToken
          Lens..~ rs
            Lens.^? listProvisioningArtifactsForServiceActionResponse_nextPageToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListProvisioningArtifactsForServiceAction
  where
  type
    AWSResponse
      ListProvisioningArtifactsForServiceAction =
      ListProvisioningArtifactsForServiceActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsForServiceActionResponse'
            Prelude.<$> ( x Core..?> "ProvisioningArtifactViews"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextPageToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListProvisioningArtifactsForServiceAction
  where
  hashWithSalt
    _salt
    ListProvisioningArtifactsForServiceAction' {..} =
      _salt `Prelude.hashWithSalt` pageToken
        `Prelude.hashWithSalt` pageSize
        `Prelude.hashWithSalt` acceptLanguage
        `Prelude.hashWithSalt` serviceActionId

instance
  Prelude.NFData
    ListProvisioningArtifactsForServiceAction
  where
  rnf ListProvisioningArtifactsForServiceAction' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf serviceActionId

instance
  Core.ToHeaders
    ListProvisioningArtifactsForServiceAction
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListProvisioningArtifactsForServiceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListProvisioningArtifactsForServiceAction
  where
  toJSON ListProvisioningArtifactsForServiceAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageToken" Core..=) Prelude.<$> pageToken,
            ("PageSize" Core..=) Prelude.<$> pageSize,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just
              ("ServiceActionId" Core..= serviceActionId)
          ]
      )

instance
  Core.ToPath
    ListProvisioningArtifactsForServiceAction
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListProvisioningArtifactsForServiceAction
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProvisioningArtifactsForServiceActionResponse' smart constructor.
data ListProvisioningArtifactsForServiceActionResponse = ListProvisioningArtifactsForServiceActionResponse'
  { -- | An array of objects with information about product views and
    -- provisioning artifacts.
    provisioningArtifactViews :: Prelude.Maybe [ProvisioningArtifactView],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListProvisioningArtifactsForServiceActionResponse
newListProvisioningArtifactsForServiceActionResponse
  pHttpStatus_ =
    ListProvisioningArtifactsForServiceActionResponse'
      { provisioningArtifactViews =
          Prelude.Nothing,
        nextPageToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An array of objects with information about product views and
-- provisioning artifacts.
listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Prelude.Maybe [ProvisioningArtifactView])
listProvisioningArtifactsForServiceActionResponse_provisioningArtifactViews = Lens.lens (\ListProvisioningArtifactsForServiceActionResponse' {provisioningArtifactViews} -> provisioningArtifactViews) (\s@ListProvisioningArtifactsForServiceActionResponse' {} a -> s {provisioningArtifactViews = a} :: ListProvisioningArtifactsForServiceActionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listProvisioningArtifactsForServiceActionResponse_nextPageToken :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Prelude.Maybe Prelude.Text)
listProvisioningArtifactsForServiceActionResponse_nextPageToken = Lens.lens (\ListProvisioningArtifactsForServiceActionResponse' {nextPageToken} -> nextPageToken) (\s@ListProvisioningArtifactsForServiceActionResponse' {} a -> s {nextPageToken = a} :: ListProvisioningArtifactsForServiceActionResponse)

-- | The response's http status code.
listProvisioningArtifactsForServiceActionResponse_httpStatus :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse Prelude.Int
listProvisioningArtifactsForServiceActionResponse_httpStatus = Lens.lens (\ListProvisioningArtifactsForServiceActionResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningArtifactsForServiceActionResponse' {} a -> s {httpStatus = a} :: ListProvisioningArtifactsForServiceActionResponse)

instance
  Prelude.NFData
    ListProvisioningArtifactsForServiceActionResponse
  where
  rnf
    ListProvisioningArtifactsForServiceActionResponse' {..} =
      Prelude.rnf provisioningArtifactViews
        `Prelude.seq` Prelude.rnf nextPageToken
        `Prelude.seq` Prelude.rnf httpStatus
