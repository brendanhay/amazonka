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
-- Module      : Network.AWS.RAM.ListPendingInvitationResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources in a resource share that is shared with you but that
-- the invitation is still pending for.
module Network.AWS.RAM.ListPendingInvitationResources
  ( -- * Creating a Request
    ListPendingInvitationResources (..),
    newListPendingInvitationResources,

    -- * Request Lenses
    listPendingInvitationResources_nextToken,
    listPendingInvitationResources_maxResults,
    listPendingInvitationResources_resourceShareInvitationArn,

    -- * Destructuring the Response
    ListPendingInvitationResourcesResponse (..),
    newListPendingInvitationResourcesResponse,

    -- * Response Lenses
    listPendingInvitationResourcesResponse_resources,
    listPendingInvitationResourcesResponse_nextToken,
    listPendingInvitationResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPendingInvitationResources' smart constructor.
data ListPendingInvitationResources = ListPendingInvitationResources'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the invitation.
    resourceShareInvitationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPendingInvitationResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPendingInvitationResources_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listPendingInvitationResources_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'resourceShareInvitationArn', 'listPendingInvitationResources_resourceShareInvitationArn' - The Amazon Resource Name (ARN) of the invitation.
newListPendingInvitationResources ::
  -- | 'resourceShareInvitationArn'
  Prelude.Text ->
  ListPendingInvitationResources
newListPendingInvitationResources
  pResourceShareInvitationArn_ =
    ListPendingInvitationResources'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        resourceShareInvitationArn =
          pResourceShareInvitationArn_
      }

-- | The token for the next page of results.
listPendingInvitationResources_nextToken :: Lens.Lens' ListPendingInvitationResources (Prelude.Maybe Prelude.Text)
listPendingInvitationResources_nextToken = Lens.lens (\ListPendingInvitationResources' {nextToken} -> nextToken) (\s@ListPendingInvitationResources' {} a -> s {nextToken = a} :: ListPendingInvitationResources)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listPendingInvitationResources_maxResults :: Lens.Lens' ListPendingInvitationResources (Prelude.Maybe Prelude.Natural)
listPendingInvitationResources_maxResults = Lens.lens (\ListPendingInvitationResources' {maxResults} -> maxResults) (\s@ListPendingInvitationResources' {} a -> s {maxResults = a} :: ListPendingInvitationResources)

-- | The Amazon Resource Name (ARN) of the invitation.
listPendingInvitationResources_resourceShareInvitationArn :: Lens.Lens' ListPendingInvitationResources Prelude.Text
listPendingInvitationResources_resourceShareInvitationArn = Lens.lens (\ListPendingInvitationResources' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@ListPendingInvitationResources' {} a -> s {resourceShareInvitationArn = a} :: ListPendingInvitationResources)

instance
  Core.AWSRequest
    ListPendingInvitationResources
  where
  type
    AWSResponse ListPendingInvitationResources =
      ListPendingInvitationResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPendingInvitationResourcesResponse'
            Prelude.<$> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPendingInvitationResources

instance
  Prelude.NFData
    ListPendingInvitationResources

instance
  Core.ToHeaders
    ListPendingInvitationResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPendingInvitationResources where
  toJSON ListPendingInvitationResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "resourceShareInvitationArn"
                  Core..= resourceShareInvitationArn
              )
          ]
      )

instance Core.ToPath ListPendingInvitationResources where
  toPath =
    Prelude.const "/listpendinginvitationresources"

instance Core.ToQuery ListPendingInvitationResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPendingInvitationResourcesResponse' smart constructor.
data ListPendingInvitationResourcesResponse = ListPendingInvitationResourcesResponse'
  { -- | Information about the resources included the resource share.
    resources :: Prelude.Maybe [Resource],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPendingInvitationResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'listPendingInvitationResourcesResponse_resources' - Information about the resources included the resource share.
--
-- 'nextToken', 'listPendingInvitationResourcesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listPendingInvitationResourcesResponse_httpStatus' - The response's http status code.
newListPendingInvitationResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPendingInvitationResourcesResponse
newListPendingInvitationResourcesResponse
  pHttpStatus_ =
    ListPendingInvitationResourcesResponse'
      { resources =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the resources included the resource share.
listPendingInvitationResourcesResponse_resources :: Lens.Lens' ListPendingInvitationResourcesResponse (Prelude.Maybe [Resource])
listPendingInvitationResourcesResponse_resources = Lens.lens (\ListPendingInvitationResourcesResponse' {resources} -> resources) (\s@ListPendingInvitationResourcesResponse' {} a -> s {resources = a} :: ListPendingInvitationResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listPendingInvitationResourcesResponse_nextToken :: Lens.Lens' ListPendingInvitationResourcesResponse (Prelude.Maybe Prelude.Text)
listPendingInvitationResourcesResponse_nextToken = Lens.lens (\ListPendingInvitationResourcesResponse' {nextToken} -> nextToken) (\s@ListPendingInvitationResourcesResponse' {} a -> s {nextToken = a} :: ListPendingInvitationResourcesResponse)

-- | The response's http status code.
listPendingInvitationResourcesResponse_httpStatus :: Lens.Lens' ListPendingInvitationResourcesResponse Prelude.Int
listPendingInvitationResourcesResponse_httpStatus = Lens.lens (\ListPendingInvitationResourcesResponse' {httpStatus} -> httpStatus) (\s@ListPendingInvitationResourcesResponse' {} a -> s {httpStatus = a} :: ListPendingInvitationResourcesResponse)

instance
  Prelude.NFData
    ListPendingInvitationResourcesResponse
