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
-- Module      : Network.AWS.Glue.ListDevEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all @DevEndpoint@ resources in this AWS account,
-- or the resources with the specified tag. This operation allows you to
-- see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a
-- filter on the response so that tagged resources can be retrieved as a
-- group. If you choose to use tags filtering, only resources with the tag
-- are retrieved.
module Network.AWS.Glue.ListDevEndpoints
  ( -- * Creating a Request
    ListDevEndpoints (..),
    newListDevEndpoints,

    -- * Request Lenses
    listDevEndpoints_nextToken,
    listDevEndpoints_maxResults,
    listDevEndpoints_tags,

    -- * Destructuring the Response
    ListDevEndpointsResponse (..),
    newListDevEndpointsResponse,

    -- * Response Lenses
    listDevEndpointsResponse_nextToken,
    listDevEndpointsResponse_devEndpointNames,
    listDevEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDevEndpoints' smart constructor.
data ListDevEndpoints = ListDevEndpoints'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of a list to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies to return only these tagged resources.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevEndpoints_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'listDevEndpoints_maxResults' - The maximum size of a list to return.
--
-- 'tags', 'listDevEndpoints_tags' - Specifies to return only these tagged resources.
newListDevEndpoints ::
  ListDevEndpoints
newListDevEndpoints =
  ListDevEndpoints'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      tags = Core.Nothing
    }

-- | A continuation token, if this is a continuation request.
listDevEndpoints_nextToken :: Lens.Lens' ListDevEndpoints (Core.Maybe Core.Text)
listDevEndpoints_nextToken = Lens.lens (\ListDevEndpoints' {nextToken} -> nextToken) (\s@ListDevEndpoints' {} a -> s {nextToken = a} :: ListDevEndpoints)

-- | The maximum size of a list to return.
listDevEndpoints_maxResults :: Lens.Lens' ListDevEndpoints (Core.Maybe Core.Natural)
listDevEndpoints_maxResults = Lens.lens (\ListDevEndpoints' {maxResults} -> maxResults) (\s@ListDevEndpoints' {} a -> s {maxResults = a} :: ListDevEndpoints)

-- | Specifies to return only these tagged resources.
listDevEndpoints_tags :: Lens.Lens' ListDevEndpoints (Core.Maybe (Core.HashMap Core.Text Core.Text))
listDevEndpoints_tags = Lens.lens (\ListDevEndpoints' {tags} -> tags) (\s@ListDevEndpoints' {} a -> s {tags = a} :: ListDevEndpoints) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest ListDevEndpoints where
  type
    AWSResponse ListDevEndpoints =
      ListDevEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevEndpointsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "DevEndpointNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDevEndpoints

instance Core.NFData ListDevEndpoints

instance Core.ToHeaders ListDevEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListDevEndpoints" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDevEndpoints where
  toJSON ListDevEndpoints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath ListDevEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery ListDevEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDevEndpointsResponse' smart constructor.
data ListDevEndpointsResponse = ListDevEndpointsResponse'
  { -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Core.Maybe Core.Text,
    -- | The names of all the @DevEndpoint@s in the account, or the
    -- @DevEndpoint@s with the specified tags.
    devEndpointNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevEndpointsResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'devEndpointNames', 'listDevEndpointsResponse_devEndpointNames' - The names of all the @DevEndpoint@s in the account, or the
-- @DevEndpoint@s with the specified tags.
--
-- 'httpStatus', 'listDevEndpointsResponse_httpStatus' - The response's http status code.
newListDevEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDevEndpointsResponse
newListDevEndpointsResponse pHttpStatus_ =
  ListDevEndpointsResponse'
    { nextToken = Core.Nothing,
      devEndpointNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list does not contain the last
-- metric available.
listDevEndpointsResponse_nextToken :: Lens.Lens' ListDevEndpointsResponse (Core.Maybe Core.Text)
listDevEndpointsResponse_nextToken = Lens.lens (\ListDevEndpointsResponse' {nextToken} -> nextToken) (\s@ListDevEndpointsResponse' {} a -> s {nextToken = a} :: ListDevEndpointsResponse)

-- | The names of all the @DevEndpoint@s in the account, or the
-- @DevEndpoint@s with the specified tags.
listDevEndpointsResponse_devEndpointNames :: Lens.Lens' ListDevEndpointsResponse (Core.Maybe [Core.Text])
listDevEndpointsResponse_devEndpointNames = Lens.lens (\ListDevEndpointsResponse' {devEndpointNames} -> devEndpointNames) (\s@ListDevEndpointsResponse' {} a -> s {devEndpointNames = a} :: ListDevEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDevEndpointsResponse_httpStatus :: Lens.Lens' ListDevEndpointsResponse Core.Int
listDevEndpointsResponse_httpStatus = Lens.lens (\ListDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListDevEndpointsResponse' {} a -> s {httpStatus = a} :: ListDevEndpointsResponse)

instance Core.NFData ListDevEndpointsResponse
