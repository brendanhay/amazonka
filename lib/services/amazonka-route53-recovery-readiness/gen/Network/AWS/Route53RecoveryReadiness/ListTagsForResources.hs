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
-- Module      : Network.AWS.Route53RecoveryReadiness.ListTagsForResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the tags assigned to the specified resource.
module Network.AWS.Route53RecoveryReadiness.ListTagsForResources
  ( -- * Creating a Request
    ListTagsForResources (..),
    newListTagsForResources,

    -- * Request Lenses
    listTagsForResources_resourceArn,

    -- * Destructuring the Response
    ListTagsForResourcesResponse (..),
    newListTagsForResourcesResponse,

    -- * Response Lenses
    listTagsForResourcesResponse_tags,
    listTagsForResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryReadiness.Types

-- | /See:/ 'newListTagsForResources' smart constructor.
data ListTagsForResources = ListTagsForResources'
  { -- | The Amazon Resource Name (ARN) for the resource. You can get this from
    -- the response to any request to the resource.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTagsForResources_resourceArn' - The Amazon Resource Name (ARN) for the resource. You can get this from
-- the response to any request to the resource.
newListTagsForResources ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListTagsForResources
newListTagsForResources pResourceArn_ =
  ListTagsForResources' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) for the resource. You can get this from
-- the response to any request to the resource.
listTagsForResources_resourceArn :: Lens.Lens' ListTagsForResources Prelude.Text
listTagsForResources_resourceArn = Lens.lens (\ListTagsForResources' {resourceArn} -> resourceArn) (\s@ListTagsForResources' {} a -> s {resourceArn = a} :: ListTagsForResources)

instance Core.AWSRequest ListTagsForResources where
  type
    AWSResponse ListTagsForResources =
      ListTagsForResourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourcesResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResources

instance Prelude.NFData ListTagsForResources

instance Core.ToHeaders ListTagsForResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListTagsForResources where
  toPath ListTagsForResources' {..} =
    Prelude.mconcat ["/tags/", Core.toBS resourceArn]

instance Core.ToQuery ListTagsForResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForResourcesResponse' smart constructor.
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsForResourcesResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'listTagsForResourcesResponse_httpStatus' - The response's http status code.
newListTagsForResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourcesResponse
newListTagsForResourcesResponse pHttpStatus_ =
  ListTagsForResourcesResponse'
    { tags =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listTagsForResourcesResponse_tags :: Lens.Lens' ListTagsForResourcesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsForResourcesResponse_tags = Lens.lens (\ListTagsForResourcesResponse' {tags} -> tags) (\s@ListTagsForResourcesResponse' {} a -> s {tags = a} :: ListTagsForResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForResourcesResponse_httpStatus :: Lens.Lens' ListTagsForResourcesResponse Prelude.Int
listTagsForResourcesResponse_httpStatus = Lens.lens (\ListTagsForResourcesResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourcesResponse' {} a -> s {httpStatus = a} :: ListTagsForResourcesResponse)

instance Prelude.NFData ListTagsForResourcesResponse
