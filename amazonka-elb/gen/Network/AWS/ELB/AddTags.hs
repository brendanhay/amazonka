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
-- Module      : Network.AWS.ELB.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified load balancer. Each load
-- balancer can have a maximum of 10 tags.
--
-- Each tag consists of a key and an optional value. If a tag with the same
-- key is already associated with the load balancer, @AddTags@ updates its
-- value.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_loadBalancerNames,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AddTags.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The name of the load balancer. You can specify one load balancer only.
    loadBalancerNames :: [Core.Text],
    -- | The tags.
    tags :: Core.NonEmpty Tag
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerNames', 'addTags_loadBalancerNames' - The name of the load balancer. You can specify one load balancer only.
--
-- 'tags', 'addTags_tags' - The tags.
newAddTags ::
  -- | 'tags'
  Core.NonEmpty Tag ->
  AddTags
newAddTags pTags_ =
  AddTags'
    { loadBalancerNames = Core.mempty,
      tags = Lens._Coerce Lens.# pTags_
    }

-- | The name of the load balancer. You can specify one load balancer only.
addTags_loadBalancerNames :: Lens.Lens' AddTags [Core.Text]
addTags_loadBalancerNames = Lens.lens (\AddTags' {loadBalancerNames} -> loadBalancerNames) (\s@AddTags' {} a -> s {loadBalancerNames = a} :: AddTags) Core.. Lens._Coerce

-- | The tags.
addTags_tags :: Lens.Lens' AddTags (Core.NonEmpty Tag)
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Core.. Lens._Coerce

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AddTagsResult"
      ( \s h x ->
          AddTagsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddTags

instance Core.NFData AddTags

instance Core.ToHeaders AddTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddTags where
  toPath = Core.const "/"

instance Core.ToQuery AddTags where
  toQuery AddTags' {..} =
    Core.mconcat
      [ "Action" Core.=: ("AddTags" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerNames"
          Core.=: Core.toQueryList "member" loadBalancerNames,
        "Tags" Core.=: Core.toQueryList "member" tags
      ]

-- | Contains the output of AddTags.
--
-- /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Core.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Core.NFData AddTagsResponse
