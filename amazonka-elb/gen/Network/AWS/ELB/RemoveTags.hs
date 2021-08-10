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
-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified load balancer.
module Network.AWS.ELB.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_loadBalancerNames,
    removeTags_tags,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,

    -- * Response Lenses
    removeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The name of the load balancer. You can specify a maximum of one load
    -- balancer name.
    loadBalancerNames :: [Prelude.Text],
    -- | The list of tag keys to remove.
    tags :: Prelude.NonEmpty TagKeyOnly
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerNames', 'removeTags_loadBalancerNames' - The name of the load balancer. You can specify a maximum of one load
-- balancer name.
--
-- 'tags', 'removeTags_tags' - The list of tag keys to remove.
newRemoveTags ::
  -- | 'tags'
  Prelude.NonEmpty TagKeyOnly ->
  RemoveTags
newRemoveTags pTags_ =
  RemoveTags'
    { loadBalancerNames = Prelude.mempty,
      tags = Lens._Coerce Lens.# pTags_
    }

-- | The name of the load balancer. You can specify a maximum of one load
-- balancer name.
removeTags_loadBalancerNames :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_loadBalancerNames = Lens.lens (\RemoveTags' {loadBalancerNames} -> loadBalancerNames) (\s@RemoveTags' {} a -> s {loadBalancerNames = a} :: RemoveTags) Prelude.. Lens._Coerce

-- | The list of tag keys to remove.
removeTags_tags :: Lens.Lens' RemoveTags (Prelude.NonEmpty TagKeyOnly)
removeTags_tags = Lens.lens (\RemoveTags' {tags} -> tags) (\s@RemoveTags' {} a -> s {tags = a} :: RemoveTags) Prelude.. Lens._Coerce

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RemoveTagsResult"
      ( \s h x ->
          RemoveTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTags

instance Prelude.NFData RemoveTags

instance Core.ToHeaders RemoveTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RemoveTags where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTags where
  toQuery RemoveTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RemoveTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerNames"
          Core.=: Core.toQueryList "member" loadBalancerNames,
        "Tags" Core.=: Core.toQueryList "member" tags
      ]

-- | Contains the output of RemoveTags.
--
-- /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsResponse_httpStatus' - The response's http status code.
newRemoveTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsResponse
newRemoveTagsResponse pHttpStatus_ =
  RemoveTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeTagsResponse_httpStatus :: Lens.Lens' RemoveTagsResponse Prelude.Int
removeTagsResponse_httpStatus = Lens.lens (\RemoveTagsResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsResponse' {} a -> s {httpStatus = a} :: RemoveTagsResponse)

instance Prelude.NFData RemoveTagsResponse
