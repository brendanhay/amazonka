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
-- Module      : Network.AWS.DAX.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association of tags from a DAX resource. You can call
-- @UntagResource@ up to 5 times per second, per account.
module Network.AWS.DAX.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceName,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,

    -- * Response Lenses
    untagResourceResponse_tags,
    untagResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The name of the DAX resource from which the tags should be removed.
    resourceName :: Core.Text,
    -- | A list of tag keys. If the DAX cluster has any tags with these keys,
    -- then the tags are removed from the cluster.
    tagKeys :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'untagResource_resourceName' - The name of the DAX resource from which the tags should be removed.
--
-- 'tagKeys', 'untagResource_tagKeys' - A list of tag keys. If the DAX cluster has any tags with these keys,
-- then the tags are removed from the cluster.
newUntagResource ::
  -- | 'resourceName'
  Core.Text ->
  UntagResource
newUntagResource pResourceName_ =
  UntagResource'
    { resourceName = pResourceName_,
      tagKeys = Core.mempty
    }

-- | The name of the DAX resource from which the tags should be removed.
untagResource_resourceName :: Lens.Lens' UntagResource Core.Text
untagResource_resourceName = Lens.lens (\UntagResource' {resourceName} -> resourceName) (\s@UntagResource' {} a -> s {resourceName = a} :: UntagResource)

-- | A list of tag keys. If the DAX cluster has any tags with these keys,
-- then the tags are removed from the cluster.
untagResource_tagKeys :: Lens.Lens' UntagResource [Core.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Core.. Lens._Coerce

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResourceResponse'
            Core.<$> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UntagResource

instance Core.NFData UntagResource

instance Core.ToHeaders UntagResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonDAXV3.UntagResource" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceName" Core..= resourceName),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath UntagResource where
  toPath = Core.const "/"

instance Core.ToQuery UntagResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | The tag keys that have been removed from the cluster.
    tags :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'untagResourceResponse_tags' - The tag keys that have been removed from the cluster.
--
-- 'httpStatus', 'untagResourceResponse_httpStatus' - The response's http status code.
newUntagResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UntagResourceResponse
newUntagResourceResponse pHttpStatus_ =
  UntagResourceResponse'
    { tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tag keys that have been removed from the cluster.
untagResourceResponse_tags :: Lens.Lens' UntagResourceResponse (Core.Maybe [Tag])
untagResourceResponse_tags = Lens.lens (\UntagResourceResponse' {tags} -> tags) (\s@UntagResourceResponse' {} a -> s {tags = a} :: UntagResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
untagResourceResponse_httpStatus :: Lens.Lens' UntagResourceResponse Core.Int
untagResourceResponse_httpStatus = Lens.lens (\UntagResourceResponse' {httpStatus} -> httpStatus) (\s@UntagResourceResponse' {} a -> s {httpStatus = a} :: UntagResourceResponse)

instance Core.NFData UntagResourceResponse
