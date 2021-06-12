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
-- Module      : Network.AWS.CloudTrail.RemoveTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from a trail.
module Network.AWS.CloudTrail.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_tagsList,
    removeTags_resourceId,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,

    -- * Response Lenses
    removeTagsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies the tags to remove from a trail.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | Specifies a list of tags to be removed.
    tagsList :: Core.Maybe [Tag],
    -- | Specifies the ARN of the trail from which tags should be removed. The
    -- format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagsList', 'removeTags_tagsList' - Specifies a list of tags to be removed.
--
-- 'resourceId', 'removeTags_resourceId' - Specifies the ARN of the trail from which tags should be removed. The
-- format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newRemoveTags ::
  -- | 'resourceId'
  Core.Text ->
  RemoveTags
newRemoveTags pResourceId_ =
  RemoveTags'
    { tagsList = Core.Nothing,
      resourceId = pResourceId_
    }

-- | Specifies a list of tags to be removed.
removeTags_tagsList :: Lens.Lens' RemoveTags (Core.Maybe [Tag])
removeTags_tagsList = Lens.lens (\RemoveTags' {tagsList} -> tagsList) (\s@RemoveTags' {} a -> s {tagsList = a} :: RemoveTags) Core.. Lens.mapping Lens._Coerce

-- | Specifies the ARN of the trail from which tags should be removed. The
-- format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
removeTags_resourceId :: Lens.Lens' RemoveTags Core.Text
removeTags_resourceId = Lens.lens (\RemoveTags' {resourceId} -> resourceId) (\s@RemoveTags' {} a -> s {resourceId = a} :: RemoveTags)

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveTags

instance Core.NFData RemoveTags

instance Core.ToHeaders RemoveTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.RemoveTags" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagsList" Core..=) Core.<$> tagsList,
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath RemoveTags where
  toPath = Core.const "/"

instance Core.ToQuery RemoveTags where
  toQuery = Core.const Core.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RemoveTagsResponse
newRemoveTagsResponse pHttpStatus_ =
  RemoveTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeTagsResponse_httpStatus :: Lens.Lens' RemoveTagsResponse Core.Int
removeTagsResponse_httpStatus = Lens.lens (\RemoveTagsResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsResponse' {} a -> s {httpStatus = a} :: RemoveTagsResponse)

instance Core.NFData RemoveTagsResponse
