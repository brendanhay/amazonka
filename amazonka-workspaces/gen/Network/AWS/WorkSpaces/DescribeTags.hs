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
-- Module      : Network.AWS.WorkSpaces.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for the specified WorkSpaces resource.
module Network.AWS.WorkSpaces.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_resourceId,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_tagList,
    describeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The identifier of the WorkSpaces resource. The supported resource types
    -- are WorkSpaces, registered directories, images, custom bundles, IP
    -- access control groups, and connection aliases.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeTags_resourceId' - The identifier of the WorkSpaces resource. The supported resource types
-- are WorkSpaces, registered directories, images, custom bundles, IP
-- access control groups, and connection aliases.
newDescribeTags ::
  -- | 'resourceId'
  Core.Text ->
  DescribeTags
newDescribeTags pResourceId_ =
  DescribeTags' {resourceId = pResourceId_}

-- | The identifier of the WorkSpaces resource. The supported resource types
-- are WorkSpaces, registered directories, images, custom bundles, IP
-- access control groups, and connection aliases.
describeTags_resourceId :: Lens.Lens' DescribeTags Core.Text
describeTags_resourceId = Lens.lens (\DescribeTags' {resourceId} -> resourceId) (\s@DescribeTags' {} a -> s {resourceId = a} :: DescribeTags)

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Core.<$> (x Core..?> "TagList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTags

instance Core.NFData DescribeTags

instance Core.ToHeaders DescribeTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeTags" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ResourceId" Core..= resourceId)]
      )

instance Core.ToPath DescribeTags where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The tags.
    tagList :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'describeTagsResponse_tagList' - The tags.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { tagList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags.
describeTagsResponse_tagList :: Lens.Lens' DescribeTagsResponse (Core.Maybe [Tag])
describeTagsResponse_tagList = Lens.lens (\DescribeTagsResponse' {tagList} -> tagList) (\s@DescribeTagsResponse' {} a -> s {tagList = a} :: DescribeTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Core.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Core.NFData DescribeTagsResponse
