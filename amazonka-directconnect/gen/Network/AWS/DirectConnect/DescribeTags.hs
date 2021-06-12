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
-- Module      : Network.AWS.DirectConnect.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified AWS Direct Connect
-- resources.
module Network.AWS.DirectConnect.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_resourceArns,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_resourceTags,
    describeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: [Core.Text]
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
-- 'resourceArns', 'describeTags_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags' {resourceArns = Core.mempty}

-- | The Amazon Resource Names (ARNs) of the resources.
describeTags_resourceArns :: Lens.Lens' DescribeTags [Core.Text]
describeTags_resourceArns = Lens.lens (\DescribeTags' {resourceArns} -> resourceArns) (\s@DescribeTags' {} a -> s {resourceArns = a} :: DescribeTags) Core.. Lens._Coerce

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Core.<$> (x Core..?> "resourceTags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTags

instance Core.NFData DescribeTags

instance Core.ToHeaders DescribeTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.DescribeTags" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("resourceArns" Core..= resourceArns)]
      )

instance Core.ToPath DescribeTags where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | Information about the tags.
    resourceTags :: Core.Maybe [ResourceTag],
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
-- 'resourceTags', 'describeTagsResponse_resourceTags' - Information about the tags.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { resourceTags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags.
describeTagsResponse_resourceTags :: Lens.Lens' DescribeTagsResponse (Core.Maybe [ResourceTag])
describeTagsResponse_resourceTags = Lens.lens (\DescribeTagsResponse' {resourceTags} -> resourceTags) (\s@DescribeTagsResponse' {} a -> s {resourceTags = a} :: DescribeTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Core.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Core.NFData DescribeTagsResponse
