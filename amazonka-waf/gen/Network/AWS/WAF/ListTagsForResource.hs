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
-- Module      : Network.AWS.WAF.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Retrieves the tags associated with the specified AWS resource. Tags are
-- key:value pairs that you can use to categorize and manage your
-- resources, for purposes like billing. For example, you might set the tag
-- key to \"customer\" and the value to the customer name or ID. You can
-- specify one or more tags to add to each AWS resource, up to 50 tags for
-- a resource.
--
-- Tagging is only available through the API, SDKs, and CLI. You can\'t
-- manage or view tags through the AWS WAF Classic console. You can tag the
-- AWS resources that you manage through AWS WAF Classic: web ACLs, rule
-- groups, and rules.
module Network.AWS.WAF.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_nextMarker,
    listTagsForResource_limit,
    listTagsForResource_resourceARN,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_nextMarker,
    listTagsForResourceResponse_tagInfoForResource,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { nextMarker :: Core.Maybe Core.Text,
    limit :: Core.Maybe Core.Natural,
    resourceARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listTagsForResource_nextMarker' -
--
-- 'limit', 'listTagsForResource_limit' -
--
-- 'resourceARN', 'listTagsForResource_resourceARN' -
newListTagsForResource ::
  -- | 'resourceARN'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { nextMarker = Core.Nothing,
      limit = Core.Nothing,
      resourceARN = pResourceARN_
    }

-- |
listTagsForResource_nextMarker :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Text)
listTagsForResource_nextMarker = Lens.lens (\ListTagsForResource' {nextMarker} -> nextMarker) (\s@ListTagsForResource' {} a -> s {nextMarker = a} :: ListTagsForResource)

-- |
listTagsForResource_limit :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Natural)
listTagsForResource_limit = Lens.lens (\ListTagsForResource' {limit} -> limit) (\s@ListTagsForResource' {} a -> s {limit = a} :: ListTagsForResource)

-- |
listTagsForResource_resourceARN :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceARN = Lens.lens (\ListTagsForResource' {resourceARN} -> resourceARN) (\s@ListTagsForResource' {} a -> s {resourceARN = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "TagInfoForResource")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForResource

instance Core.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListTagsForResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("ResourceARN" Core..= resourceARN)
          ]
      )

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { nextMarker :: Core.Maybe Core.Text,
    tagInfoForResource :: Core.Maybe TagInfoForResource,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listTagsForResourceResponse_nextMarker' -
--
-- 'tagInfoForResource', 'listTagsForResourceResponse_tagInfoForResource' -
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { nextMarker =
        Core.Nothing,
      tagInfoForResource = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
listTagsForResourceResponse_nextMarker :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Core.Text)
listTagsForResourceResponse_nextMarker = Lens.lens (\ListTagsForResourceResponse' {nextMarker} -> nextMarker) (\s@ListTagsForResourceResponse' {} a -> s {nextMarker = a} :: ListTagsForResourceResponse)

-- |
listTagsForResourceResponse_tagInfoForResource :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe TagInfoForResource)
listTagsForResourceResponse_tagInfoForResource = Lens.lens (\ListTagsForResourceResponse' {tagInfoForResource} -> tagInfoForResource) (\s@ListTagsForResourceResponse' {} a -> s {tagInfoForResource = a} :: ListTagsForResourceResponse)

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
