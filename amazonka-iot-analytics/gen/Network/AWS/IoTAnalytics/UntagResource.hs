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
-- Module      : Network.AWS.IoTAnalytics.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the given tags (metadata) from the resource.
module Network.AWS.IoTAnalytics.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,

    -- * Response Lenses
    untagResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The ARN of the resource whose tags you want to remove.
    resourceArn :: Core.Text,
    -- | The keys of those tags which you want to remove.
    tagKeys :: Core.NonEmpty Core.Text
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
-- 'resourceArn', 'untagResource_resourceArn' - The ARN of the resource whose tags you want to remove.
--
-- 'tagKeys', 'untagResource_tagKeys' - The keys of those tags which you want to remove.
newUntagResource ::
  -- | 'resourceArn'
  Core.Text ->
  -- | 'tagKeys'
  Core.NonEmpty Core.Text ->
  UntagResource
newUntagResource pResourceArn_ pTagKeys_ =
  UntagResource'
    { resourceArn = pResourceArn_,
      tagKeys = Lens._Coerce Lens.# pTagKeys_
    }

-- | The ARN of the resource whose tags you want to remove.
untagResource_resourceArn :: Lens.Lens' UntagResource Core.Text
untagResource_resourceArn = Lens.lens (\UntagResource' {resourceArn} -> resourceArn) (\s@UntagResource' {} a -> s {resourceArn = a} :: UntagResource)

-- | The keys of those tags which you want to remove.
untagResource_tagKeys :: Lens.Lens' UntagResource (Core.NonEmpty Core.Text)
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Core.. Lens._Coerce

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UntagResource

instance Core.NFData UntagResource

instance Core.ToHeaders UntagResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UntagResource where
  toPath = Core.const "/tags"

instance Core.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Core.mconcat
      [ "resourceArn" Core.=: resourceArn,
        "tagKeys" Core.=: Core.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'untagResourceResponse_httpStatus' - The response's http status code.
newUntagResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UntagResourceResponse
newUntagResourceResponse pHttpStatus_ =
  UntagResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
untagResourceResponse_httpStatus :: Lens.Lens' UntagResourceResponse Core.Int
untagResourceResponse_httpStatus = Lens.lens (\UntagResourceResponse' {httpStatus} -> httpStatus) (\s@UntagResourceResponse' {} a -> s {httpStatus = a} :: UntagResourceResponse)

instance Core.NFData UntagResourceResponse
