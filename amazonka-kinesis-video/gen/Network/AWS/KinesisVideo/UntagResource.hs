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
-- Module      : Network.AWS.KinesisVideo.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a signaling channel. In the request,
-- specify only a tag key or keys; don\'t specify the value. If you specify
-- a tag key that does not exist, it\'s ignored.
module Network.AWS.KinesisVideo.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceARN,
    untagResource_tagKeyList,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,

    -- * Response Lenses
    untagResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The Amazon Resource Name (ARN) of the signaling channel from which you
    -- want to remove tags.
    resourceARN :: Core.Text,
    -- | A list of the keys of the tags that you want to remove.
    tagKeyList :: Core.NonEmpty Core.Text
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
-- 'resourceARN', 'untagResource_resourceARN' - The Amazon Resource Name (ARN) of the signaling channel from which you
-- want to remove tags.
--
-- 'tagKeyList', 'untagResource_tagKeyList' - A list of the keys of the tags that you want to remove.
newUntagResource ::
  -- | 'resourceARN'
  Core.Text ->
  -- | 'tagKeyList'
  Core.NonEmpty Core.Text ->
  UntagResource
newUntagResource pResourceARN_ pTagKeyList_ =
  UntagResource'
    { resourceARN = pResourceARN_,
      tagKeyList = Lens._Coerce Lens.# pTagKeyList_
    }

-- | The Amazon Resource Name (ARN) of the signaling channel from which you
-- want to remove tags.
untagResource_resourceARN :: Lens.Lens' UntagResource Core.Text
untagResource_resourceARN = Lens.lens (\UntagResource' {resourceARN} -> resourceARN) (\s@UntagResource' {} a -> s {resourceARN = a} :: UntagResource)

-- | A list of the keys of the tags that you want to remove.
untagResource_tagKeyList :: Lens.Lens' UntagResource (Core.NonEmpty Core.Text)
untagResource_tagKeyList = Lens.lens (\UntagResource' {tagKeyList} -> tagKeyList) (\s@UntagResource' {} a -> s {tagKeyList = a} :: UntagResource) Core.. Lens._Coerce

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request = Request.postJSON defaultService
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

instance Core.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("TagKeyList" Core..= tagKeyList)
          ]
      )

instance Core.ToPath UntagResource where
  toPath = Core.const "/UntagResource"

instance Core.ToQuery UntagResource where
  toQuery = Core.const Core.mempty

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
