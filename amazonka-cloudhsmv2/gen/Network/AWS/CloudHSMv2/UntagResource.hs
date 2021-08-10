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
-- Module      : Network.AWS.CloudHSMv2.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tag or tags from the specified AWS CloudHSM
-- cluster.
module Network.AWS.CloudHSMv2.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceId,
    untagResource_tagKeyList,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,

    -- * Response Lenses
    untagResourceResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The cluster identifier (ID) for the cluster whose tags you are removing.
    -- To find the cluster ID, use DescribeClusters.
    resourceId :: Prelude.Text,
    -- | A list of one or more tag keys for the tags that you are removing.
    -- Specify only the tag keys, not the tag values.
    tagKeyList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'untagResource_resourceId' - The cluster identifier (ID) for the cluster whose tags you are removing.
-- To find the cluster ID, use DescribeClusters.
--
-- 'tagKeyList', 'untagResource_tagKeyList' - A list of one or more tag keys for the tags that you are removing.
-- Specify only the tag keys, not the tag values.
newUntagResource ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'tagKeyList'
  Prelude.NonEmpty Prelude.Text ->
  UntagResource
newUntagResource pResourceId_ pTagKeyList_ =
  UntagResource'
    { resourceId = pResourceId_,
      tagKeyList = Lens._Coerce Lens.# pTagKeyList_
    }

-- | The cluster identifier (ID) for the cluster whose tags you are removing.
-- To find the cluster ID, use DescribeClusters.
untagResource_resourceId :: Lens.Lens' UntagResource Prelude.Text
untagResource_resourceId = Lens.lens (\UntagResource' {resourceId} -> resourceId) (\s@UntagResource' {} a -> s {resourceId = a} :: UntagResource)

-- | A list of one or more tag keys for the tags that you are removing.
-- Specify only the tag keys, not the tag values.
untagResource_tagKeyList :: Lens.Lens' UntagResource (Prelude.NonEmpty Prelude.Text)
untagResource_tagKeyList = Lens.lens (\UntagResource' {tagKeyList} -> tagKeyList) (\s@UntagResource' {} a -> s {tagKeyList = a} :: UntagResource) Prelude.. Lens._Coerce

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagResource

instance Prelude.NFData UntagResource

instance Core.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.UntagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just ("TagKeyList" Core..= tagKeyList)
          ]
      )

instance Core.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Core.ToQuery UntagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UntagResourceResponse
newUntagResourceResponse pHttpStatus_ =
  UntagResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
untagResourceResponse_httpStatus :: Lens.Lens' UntagResourceResponse Prelude.Int
untagResourceResponse_httpStatus = Lens.lens (\UntagResourceResponse' {httpStatus} -> httpStatus) (\s@UntagResourceResponse' {} a -> s {httpStatus = a} :: UntagResourceResponse)

instance Prelude.NFData UntagResourceResponse
