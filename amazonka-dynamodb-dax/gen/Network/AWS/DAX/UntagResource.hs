{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The name of the DAX resource from which the tags should be removed.
    resourceName :: Prelude.Text,
    -- | A list of tag keys. If the DAX cluster has any tags with these keys,
    -- then the tags are removed from the cluster.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UntagResource
newUntagResource pResourceName_ =
  UntagResource'
    { resourceName = pResourceName_,
      tagKeys = Prelude.mempty
    }

-- | The name of the DAX resource from which the tags should be removed.
untagResource_resourceName :: Lens.Lens' UntagResource Prelude.Text
untagResource_resourceName = Lens.lens (\UntagResource' {resourceName} -> resourceName) (\s@UntagResource' {} a -> s {resourceName = a} :: UntagResource)

-- | A list of tag keys. If the DAX cluster has any tags with these keys,
-- then the tags are removed from the cluster.
untagResource_tagKeys :: Lens.Lens' UntagResource [Prelude.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResourceResponse'
            Prelude.<$> (x Prelude..?> "Tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagResource

instance Prelude.NFData UntagResource

instance Prelude.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonDAXV3.UntagResource" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceName" Prelude..= resourceName),
            Prelude.Just ("TagKeys" Prelude..= tagKeys)
          ]
      )

instance Prelude.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | The tag keys that have been removed from the cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UntagResourceResponse
newUntagResourceResponse pHttpStatus_ =
  UntagResourceResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tag keys that have been removed from the cluster.
untagResourceResponse_tags :: Lens.Lens' UntagResourceResponse (Prelude.Maybe [Tag])
untagResourceResponse_tags = Lens.lens (\UntagResourceResponse' {tags} -> tags) (\s@UntagResourceResponse' {} a -> s {tags = a} :: UntagResourceResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
untagResourceResponse_httpStatus :: Lens.Lens' UntagResourceResponse Prelude.Int
untagResourceResponse_httpStatus = Lens.lens (\UntagResourceResponse' {httpStatus} -> httpStatus) (\s@UntagResourceResponse' {} a -> s {httpStatus = a} :: UntagResourceResponse)

instance Prelude.NFData UntagResourceResponse
