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
-- Module      : Network.AWS.EFS.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from an EFS resource. You can remove tags from EFS file
-- systems and access points using this API operation.
--
-- This operation requires permissions for the
-- @elasticfilesystem:UntagResource@ action.
module Network.AWS.EFS.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceId,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | Specifies the EFS resource that you want to remove tags from.
    resourceId :: Prelude.Text,
    -- | The keys of the key-value tag pairs that you want to remove from the
    -- specified EFS resource.
    tagKeys :: Prelude.NonEmpty Prelude.Text
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
-- 'resourceId', 'untagResource_resourceId' - Specifies the EFS resource that you want to remove tags from.
--
-- 'tagKeys', 'untagResource_tagKeys' - The keys of the key-value tag pairs that you want to remove from the
-- specified EFS resource.
newUntagResource ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  UntagResource
newUntagResource pResourceId_ pTagKeys_ =
  UntagResource'
    { resourceId = pResourceId_,
      tagKeys = Lens._Coerce Lens.# pTagKeys_
    }

-- | Specifies the EFS resource that you want to remove tags from.
untagResource_resourceId :: Lens.Lens' UntagResource Prelude.Text
untagResource_resourceId = Lens.lens (\UntagResource' {resourceId} -> resourceId) (\s@UntagResource' {} a -> s {resourceId = a} :: UntagResource)

-- | The keys of the key-value tag pairs that you want to remove from the
-- specified EFS resource.
untagResource_tagKeys :: Lens.Lens' UntagResource (Prelude.NonEmpty Prelude.Text)
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Prelude.. Lens._Coerce

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource

instance Prelude.NFData UntagResource

instance Core.ToHeaders UntagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UntagResource where
  toPath UntagResource' {..} =
    Prelude.mconcat
      ["/2015-02-01/resource-tags/", Core.toBS resourceId]

instance Core.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Prelude.mconcat
      ["tagKeys" Core.=: Core.toQueryList "member" tagKeys]

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Prelude.NFData UntagResourceResponse
