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
-- Module      : Network.AWS.Organizations.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes any tags with the specified keys from the specified resource.
--
-- You can attach tags to the following resources in AWS Organizations.
--
-- -   AWS account
--
-- -   Organization root
--
-- -   Organizational unit (OU)
--
-- -   Policy (any type)
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.UntagResource
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
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The ID of the resource to remove a tag from.
    --
    -- You can specify any of the following taggable resources.
    --
    -- -   AWS account – specify the account ID number.
    --
    -- -   Organizational unit – specify the OU ID that begins with @ou-@ and
    --     looks similar to: @ou-1a2b-34uvwxyz @
    --
    -- -   Root – specify the root ID that begins with @r-@ and looks similar
    --     to: @r-1a2b @
    --
    -- -   Policy – specify the policy ID that begins with @p-@ andlooks
    --     similar to: @p-12abcdefg3 @
    resourceId :: Core.Text,
    -- | The list of keys for tags to remove from the specified resource.
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
-- 'resourceId', 'untagResource_resourceId' - The ID of the resource to remove a tag from.
--
-- You can specify any of the following taggable resources.
--
-- -   AWS account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-1a2b-34uvwxyz @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-1a2b @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-12abcdefg3 @
--
-- 'tagKeys', 'untagResource_tagKeys' - The list of keys for tags to remove from the specified resource.
newUntagResource ::
  -- | 'resourceId'
  Core.Text ->
  UntagResource
newUntagResource pResourceId_ =
  UntagResource'
    { resourceId = pResourceId_,
      tagKeys = Core.mempty
    }

-- | The ID of the resource to remove a tag from.
--
-- You can specify any of the following taggable resources.
--
-- -   AWS account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-1a2b-34uvwxyz @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-1a2b @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-12abcdefg3 @
untagResource_resourceId :: Lens.Lens' UntagResource Core.Text
untagResource_resourceId = Lens.lens (\UntagResource' {resourceId} -> resourceId) (\s@UntagResource' {} a -> s {resourceId = a} :: UntagResource)

-- | The list of keys for tags to remove from the specified resource.
untagResource_tagKeys :: Lens.Lens' UntagResource [Core.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Core.. Lens._Coerce

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UntagResourceResponse'

instance Core.Hashable UntagResource

instance Core.NFData UntagResource

instance Core.ToHeaders UntagResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.UntagResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath UntagResource where
  toPath = Core.const "/"

instance Core.ToQuery UntagResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Core.NFData UntagResourceResponse
