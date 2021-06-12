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
-- Module      : Network.AWS.Config.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes specified tags from a resource.
module Network.AWS.Config.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The Amazon Resource Name (ARN) that identifies the resource for which to
    -- list the tags. Currently, the supported resources are @ConfigRule@,
    -- @ConfigurationAggregator@ and @AggregatorAuthorization@.
    resourceArn :: Core.Text,
    -- | The keys of the tags to be removed.
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
-- 'resourceArn', 'untagResource_resourceArn' - The Amazon Resource Name (ARN) that identifies the resource for which to
-- list the tags. Currently, the supported resources are @ConfigRule@,
-- @ConfigurationAggregator@ and @AggregatorAuthorization@.
--
-- 'tagKeys', 'untagResource_tagKeys' - The keys of the tags to be removed.
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

-- | The Amazon Resource Name (ARN) that identifies the resource for which to
-- list the tags. Currently, the supported resources are @ConfigRule@,
-- @ConfigurationAggregator@ and @AggregatorAuthorization@.
untagResource_resourceArn :: Lens.Lens' UntagResource Core.Text
untagResource_resourceArn = Lens.lens (\UntagResource' {resourceArn} -> resourceArn) (\s@UntagResource' {} a -> s {resourceArn = a} :: UntagResource)

-- | The keys of the tags to be removed.
untagResource_tagKeys :: Lens.Lens' UntagResource (Core.NonEmpty Core.Text)
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
              Core.=# ( "StarlingDoveService.UntagResource" ::
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
          [ Core.Just ("ResourceArn" Core..= resourceArn),
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
