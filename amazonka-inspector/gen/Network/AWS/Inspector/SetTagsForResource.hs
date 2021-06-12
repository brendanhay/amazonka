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
-- Module      : Network.AWS.Inspector.SetTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets tags (key and value pairs) to the assessment template that is
-- specified by the ARN of the assessment template.
module Network.AWS.Inspector.SetTagsForResource
  ( -- * Creating a Request
    SetTagsForResource (..),
    newSetTagsForResource,

    -- * Request Lenses
    setTagsForResource_tags,
    setTagsForResource_resourceArn,

    -- * Destructuring the Response
    SetTagsForResourceResponse (..),
    newSetTagsForResourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetTagsForResource' smart constructor.
data SetTagsForResource = SetTagsForResource'
  { -- | A collection of key and value pairs that you want to set to the
    -- assessment template.
    tags :: Core.Maybe [Tag],
    -- | The ARN of the assessment template that you want to set tags to.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'setTagsForResource_tags' - A collection of key and value pairs that you want to set to the
-- assessment template.
--
-- 'resourceArn', 'setTagsForResource_resourceArn' - The ARN of the assessment template that you want to set tags to.
newSetTagsForResource ::
  -- | 'resourceArn'
  Core.Text ->
  SetTagsForResource
newSetTagsForResource pResourceArn_ =
  SetTagsForResource'
    { tags = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | A collection of key and value pairs that you want to set to the
-- assessment template.
setTagsForResource_tags :: Lens.Lens' SetTagsForResource (Core.Maybe [Tag])
setTagsForResource_tags = Lens.lens (\SetTagsForResource' {tags} -> tags) (\s@SetTagsForResource' {} a -> s {tags = a} :: SetTagsForResource) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the assessment template that you want to set tags to.
setTagsForResource_resourceArn :: Lens.Lens' SetTagsForResource Core.Text
setTagsForResource_resourceArn = Lens.lens (\SetTagsForResource' {resourceArn} -> resourceArn) (\s@SetTagsForResource' {} a -> s {resourceArn = a} :: SetTagsForResource)

instance Core.AWSRequest SetTagsForResource where
  type
    AWSResponse SetTagsForResource =
      SetTagsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetTagsForResourceResponse'

instance Core.Hashable SetTagsForResource

instance Core.NFData SetTagsForResource

instance Core.ToHeaders SetTagsForResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.SetTagsForResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetTagsForResource where
  toJSON SetTagsForResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("resourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath SetTagsForResource where
  toPath = Core.const "/"

instance Core.ToQuery SetTagsForResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetTagsForResourceResponse' smart constructor.
data SetTagsForResourceResponse = SetTagsForResourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTagsForResourceResponse ::
  SetTagsForResourceResponse
newSetTagsForResourceResponse =
  SetTagsForResourceResponse'

instance Core.NFData SetTagsForResourceResponse
