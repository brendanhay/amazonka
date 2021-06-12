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
-- Module      : Network.AWS.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an Amazon RDS resource. These tags can also be
-- used with cost allocation reporting to track cost associated with Amazon
-- RDS resources, or used in a Condition statement in an IAM policy for
-- Amazon RDS.
--
-- For an overview on tagging Amazon RDS resources, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources>.
module Network.AWS.RDS.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- * Destructuring the Response
    AddTagsToResourceResponse (..),
    newAddTagsToResourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon RDS resource that the tags are added to. This value is an
    -- Amazon Resource Name (ARN). For information about creating an ARN, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
    resourceName :: Core.Text,
    -- | The tags to be assigned to the Amazon RDS resource.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'addTagsToResource_resourceName' - The Amazon RDS resource that the tags are added to. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
--
-- 'tags', 'addTagsToResource_tags' - The tags to be assigned to the Amazon RDS resource.
newAddTagsToResource ::
  -- | 'resourceName'
  Core.Text ->
  AddTagsToResource
newAddTagsToResource pResourceName_ =
  AddTagsToResource'
    { resourceName = pResourceName_,
      tags = Core.mempty
    }

-- | The Amazon RDS resource that the tags are added to. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
addTagsToResource_resourceName :: Lens.Lens' AddTagsToResource Core.Text
addTagsToResource_resourceName = Lens.lens (\AddTagsToResource' {resourceName} -> resourceName) (\s@AddTagsToResource' {} a -> s {resourceName = a} :: AddTagsToResource)

-- | The tags to be assigned to the Amazon RDS resource.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Core.. Lens._Coerce

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddTagsToResourceResponse'

instance Core.Hashable AddTagsToResource

instance Core.NFData AddTagsToResource

instance Core.ToHeaders AddTagsToResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddTagsToResource where
  toPath = Core.const "/"

instance Core.ToQuery AddTagsToResource where
  toQuery AddTagsToResource' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AddTagsToResource" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ResourceName" Core.=: resourceName,
        "Tags" Core.=: Core.toQueryList "Tag" tags
      ]

-- | /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToResourceResponse ::
  AddTagsToResourceResponse
newAddTagsToResourceResponse =
  AddTagsToResourceResponse'

instance Core.NFData AddTagsToResourceResponse
