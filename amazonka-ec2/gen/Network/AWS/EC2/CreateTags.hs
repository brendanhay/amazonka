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
-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites only the specified tags for the specified Amazon EC2
-- resource or resources. When you specify an existing tag key, the value
-- is overwritten with the new value. Each resource can have a maximum of
-- 50 tags. Each tag consists of a key and optional value. Tag keys must be
-- unique per resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/. For more information
-- about creating IAM policies that control users\' access to resources
-- based on tags, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-iam-actions-resources.html Supported Resource-Level Permissions for Amazon EC2 API Actions>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateTags
  ( -- * Creating a Request
    CreateTags (..),
    newCreateTags,

    -- * Request Lenses
    createTags_dryRun,
    createTags_resources,
    createTags_tags,

    -- * Destructuring the Response
    CreateTagsResponse (..),
    newCreateTagsResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the resources, separated by spaces.
    --
    -- Constraints: Up to 1000 resource IDs. We recommend breaking up this
    -- request into smaller batches.
    resources :: [Core.Text],
    -- | The tags. The @value@ parameter is required, but if you don\'t want the
    -- tag to have a value, specify the parameter with no value, and we set the
    -- value to an empty string.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTags_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'resources', 'createTags_resources' - The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this
-- request into smaller batches.
--
-- 'tags', 'createTags_tags' - The tags. The @value@ parameter is required, but if you don\'t want the
-- tag to have a value, specify the parameter with no value, and we set the
-- value to an empty string.
newCreateTags ::
  CreateTags
newCreateTags =
  CreateTags'
    { dryRun = Core.Nothing,
      resources = Core.mempty,
      tags = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTags_dryRun :: Lens.Lens' CreateTags (Core.Maybe Core.Bool)
createTags_dryRun = Lens.lens (\CreateTags' {dryRun} -> dryRun) (\s@CreateTags' {} a -> s {dryRun = a} :: CreateTags)

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this
-- request into smaller batches.
createTags_resources :: Lens.Lens' CreateTags [Core.Text]
createTags_resources = Lens.lens (\CreateTags' {resources} -> resources) (\s@CreateTags' {} a -> s {resources = a} :: CreateTags) Core.. Lens._Coerce

-- | The tags. The @value@ parameter is required, but if you don\'t want the
-- tag to have a value, specify the parameter with no value, and we set the
-- value to an empty string.
createTags_tags :: Lens.Lens' CreateTags [Tag]
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Core.. Lens._Coerce

instance Core.AWSRequest CreateTags where
  type AWSResponse CreateTags = CreateTagsResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull CreateTagsResponse'

instance Core.Hashable CreateTags

instance Core.NFData CreateTags

instance Core.ToHeaders CreateTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTags where
  toPath = Core.const "/"

instance Core.ToQuery CreateTags where
  toQuery CreateTags' {..} =
    Core.mconcat
      [ "Action" Core.=: ("CreateTags" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "ResourceId" resources,
        Core.toQueryList "Tag" tags
      ]

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateTagsResponse ::
  CreateTagsResponse
newCreateTagsResponse = CreateTagsResponse'

instance Core.NFData CreateTagsResponse
