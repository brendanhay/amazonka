{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites only the specified tags for the specified Amazon EC2 resource or resources. When you specify an existing tag key, the value is overwritten with the new value. Each resource can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique per resource.
--
-- For more information about tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ . For more information about creating IAM policies that control users' access to resources based on tags, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-iam-actions-resources.html Supported Resource-Level Permissions for Amazon EC2 API Actions> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctResources,
    ctDryRun,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | The IDs of the resources, separated by spaces.
    --
    -- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
    resources :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- * 'resources' - The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tags' - The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
mkCreateTags ::
  CreateTags
mkCreateTags =
  CreateTags'
    { resources = Lude.mempty,
      dryRun = Lude.Nothing,
      tags = Lude.mempty
    }

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this request into smaller batches.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResources :: Lens.Lens' CreateTags [Lude.Text]
ctResources = Lens.lens (resources :: CreateTags -> [Lude.Text]) (\s a -> s {resources = a} :: CreateTags)
{-# DEPRECATED ctResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDryRun :: Lens.Lens' CreateTags (Lude.Maybe Lude.Bool)
ctDryRun = Lens.lens (dryRun :: CreateTags -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTags)
{-# DEPRECATED ctDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags. The @value@ parameter is required, but if you don't want the tag to have a value, specify the parameter with no value, and we set the value to an empty string.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Tag]
ctTags = Lens.lens (tags :: CreateTags -> [Tag]) (\s a -> s {tags = a} :: CreateTags)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull CreateTagsResponse'

instance Lude.ToHeaders CreateTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTags where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTags where
  toQuery CreateTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTags" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "ResourceId" resources,
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "Tag" tags
      ]

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
mkCreateTagsResponse ::
  CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'
