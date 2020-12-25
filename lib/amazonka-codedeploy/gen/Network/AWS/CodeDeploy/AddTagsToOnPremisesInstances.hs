{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to on-premises instances.
module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
  ( -- * Creating a request
    AddTagsToOnPremisesInstances (..),
    mkAddTagsToOnPremisesInstances,

    -- ** Request lenses
    attopiTags,
    attopiInstanceNames,

    -- * Destructuring the response
    AddTagsToOnPremisesInstancesResponse (..),
    mkAddTagsToOnPremisesInstancesResponse,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of, and adds tags to, an on-premises instance operation.
--
-- /See:/ 'mkAddTagsToOnPremisesInstances' smart constructor.
data AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstances'
  { -- | The tag key-value pairs to add to the on-premises instances.
    --
    -- Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
    tags :: [Types.Tag],
    -- | The names of the on-premises instances to which to add tags.
    instanceNames :: [Types.InstanceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToOnPremisesInstances' value with any optional fields omitted.
mkAddTagsToOnPremisesInstances ::
  AddTagsToOnPremisesInstances
mkAddTagsToOnPremisesInstances =
  AddTagsToOnPremisesInstances'
    { tags = Core.mempty,
      instanceNames = Core.mempty
    }

-- | The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be null or empty strings. Value-only tags are not allowed.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attopiTags :: Lens.Lens' AddTagsToOnPremisesInstances [Types.Tag]
attopiTags = Lens.field @"tags"
{-# DEPRECATED attopiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The names of the on-premises instances to which to add tags.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attopiInstanceNames :: Lens.Lens' AddTagsToOnPremisesInstances [Types.InstanceName]
attopiInstanceNames = Lens.field @"instanceNames"
{-# DEPRECATED attopiInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

instance Core.FromJSON AddTagsToOnPremisesInstances where
  toJSON AddTagsToOnPremisesInstances {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("tags" Core..= tags),
            Core.Just ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.AWSRequest AddTagsToOnPremisesInstances where
  type
    Rs AddTagsToOnPremisesInstances =
      AddTagsToOnPremisesInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeDeploy_20141006.AddTagsToOnPremisesInstances"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull AddTagsToOnPremisesInstancesResponse'

-- | /See:/ 'mkAddTagsToOnPremisesInstancesResponse' smart constructor.
data AddTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToOnPremisesInstancesResponse' value with any optional fields omitted.
mkAddTagsToOnPremisesInstancesResponse ::
  AddTagsToOnPremisesInstancesResponse
mkAddTagsToOnPremisesInstancesResponse =
  AddTagsToOnPremisesInstancesResponse'
