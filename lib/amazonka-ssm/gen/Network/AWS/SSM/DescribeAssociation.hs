{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the association for the specified target or instance. If you created the association by using the @Targets@ parameter, then you must retrieve the association by using the association ID. If you created the association by specifying an instance ID and a Systems Manager document, then you retrieve the association by specifying the document name and the instance ID.
module Network.AWS.SSM.DescribeAssociation
  ( -- * Creating a request
    DescribeAssociation (..),
    mkDescribeAssociation,

    -- ** Request lenses
    daAssociationId,
    daAssociationVersion,
    daInstanceId,
    daName,

    -- * Destructuring the response
    DescribeAssociationResponse (..),
    mkDescribeAssociationResponse,

    -- ** Response lenses
    darfrsAssociationDescription,
    darfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAssociation' smart constructor.
data DescribeAssociation = DescribeAssociation'
  { -- | The association ID for which you want information.
    associationId :: Core.Maybe Types.AssociationId,
    -- | Specify the association version to retrieve. To view the latest version, either specify @> LATEST@ for this parameter, or omit this parameter. To view a list of all associations for an instance, use 'ListAssociations' . To get a list of versions for a specific association, use 'ListAssociationVersions' .
    associationVersion :: Core.Maybe Types.AssociationVersion,
    -- | The instance ID.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Types.DocumentARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAssociation' value with any optional fields omitted.
mkDescribeAssociation ::
  DescribeAssociation
mkDescribeAssociation =
  DescribeAssociation'
    { associationId = Core.Nothing,
      associationVersion = Core.Nothing,
      instanceId = Core.Nothing,
      name = Core.Nothing
    }

-- | The association ID for which you want information.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAssociationId :: Lens.Lens' DescribeAssociation (Core.Maybe Types.AssociationId)
daAssociationId = Lens.field @"associationId"
{-# DEPRECATED daAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | Specify the association version to retrieve. To view the latest version, either specify @> LATEST@ for this parameter, or omit this parameter. To view a list of all associations for an instance, use 'ListAssociations' . To get a list of versions for a specific association, use 'ListAssociationVersions' .
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAssociationVersion :: Lens.Lens' DescribeAssociation (Core.Maybe Types.AssociationVersion)
daAssociationVersion = Lens.field @"associationVersion"
{-# DEPRECATED daAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daInstanceId :: Lens.Lens' DescribeAssociation (Core.Maybe Types.InstanceId)
daInstanceId = Lens.field @"instanceId"
{-# DEPRECATED daInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DescribeAssociation (Core.Maybe Types.DocumentARN)
daName = Lens.field @"name"
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeAssociation where
  toJSON DescribeAssociation {..} =
    Core.object
      ( Core.catMaybes
          [ ("AssociationId" Core..=) Core.<$> associationId,
            ("AssociationVersion" Core..=) Core.<$> associationVersion,
            ("InstanceId" Core..=) Core.<$> instanceId,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest DescribeAssociation where
  type Rs DescribeAssociation = DescribeAssociationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeAssociation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssociationResponse'
            Core.<$> (x Core..:? "AssociationDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAssociationResponse' smart constructor.
data DescribeAssociationResponse = DescribeAssociationResponse'
  { -- | Information about the association.
    associationDescription :: Core.Maybe Types.AssociationDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAssociationResponse' value with any optional fields omitted.
mkDescribeAssociationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAssociationResponse
mkDescribeAssociationResponse responseStatus =
  DescribeAssociationResponse'
    { associationDescription =
        Core.Nothing,
      responseStatus
    }

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsAssociationDescription :: Lens.Lens' DescribeAssociationResponse (Core.Maybe Types.AssociationDescription)
darfrsAssociationDescription = Lens.field @"associationDescription"
{-# DEPRECATED darfrsAssociationDescription "Use generic-lens or generic-optics with 'associationDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsResponseStatus :: Lens.Lens' DescribeAssociationResponse Core.Int
darfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
