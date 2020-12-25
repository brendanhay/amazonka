{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.AssociateCreatedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a created artifact of an AWS cloud resource, the target receiving the migration, with the migration task performed by a migration tool. This API has the following traits:
--
--
--     * Migration tools can call the @AssociateCreatedArtifact@ operation to indicate which AWS artifact is associated with a migration task.
--
--
--     * The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: @arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b@ .
--
--
--     * Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or DMS endpoint, etc.
module Network.AWS.MigrationHub.AssociateCreatedArtifact
  ( -- * Creating a request
    AssociateCreatedArtifact (..),
    mkAssociateCreatedArtifact,

    -- ** Request lenses
    acaProgressUpdateStream,
    acaMigrationTaskName,
    acaCreatedArtifact,
    acaDryRun,

    -- * Destructuring the response
    AssociateCreatedArtifactResponse (..),
    mkAssociateCreatedArtifactResponse,

    -- ** Response lenses
    acarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateCreatedArtifact' smart constructor.
data AssociateCreatedArtifact = AssociateCreatedArtifact'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
    createdArtifact :: Types.CreatedArtifact,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateCreatedArtifact' value with any optional fields omitted.
mkAssociateCreatedArtifact ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  -- | 'createdArtifact'
  Types.CreatedArtifact ->
  AssociateCreatedArtifact
mkAssociateCreatedArtifact
  progressUpdateStream
  migrationTaskName
  createdArtifact =
    AssociateCreatedArtifact'
      { progressUpdateStream,
        migrationTaskName,
        createdArtifact,
        dryRun = Core.Nothing
      }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaProgressUpdateStream :: Lens.Lens' AssociateCreatedArtifact Types.ProgressUpdateStream
acaProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED acaProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaMigrationTaskName :: Lens.Lens' AssociateCreatedArtifact Types.MigrationTaskName
acaMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED acaMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
--
-- /Note:/ Consider using 'createdArtifact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaCreatedArtifact :: Lens.Lens' AssociateCreatedArtifact Types.CreatedArtifact
acaCreatedArtifact = Lens.field @"createdArtifact"
{-# DEPRECATED acaCreatedArtifact "Use generic-lens or generic-optics with 'createdArtifact' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaDryRun :: Lens.Lens' AssociateCreatedArtifact (Core.Maybe Core.Bool)
acaDryRun = Lens.field @"dryRun"
{-# DEPRECATED acaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON AssociateCreatedArtifact where
  toJSON AssociateCreatedArtifact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just ("CreatedArtifact" Core..= createdArtifact),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest AssociateCreatedArtifact where
  type Rs AssociateCreatedArtifact = AssociateCreatedArtifactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.AssociateCreatedArtifact")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateCreatedArtifactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateCreatedArtifactResponse' smart constructor.
newtype AssociateCreatedArtifactResponse = AssociateCreatedArtifactResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateCreatedArtifactResponse' value with any optional fields omitted.
mkAssociateCreatedArtifactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateCreatedArtifactResponse
mkAssociateCreatedArtifactResponse responseStatus =
  AssociateCreatedArtifactResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarrsResponseStatus :: Lens.Lens' AssociateCreatedArtifactResponse Core.Int
acarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED acarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
