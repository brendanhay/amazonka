{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DisassociateCreatedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a created artifact of an AWS resource with a migration task performed by a migration tool that was previously associated. This API has the following traits:
--
--
--     * A migration user can call the @DisassociateCreatedArtifacts@ operation to disassociate a created AWS Artifact from a migration task.
--
--
--     * The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: @arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b@ .
--
--
--     * Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or RDS instance, etc.
module Network.AWS.MigrationHub.DisassociateCreatedArtifact
  ( -- * Creating a request
    DisassociateCreatedArtifact (..),
    mkDisassociateCreatedArtifact,

    -- ** Request lenses
    dcaProgressUpdateStream,
    dcaMigrationTaskName,
    dcaCreatedArtifactName,
    dcaDryRun,

    -- * Destructuring the response
    DisassociateCreatedArtifactResponse (..),
    mkDisassociateCreatedArtifactResponse,

    -- ** Response lenses
    dcarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateCreatedArtifact' smart constructor.
data DisassociateCreatedArtifact = DisassociateCreatedArtifact'
  { -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | Unique identifier that references the migration task to be disassociated with the artifact. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
    createdArtifactName :: Types.CreatedArtifactName,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateCreatedArtifact' value with any optional fields omitted.
mkDisassociateCreatedArtifact ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  -- | 'createdArtifactName'
  Types.CreatedArtifactName ->
  DisassociateCreatedArtifact
mkDisassociateCreatedArtifact
  progressUpdateStream
  migrationTaskName
  createdArtifactName =
    DisassociateCreatedArtifact'
      { progressUpdateStream,
        migrationTaskName,
        createdArtifactName,
        dryRun = Core.Nothing
      }

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaProgressUpdateStream :: Lens.Lens' DisassociateCreatedArtifact Types.ProgressUpdateStream
dcaProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED dcaProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task to be disassociated with the artifact. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaMigrationTaskName :: Lens.Lens' DisassociateCreatedArtifact Types.MigrationTaskName
dcaMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED dcaMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
--
-- /Note:/ Consider using 'createdArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaCreatedArtifactName :: Lens.Lens' DisassociateCreatedArtifact Types.CreatedArtifactName
dcaCreatedArtifactName = Lens.field @"createdArtifactName"
{-# DEPRECATED dcaCreatedArtifactName "Use generic-lens or generic-optics with 'createdArtifactName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaDryRun :: Lens.Lens' DisassociateCreatedArtifact (Core.Maybe Core.Bool)
dcaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON DisassociateCreatedArtifact where
  toJSON DisassociateCreatedArtifact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just ("CreatedArtifactName" Core..= createdArtifactName),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest DisassociateCreatedArtifact where
  type
    Rs DisassociateCreatedArtifact =
      DisassociateCreatedArtifactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.DisassociateCreatedArtifact")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateCreatedArtifactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateCreatedArtifactResponse' smart constructor.
newtype DisassociateCreatedArtifactResponse = DisassociateCreatedArtifactResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateCreatedArtifactResponse' value with any optional fields omitted.
mkDisassociateCreatedArtifactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateCreatedArtifactResponse
mkDisassociateCreatedArtifactResponse responseStatus =
  DisassociateCreatedArtifactResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarrsResponseStatus :: Lens.Lens' DisassociateCreatedArtifactResponse Core.Int
dcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
