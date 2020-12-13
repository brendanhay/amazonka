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
    acaCreatedArtifact,
    acaProgressUpdateStream,
    acaMigrationTaskName,
    acaDryRun,

    -- * Destructuring the response
    AssociateCreatedArtifactResponse (..),
    mkAssociateCreatedArtifactResponse,

    -- ** Response lenses
    acarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateCreatedArtifact' smart constructor.
data AssociateCreatedArtifact = AssociateCreatedArtifact'
  { -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
    createdArtifact :: CreatedArtifact,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateCreatedArtifact' with the minimum fields required to make a request.
--
-- * 'createdArtifact' - An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
mkAssociateCreatedArtifact ::
  -- | 'createdArtifact'
  CreatedArtifact ->
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  AssociateCreatedArtifact
mkAssociateCreatedArtifact
  pCreatedArtifact_
  pProgressUpdateStream_
  pMigrationTaskName_ =
    AssociateCreatedArtifact'
      { createdArtifact = pCreatedArtifact_,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        dryRun = Lude.Nothing
      }

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
--
-- /Note:/ Consider using 'createdArtifact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaCreatedArtifact :: Lens.Lens' AssociateCreatedArtifact CreatedArtifact
acaCreatedArtifact = Lens.lens (createdArtifact :: AssociateCreatedArtifact -> CreatedArtifact) (\s a -> s {createdArtifact = a} :: AssociateCreatedArtifact)
{-# DEPRECATED acaCreatedArtifact "Use generic-lens or generic-optics with 'createdArtifact' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaProgressUpdateStream :: Lens.Lens' AssociateCreatedArtifact Lude.Text
acaProgressUpdateStream = Lens.lens (progressUpdateStream :: AssociateCreatedArtifact -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: AssociateCreatedArtifact)
{-# DEPRECATED acaProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaMigrationTaskName :: Lens.Lens' AssociateCreatedArtifact Lude.Text
acaMigrationTaskName = Lens.lens (migrationTaskName :: AssociateCreatedArtifact -> Lude.Text) (\s a -> s {migrationTaskName = a} :: AssociateCreatedArtifact)
{-# DEPRECATED acaMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaDryRun :: Lens.Lens' AssociateCreatedArtifact (Lude.Maybe Lude.Bool)
acaDryRun = Lens.lens (dryRun :: AssociateCreatedArtifact -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateCreatedArtifact)
{-# DEPRECATED acaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AssociateCreatedArtifact where
  type Rs AssociateCreatedArtifact = AssociateCreatedArtifactResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateCreatedArtifactResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateCreatedArtifact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.AssociateCreatedArtifact" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateCreatedArtifact where
  toJSON AssociateCreatedArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CreatedArtifact" Lude..= createdArtifact),
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath AssociateCreatedArtifact where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateCreatedArtifact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateCreatedArtifactResponse' smart constructor.
newtype AssociateCreatedArtifactResponse = AssociateCreatedArtifactResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateCreatedArtifactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateCreatedArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateCreatedArtifactResponse
mkAssociateCreatedArtifactResponse pResponseStatus_ =
  AssociateCreatedArtifactResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarsResponseStatus :: Lens.Lens' AssociateCreatedArtifactResponse Lude.Int
acarsResponseStatus = Lens.lens (responseStatus :: AssociateCreatedArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateCreatedArtifactResponse)
{-# DEPRECATED acarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
