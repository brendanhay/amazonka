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
    dcaCreatedArtifactName,
    dcaProgressUpdateStream,
    dcaMigrationTaskName,
    dcaDryRun,

    -- * Destructuring the response
    DisassociateCreatedArtifactResponse (..),
    mkDisassociateCreatedArtifactResponse,

    -- ** Response lenses
    dcarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateCreatedArtifact' smart constructor.
data DisassociateCreatedArtifact = DisassociateCreatedArtifact'
  { -- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
    createdArtifactName :: Lude.Text,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | Unique identifier that references the migration task to be disassociated with the artifact. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateCreatedArtifact' with the minimum fields required to make a request.
--
-- * 'createdArtifactName' - An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - Unique identifier that references the migration task to be disassociated with the artifact. /Do not store personal data in this field./
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
mkDisassociateCreatedArtifact ::
  -- | 'createdArtifactName'
  Lude.Text ->
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  DisassociateCreatedArtifact
mkDisassociateCreatedArtifact
  pCreatedArtifactName_
  pProgressUpdateStream_
  pMigrationTaskName_ =
    DisassociateCreatedArtifact'
      { createdArtifactName =
          pCreatedArtifactName_,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        dryRun = Lude.Nothing
      }

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
--
-- /Note:/ Consider using 'createdArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaCreatedArtifactName :: Lens.Lens' DisassociateCreatedArtifact Lude.Text
dcaCreatedArtifactName = Lens.lens (createdArtifactName :: DisassociateCreatedArtifact -> Lude.Text) (\s a -> s {createdArtifactName = a} :: DisassociateCreatedArtifact)
{-# DEPRECATED dcaCreatedArtifactName "Use generic-lens or generic-optics with 'createdArtifactName' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaProgressUpdateStream :: Lens.Lens' DisassociateCreatedArtifact Lude.Text
dcaProgressUpdateStream = Lens.lens (progressUpdateStream :: DisassociateCreatedArtifact -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: DisassociateCreatedArtifact)
{-# DEPRECATED dcaProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task to be disassociated with the artifact. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaMigrationTaskName :: Lens.Lens' DisassociateCreatedArtifact Lude.Text
dcaMigrationTaskName = Lens.lens (migrationTaskName :: DisassociateCreatedArtifact -> Lude.Text) (\s a -> s {migrationTaskName = a} :: DisassociateCreatedArtifact)
{-# DEPRECATED dcaMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaDryRun :: Lens.Lens' DisassociateCreatedArtifact (Lude.Maybe Lude.Bool)
dcaDryRun = Lens.lens (dryRun :: DisassociateCreatedArtifact -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateCreatedArtifact)
{-# DEPRECATED dcaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisassociateCreatedArtifact where
  type
    Rs DisassociateCreatedArtifact =
      DisassociateCreatedArtifactResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateCreatedArtifactResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateCreatedArtifact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.DisassociateCreatedArtifact" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateCreatedArtifact where
  toJSON DisassociateCreatedArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CreatedArtifactName" Lude..= createdArtifactName),
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath DisassociateCreatedArtifact where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateCreatedArtifact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateCreatedArtifactResponse' smart constructor.
newtype DisassociateCreatedArtifactResponse = DisassociateCreatedArtifactResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateCreatedArtifactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateCreatedArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateCreatedArtifactResponse
mkDisassociateCreatedArtifactResponse pResponseStatus_ =
  DisassociateCreatedArtifactResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcarsResponseStatus :: Lens.Lens' DisassociateCreatedArtifactResponse Lude.Int
dcarsResponseStatus = Lens.lens (responseStatus :: DisassociateCreatedArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateCreatedArtifactResponse)
{-# DEPRECATED dcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
