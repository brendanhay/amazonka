{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentResult
  ( BulkDeploymentResult (..),

    -- * Smart constructor
    mkBulkDeploymentResult,

    -- * Lenses
    bdrDeploymentId,
    bdrDeploymentARN,
    bdrCreatedAt,
    bdrDeploymentType,
    bdrErrorDetails,
    bdrGroupARN,
    bdrDeploymentStatus,
    bdrErrorMessage,
  )
where

import Network.AWS.Greengrass.Types.DeploymentType
import Network.AWS.Greengrass.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an individual group deployment in a bulk deployment operation.
--
-- /See:/ 'mkBulkDeploymentResult' smart constructor.
data BulkDeploymentResult = BulkDeploymentResult'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    deploymentARN :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    deploymentType :: Lude.Maybe DeploymentType,
    errorDetails :: Lude.Maybe [ErrorDetail],
    groupARN :: Lude.Maybe Lude.Text,
    deploymentStatus :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkDeploymentResult' with the minimum fields required to make a request.
--
-- * 'createdAt' - The time, in ISO format, when the deployment was created.
-- * 'deploymentARN' - The ARN of the group deployment.
-- * 'deploymentId' - The ID of the group deployment.
-- * 'deploymentStatus' - The current status of the group deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
-- * 'deploymentType' - The type of the deployment.
-- * 'errorDetails' - Details about the error.
-- * 'errorMessage' - The error message for a failed deployment
-- * 'groupARN' - The ARN of the Greengrass group.
mkBulkDeploymentResult ::
  BulkDeploymentResult
mkBulkDeploymentResult =
  BulkDeploymentResult'
    { deploymentId = Lude.Nothing,
      deploymentARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      deploymentType = Lude.Nothing,
      errorDetails = Lude.Nothing,
      groupARN = Lude.Nothing,
      deploymentStatus = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The ID of the group deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentId :: Lens.Lens' BulkDeploymentResult (Lude.Maybe Lude.Text)
bdrDeploymentId = Lens.lens (deploymentId :: BulkDeploymentResult -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The ARN of the group deployment.
--
-- /Note:/ Consider using 'deploymentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentARN :: Lens.Lens' BulkDeploymentResult (Lude.Maybe Lude.Text)
bdrDeploymentARN = Lens.lens (deploymentARN :: BulkDeploymentResult -> Lude.Maybe Lude.Text) (\s a -> s {deploymentARN = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrDeploymentARN "Use generic-lens or generic-optics with 'deploymentARN' instead." #-}

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrCreatedAt :: Lens.Lens' BulkDeploymentResult (Lude.Maybe Lude.Text)
bdrCreatedAt = Lens.lens (createdAt :: BulkDeploymentResult -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentType :: Lens.Lens' BulkDeploymentResult (Lude.Maybe DeploymentType)
bdrDeploymentType = Lens.lens (deploymentType :: BulkDeploymentResult -> Lude.Maybe DeploymentType) (\s a -> s {deploymentType = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

-- | Details about the error.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrErrorDetails :: Lens.Lens' BulkDeploymentResult (Lude.Maybe [ErrorDetail])
bdrErrorDetails = Lens.lens (errorDetails :: BulkDeploymentResult -> Lude.Maybe [ErrorDetail]) (\s a -> s {errorDetails = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The ARN of the Greengrass group.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrGroupARN :: Lens.Lens' BulkDeploymentResult (Lude.Maybe Lude.Text)
bdrGroupARN = Lens.lens (groupARN :: BulkDeploymentResult -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The current status of the group deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrDeploymentStatus :: Lens.Lens' BulkDeploymentResult (Lude.Maybe Lude.Text)
bdrDeploymentStatus = Lens.lens (deploymentStatus :: BulkDeploymentResult -> Lude.Maybe Lude.Text) (\s a -> s {deploymentStatus = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrDeploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead." #-}

-- | The error message for a failed deployment
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdrErrorMessage :: Lens.Lens' BulkDeploymentResult (Lude.Maybe Lude.Text)
bdrErrorMessage = Lens.lens (errorMessage :: BulkDeploymentResult -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BulkDeploymentResult)
{-# DEPRECATED bdrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON BulkDeploymentResult where
  parseJSON =
    Lude.withObject
      "BulkDeploymentResult"
      ( \x ->
          BulkDeploymentResult'
            Lude.<$> (x Lude..:? "DeploymentId")
            Lude.<*> (x Lude..:? "DeploymentArn")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "DeploymentType")
            Lude.<*> (x Lude..:? "ErrorDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "GroupArn")
            Lude.<*> (x Lude..:? "DeploymentStatus")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
