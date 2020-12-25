{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeployment
  ( BulkDeployment (..),

    -- * Smart constructor
    mkBulkDeployment,

    -- * Lenses
    bdBulkDeploymentArn,
    bdBulkDeploymentId,
    bdCreatedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a bulk deployment. You cannot start a new bulk deployment while another one is still running or in a non-terminal state.
--
-- /See:/ 'mkBulkDeployment' smart constructor.
data BulkDeployment = BulkDeployment'
  { -- | The ARN of the bulk deployment.
    bulkDeploymentArn :: Core.Maybe Core.Text,
    -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Maybe Core.Text,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BulkDeployment' value with any optional fields omitted.
mkBulkDeployment ::
  BulkDeployment
mkBulkDeployment =
  BulkDeployment'
    { bulkDeploymentArn = Core.Nothing,
      bulkDeploymentId = Core.Nothing,
      createdAt = Core.Nothing
    }

-- | The ARN of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBulkDeploymentArn :: Lens.Lens' BulkDeployment (Core.Maybe Core.Text)
bdBulkDeploymentArn = Lens.field @"bulkDeploymentArn"
{-# DEPRECATED bdBulkDeploymentArn "Use generic-lens or generic-optics with 'bulkDeploymentArn' instead." #-}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBulkDeploymentId :: Lens.Lens' BulkDeployment (Core.Maybe Core.Text)
bdBulkDeploymentId = Lens.field @"bulkDeploymentId"
{-# DEPRECATED bdBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdCreatedAt :: Lens.Lens' BulkDeployment (Core.Maybe Core.Text)
bdCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED bdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

instance Core.FromJSON BulkDeployment where
  parseJSON =
    Core.withObject "BulkDeployment" Core.$
      \x ->
        BulkDeployment'
          Core.<$> (x Core..:? "BulkDeploymentArn")
          Core.<*> (x Core..:? "BulkDeploymentId")
          Core.<*> (x Core..:? "CreatedAt")
