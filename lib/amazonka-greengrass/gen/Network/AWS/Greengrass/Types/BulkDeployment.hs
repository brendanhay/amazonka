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
    bdBulkDeploymentARN,
    bdBulkDeploymentId,
    bdCreatedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a bulk deployment. You cannot start a new bulk deployment while another one is still running or in a non-terminal state.
--
-- /See:/ 'mkBulkDeployment' smart constructor.
data BulkDeployment = BulkDeployment'
  { bulkDeploymentARN ::
      Lude.Maybe Lude.Text,
    bulkDeploymentId :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BulkDeployment' with the minimum fields required to make a request.
--
-- * 'bulkDeploymentARN' - The ARN of the bulk deployment.
-- * 'bulkDeploymentId' - The ID of the bulk deployment.
-- * 'createdAt' - The time, in ISO format, when the deployment was created.
mkBulkDeployment ::
  BulkDeployment
mkBulkDeployment =
  BulkDeployment'
    { bulkDeploymentARN = Lude.Nothing,
      bulkDeploymentId = Lude.Nothing,
      createdAt = Lude.Nothing
    }

-- | The ARN of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBulkDeploymentARN :: Lens.Lens' BulkDeployment (Lude.Maybe Lude.Text)
bdBulkDeploymentARN = Lens.lens (bulkDeploymentARN :: BulkDeployment -> Lude.Maybe Lude.Text) (\s a -> s {bulkDeploymentARN = a} :: BulkDeployment)
{-# DEPRECATED bdBulkDeploymentARN "Use generic-lens or generic-optics with 'bulkDeploymentARN' instead." #-}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBulkDeploymentId :: Lens.Lens' BulkDeployment (Lude.Maybe Lude.Text)
bdBulkDeploymentId = Lens.lens (bulkDeploymentId :: BulkDeployment -> Lude.Maybe Lude.Text) (\s a -> s {bulkDeploymentId = a} :: BulkDeployment)
{-# DEPRECATED bdBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdCreatedAt :: Lens.Lens' BulkDeployment (Lude.Maybe Lude.Text)
bdCreatedAt = Lens.lens (createdAt :: BulkDeployment -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: BulkDeployment)
{-# DEPRECATED bdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

instance Lude.FromJSON BulkDeployment where
  parseJSON =
    Lude.withObject
      "BulkDeployment"
      ( \x ->
          BulkDeployment'
            Lude.<$> (x Lude..:? "BulkDeploymentArn")
            Lude.<*> (x Lude..:? "BulkDeploymentId")
            Lude.<*> (x Lude..:? "CreatedAt")
      )
