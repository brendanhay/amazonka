{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociation
  ( InstanceAssociation (..),

    -- * Smart constructor
    mkInstanceAssociation,

    -- * Lenses
    iaAssociationId,
    iaInstanceId,
    iaContent,
    iaAssociationVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One or more association documents on the instance.
--
-- /See:/ 'mkInstanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
  { -- | The association ID.
    associationId :: Lude.Maybe Lude.Text,
    -- | The instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The content of the association document for the instance(s).
    content :: Lude.Maybe Lude.Text,
    -- | Version information for the association on the instance.
    associationVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID.
-- * 'instanceId' - The instance ID.
-- * 'content' - The content of the association document for the instance(s).
-- * 'associationVersion' - Version information for the association on the instance.
mkInstanceAssociation ::
  InstanceAssociation
mkInstanceAssociation =
  InstanceAssociation'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      content = Lude.Nothing,
      associationVersion = Lude.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAssociationId :: Lens.Lens' InstanceAssociation (Lude.Maybe Lude.Text)
iaAssociationId = Lens.lens (associationId :: InstanceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: InstanceAssociation)
{-# DEPRECATED iaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInstanceId :: Lens.Lens' InstanceAssociation (Lude.Maybe Lude.Text)
iaInstanceId = Lens.lens (instanceId :: InstanceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceAssociation)
{-# DEPRECATED iaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The content of the association document for the instance(s).
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaContent :: Lens.Lens' InstanceAssociation (Lude.Maybe Lude.Text)
iaContent = Lens.lens (content :: InstanceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: InstanceAssociation)
{-# DEPRECATED iaContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | Version information for the association on the instance.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAssociationVersion :: Lens.Lens' InstanceAssociation (Lude.Maybe Lude.Text)
iaAssociationVersion = Lens.lens (associationVersion :: InstanceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: InstanceAssociation)
{-# DEPRECATED iaAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

instance Lude.FromJSON InstanceAssociation where
  parseJSON =
    Lude.withObject
      "InstanceAssociation"
      ( \x ->
          InstanceAssociation'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Content")
            Lude.<*> (x Lude..:? "AssociationVersion")
      )
