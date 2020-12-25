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
    iaAssociationVersion,
    iaContent,
    iaInstanceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.Content as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types

-- | One or more association documents on the instance.
--
-- /See:/ 'mkInstanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
  { -- | The association ID.
    associationId :: Core.Maybe Types.AssociationId,
    -- | Version information for the association on the instance.
    associationVersion :: Core.Maybe Types.AssociationVersion,
    -- | The content of the association document for the instance(s).
    content :: Core.Maybe Types.Content,
    -- | The instance ID.
    instanceId :: Core.Maybe Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceAssociation' value with any optional fields omitted.
mkInstanceAssociation ::
  InstanceAssociation
mkInstanceAssociation =
  InstanceAssociation'
    { associationId = Core.Nothing,
      associationVersion = Core.Nothing,
      content = Core.Nothing,
      instanceId = Core.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAssociationId :: Lens.Lens' InstanceAssociation (Core.Maybe Types.AssociationId)
iaAssociationId = Lens.field @"associationId"
{-# DEPRECATED iaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | Version information for the association on the instance.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAssociationVersion :: Lens.Lens' InstanceAssociation (Core.Maybe Types.AssociationVersion)
iaAssociationVersion = Lens.field @"associationVersion"
{-# DEPRECATED iaAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The content of the association document for the instance(s).
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaContent :: Lens.Lens' InstanceAssociation (Core.Maybe Types.Content)
iaContent = Lens.field @"content"
{-# DEPRECATED iaContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInstanceId :: Lens.Lens' InstanceAssociation (Core.Maybe Types.InstanceId)
iaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED iaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON InstanceAssociation where
  parseJSON =
    Core.withObject "InstanceAssociation" Core.$
      \x ->
        InstanceAssociation'
          Core.<$> (x Core..:? "AssociationId")
          Core.<*> (x Core..:? "AssociationVersion")
          Core.<*> (x Core..:? "Content")
          Core.<*> (x Core..:? "InstanceId")
