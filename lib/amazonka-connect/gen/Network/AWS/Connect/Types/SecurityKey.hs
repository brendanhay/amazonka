{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.SecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityKey
  ( SecurityKey (..),

    -- * Smart constructor
    mkSecurityKey,

    -- * Lenses
    skAssociationId,
    skCreationTime,
    skKey,
  )
where

import qualified Network.AWS.Connect.Types.AssociationId as Types
import qualified Network.AWS.Connect.Types.PEM as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information of the security key.
--
-- /See:/ 'mkSecurityKey' smart constructor.
data SecurityKey = SecurityKey'
  { -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Core.Maybe Types.AssociationId,
    -- | When the security key was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The key of the security key.
    key :: Core.Maybe Types.PEM
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SecurityKey' value with any optional fields omitted.
mkSecurityKey ::
  SecurityKey
mkSecurityKey =
  SecurityKey'
    { associationId = Core.Nothing,
      creationTime = Core.Nothing,
      key = Core.Nothing
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skAssociationId :: Lens.Lens' SecurityKey (Core.Maybe Types.AssociationId)
skAssociationId = Lens.field @"associationId"
{-# DEPRECATED skAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | When the security key was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skCreationTime :: Lens.Lens' SecurityKey (Core.Maybe Core.NominalDiffTime)
skCreationTime = Lens.field @"creationTime"
{-# DEPRECATED skCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The key of the security key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skKey :: Lens.Lens' SecurityKey (Core.Maybe Types.PEM)
skKey = Lens.field @"key"
{-# DEPRECATED skKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Core.FromJSON SecurityKey where
  parseJSON =
    Core.withObject "SecurityKey" Core.$
      \x ->
        SecurityKey'
          Core.<$> (x Core..:? "AssociationId")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Key")
