{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListAssociation
  ( PrefixListAssociation (..),

    -- * Smart constructor
    mkPrefixListAssociation,

    -- * Lenses
    plaResourceId,
    plaResourceOwner,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the resource with which a prefix list is associated.
--
-- /See:/ 'mkPrefixListAssociation' smart constructor.
data PrefixListAssociation = PrefixListAssociation'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Types.String,
    -- | The owner of the resource.
    resourceOwner :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrefixListAssociation' value with any optional fields omitted.
mkPrefixListAssociation ::
  PrefixListAssociation
mkPrefixListAssociation =
  PrefixListAssociation'
    { resourceId = Core.Nothing,
      resourceOwner = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaResourceId :: Lens.Lens' PrefixListAssociation (Core.Maybe Types.String)
plaResourceId = Lens.field @"resourceId"
{-# DEPRECATED plaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The owner of the resource.
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaResourceOwner :: Lens.Lens' PrefixListAssociation (Core.Maybe Types.String)
plaResourceOwner = Lens.field @"resourceOwner"
{-# DEPRECATED plaResourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead." #-}

instance Core.FromXML PrefixListAssociation where
  parseXML x =
    PrefixListAssociation'
      Core.<$> (x Core..@? "resourceId") Core.<*> (x Core..@? "resourceOwner")
