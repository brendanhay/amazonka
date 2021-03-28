{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
  ( ObjectIdentifierAndLinkNameTuple (..)
  -- * Smart constructor
  , mkObjectIdentifierAndLinkNameTuple
  -- * Lenses
  , oialntLinkName
  , oialntObjectIdentifier
  ) where

import qualified Network.AWS.CloudDirectory.Types.LinkName as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A pair of ObjectIdentifier and LinkName.
--
-- /See:/ 'mkObjectIdentifierAndLinkNameTuple' smart constructor.
data ObjectIdentifierAndLinkNameTuple = ObjectIdentifierAndLinkNameTuple'
  { linkName :: Core.Maybe Types.LinkName
    -- ^ The name of the link between the parent and the child object.
  , objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The ID that is associated with the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectIdentifierAndLinkNameTuple' value with any optional fields omitted.
mkObjectIdentifierAndLinkNameTuple
    :: ObjectIdentifierAndLinkNameTuple
mkObjectIdentifierAndLinkNameTuple
  = ObjectIdentifierAndLinkNameTuple'{linkName = Core.Nothing,
                                      objectIdentifier = Core.Nothing}

-- | The name of the link between the parent and the child object.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oialntLinkName :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Core.Maybe Types.LinkName)
oialntLinkName = Lens.field @"linkName"
{-# INLINEABLE oialntLinkName #-}
{-# DEPRECATED linkName "Use generic-lens or generic-optics with 'linkName' instead"  #-}

-- | The ID that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oialntObjectIdentifier :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Core.Maybe Types.ObjectIdentifier)
oialntObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE oialntObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

instance Core.FromJSON ObjectIdentifierAndLinkNameTuple where
        parseJSON
          = Core.withObject "ObjectIdentifierAndLinkNameTuple" Core.$
              \ x ->
                ObjectIdentifierAndLinkNameTuple' Core.<$>
                  (x Core..:? "LinkName") Core.<*> x Core..:? "ObjectIdentifier"
