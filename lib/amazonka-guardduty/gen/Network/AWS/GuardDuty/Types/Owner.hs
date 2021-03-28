{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Owner
  ( Owner (..)
  -- * Smart constructor
  , mkOwner
  -- * Lenses
  , oId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the owner of the bucket.
--
-- /See:/ 'mkOwner' smart constructor.
newtype Owner = Owner'
  { id :: Core.Maybe Core.Text
    -- ^ The canonical user ID of the bucket owner. For information about locating your canonical user ID see <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.> 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Owner' value with any optional fields omitted.
mkOwner
    :: Owner
mkOwner = Owner'{id = Core.Nothing}

-- | The canonical user ID of the bucket owner. For information about locating your canonical user ID see <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.> 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Owner (Core.Maybe Core.Text)
oId = Lens.field @"id"
{-# INLINEABLE oId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON Owner where
        parseJSON
          = Core.withObject "Owner" Core.$
              \ x -> Owner' Core.<$> (x Core..:? "id")
