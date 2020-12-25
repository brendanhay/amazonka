{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KGKeyPairIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KGKeyPairIds
  ( KGKeyPairIds (..),

    -- * Smart constructor
    mkKGKeyPairIds,

    -- * Lenses
    kgkpiKeyGroupId,
    kgkpiKeyPairIds,
  )
where

import qualified Network.AWS.CloudFront.Types.KeyPairIds as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of identifiers for the public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkKGKeyPairIds' smart constructor.
data KGKeyPairIds = KGKeyPairIds'
  { -- | The identifier of the key group that contains the public keys.
    keyGroupId :: Core.Maybe Types.String,
    keyPairIds :: Core.Maybe Types.KeyPairIds
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KGKeyPairIds' value with any optional fields omitted.
mkKGKeyPairIds ::
  KGKeyPairIds
mkKGKeyPairIds =
  KGKeyPairIds'
    { keyGroupId = Core.Nothing,
      keyPairIds = Core.Nothing
    }

-- | The identifier of the key group that contains the public keys.
--
-- /Note:/ Consider using 'keyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgkpiKeyGroupId :: Lens.Lens' KGKeyPairIds (Core.Maybe Types.String)
kgkpiKeyGroupId = Lens.field @"keyGroupId"
{-# DEPRECATED kgkpiKeyGroupId "Use generic-lens or generic-optics with 'keyGroupId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'keyPairIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgkpiKeyPairIds :: Lens.Lens' KGKeyPairIds (Core.Maybe Types.KeyPairIds)
kgkpiKeyPairIds = Lens.field @"keyPairIds"
{-# DEPRECATED kgkpiKeyPairIds "Use generic-lens or generic-optics with 'keyPairIds' instead." #-}

instance Core.FromXML KGKeyPairIds where
  parseXML x =
    KGKeyPairIds'
      Core.<$> (x Core..@? "KeyGroupId") Core.<*> (x Core..@? "KeyPairIds")
