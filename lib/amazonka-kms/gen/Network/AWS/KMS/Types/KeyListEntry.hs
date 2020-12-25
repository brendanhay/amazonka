{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyListEntry
  ( KeyListEntry (..),

    -- * Smart constructor
    mkKeyListEntry,

    -- * Lenses
    kleKeyArn,
    kleKeyId,
  )
where

import qualified Network.AWS.KMS.Types.ArnType as Types
import qualified Network.AWS.KMS.Types.KeyIdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about each entry in the key list.
--
-- /See:/ 'mkKeyListEntry' smart constructor.
data KeyListEntry = KeyListEntry'
  { -- | ARN of the key.
    keyArn :: Core.Maybe Types.ArnType,
    -- | Unique identifier of the key.
    keyId :: Core.Maybe Types.KeyIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyListEntry' value with any optional fields omitted.
mkKeyListEntry ::
  KeyListEntry
mkKeyListEntry =
  KeyListEntry' {keyArn = Core.Nothing, keyId = Core.Nothing}

-- | ARN of the key.
--
-- /Note:/ Consider using 'keyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kleKeyArn :: Lens.Lens' KeyListEntry (Core.Maybe Types.ArnType)
kleKeyArn = Lens.field @"keyArn"
{-# DEPRECATED kleKeyArn "Use generic-lens or generic-optics with 'keyArn' instead." #-}

-- | Unique identifier of the key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kleKeyId :: Lens.Lens' KeyListEntry (Core.Maybe Types.KeyIdType)
kleKeyId = Lens.field @"keyId"
{-# DEPRECATED kleKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Core.FromJSON KeyListEntry where
  parseJSON =
    Core.withObject "KeyListEntry" Core.$
      \x ->
        KeyListEntry'
          Core.<$> (x Core..:? "KeyArn") Core.<*> (x Core..:? "KeyId")
