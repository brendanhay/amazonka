{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Cipher
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Cipher
  ( Cipher (..),

    -- * Smart constructor
    mkCipher,

    -- * Lenses
    cName,
    cPriority,
  )
where

import qualified Network.AWS.ELBv2.Types.CipherName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a cipher used in a policy.
--
-- /See:/ 'mkCipher' smart constructor.
data Cipher = Cipher'
  { -- | The name of the cipher.
    name :: Core.Maybe Types.CipherName,
    -- | The priority of the cipher.
    priority :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Cipher' value with any optional fields omitted.
mkCipher ::
  Cipher
mkCipher = Cipher' {name = Core.Nothing, priority = Core.Nothing}

-- | The name of the cipher.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Cipher (Core.Maybe Types.CipherName)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The priority of the cipher.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPriority :: Lens.Lens' Cipher (Core.Maybe Core.Int)
cPriority = Lens.field @"priority"
{-# DEPRECATED cPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

instance Core.FromXML Cipher where
  parseXML x =
    Cipher'
      Core.<$> (x Core..@? "Name") Core.<*> (x Core..@? "Priority")
