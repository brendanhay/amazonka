{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryEncryption
  ( InventoryEncryption (..),

    -- * Smart constructor
    mkInventoryEncryption,

    -- * Lenses
    ieSSEKMS,
    ieSSES3,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.SSEKMS as Types
import qualified Network.AWS.S3.Types.SSES3 as Types

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
-- /See:/ 'mkInventoryEncryption' smart constructor.
data InventoryEncryption = InventoryEncryption'
  { -- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
    ssekms :: Core.Maybe Types.SSEKMS,
    -- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
    sSES3 :: Core.Maybe Types.SSES3
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryEncryption' value with any optional fields omitted.
mkInventoryEncryption ::
  InventoryEncryption
mkInventoryEncryption =
  InventoryEncryption' {ssekms = Core.Nothing, sSES3 = Core.Nothing}

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- /Note:/ Consider using 'ssekms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSSEKMS :: Lens.Lens' InventoryEncryption (Core.Maybe Types.SSEKMS)
ieSSEKMS = Lens.field @"ssekms"
{-# DEPRECATED ieSSEKMS "Use generic-lens or generic-optics with 'ssekms' instead." #-}

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
-- /Note:/ Consider using 'sSES3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSSES3 :: Lens.Lens' InventoryEncryption (Core.Maybe Types.SSES3)
ieSSES3 = Lens.field @"sSES3"
{-# DEPRECATED ieSSES3 "Use generic-lens or generic-optics with 'sSES3' instead." #-}

instance Core.ToXML InventoryEncryption where
  toXML InventoryEncryption {..} =
    Core.toXMLNode "SSE-KMS" Core.<$> ssekms
      Core.<> Core.toXMLNode "SSE-S3" Core.<$> sSES3

instance Core.FromXML InventoryEncryption where
  parseXML x =
    InventoryEncryption'
      Core.<$> (x Core..@? "SSE-KMS") Core.<*> (x Core..@? "SSE-S3")
