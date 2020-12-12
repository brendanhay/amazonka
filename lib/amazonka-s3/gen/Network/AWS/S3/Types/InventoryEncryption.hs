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
    ieSSES3,
    ieSSEKMS,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.SSEKMS
import Network.AWS.S3.Types.SSES3

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
-- /See:/ 'mkInventoryEncryption' smart constructor.
data InventoryEncryption = InventoryEncryption'
  { sSES3 ::
      Lude.Maybe SSES3,
    sSEKMS :: Lude.Maybe SSEKMS
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryEncryption' with the minimum fields required to make a request.
--
-- * 'sSEKMS' - Specifies the use of SSE-KMS to encrypt delivered inventory reports.
-- * 'sSES3' - Specifies the use of SSE-S3 to encrypt delivered inventory reports.
mkInventoryEncryption ::
  InventoryEncryption
mkInventoryEncryption =
  InventoryEncryption' {sSES3 = Lude.Nothing, sSEKMS = Lude.Nothing}

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
-- /Note:/ Consider using 'sSES3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSSES3 :: Lens.Lens' InventoryEncryption (Lude.Maybe SSES3)
ieSSES3 = Lens.lens (sSES3 :: InventoryEncryption -> Lude.Maybe SSES3) (\s a -> s {sSES3 = a} :: InventoryEncryption)
{-# DEPRECATED ieSSES3 "Use generic-lens or generic-optics with 'sSES3' instead." #-}

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- /Note:/ Consider using 'sSEKMS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSSEKMS :: Lens.Lens' InventoryEncryption (Lude.Maybe SSEKMS)
ieSSEKMS = Lens.lens (sSEKMS :: InventoryEncryption -> Lude.Maybe SSEKMS) (\s a -> s {sSEKMS = a} :: InventoryEncryption)
{-# DEPRECATED ieSSEKMS "Use generic-lens or generic-optics with 'sSEKMS' instead." #-}

instance Lude.FromXML InventoryEncryption where
  parseXML x =
    InventoryEncryption'
      Lude.<$> (x Lude..@? "SSE-S3") Lude.<*> (x Lude..@? "SSE-KMS")

instance Lude.ToXML InventoryEncryption where
  toXML InventoryEncryption' {..} =
    Lude.mconcat ["SSE-S3" Lude.@= sSES3, "SSE-KMS" Lude.@= sSEKMS]
