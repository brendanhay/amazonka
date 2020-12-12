{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EncryptionEntities
  ( EncryptionEntities (..),

    -- * Smart constructor
    mkEncryptionEntities,

    -- * Lenses
    eeItems,
    eeQuantity,
  )
where

import Network.AWS.CloudFront.Types.EncryptionEntity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Complex data type for field-level encryption profiles that includes all of the encryption entities.
--
-- /See:/ 'mkEncryptionEntities' smart constructor.
data EncryptionEntities = EncryptionEntities'
  { items ::
      Lude.Maybe [EncryptionEntity],
    quantity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionEntities' with the minimum fields required to make a request.
--
-- * 'items' - An array of field patterns in a field-level encryption content type-profile mapping.
-- * 'quantity' - Number of field pattern items in a field-level encryption content type-profile mapping.
mkEncryptionEntities ::
  -- | 'quantity'
  Lude.Int ->
  EncryptionEntities
mkEncryptionEntities pQuantity_ =
  EncryptionEntities' {items = Lude.Nothing, quantity = pQuantity_}

-- | An array of field patterns in a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeItems :: Lens.Lens' EncryptionEntities (Lude.Maybe [EncryptionEntity])
eeItems = Lens.lens (items :: EncryptionEntities -> Lude.Maybe [EncryptionEntity]) (\s a -> s {items = a} :: EncryptionEntities)
{-# DEPRECATED eeItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Number of field pattern items in a field-level encryption content type-profile mapping.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeQuantity :: Lens.Lens' EncryptionEntities Lude.Int
eeQuantity = Lens.lens (quantity :: EncryptionEntities -> Lude.Int) (\s a -> s {quantity = a} :: EncryptionEntities)
{-# DEPRECATED eeQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML EncryptionEntities where
  parseXML x =
    EncryptionEntities'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EncryptionEntity")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML EncryptionEntities where
  toXML EncryptionEntities' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "EncryptionEntity" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
