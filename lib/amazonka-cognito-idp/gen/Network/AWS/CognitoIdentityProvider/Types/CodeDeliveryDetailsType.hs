-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
  ( CodeDeliveryDetailsType (..),

    -- * Smart constructor
    mkCodeDeliveryDetailsType,

    -- * Lenses
    cddtDestination,
    cddtDeliveryMedium,
    cddtAttributeName,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The code delivery details being returned from the server.
--
-- /See:/ 'mkCodeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
  { destination ::
      Lude.Maybe Lude.Text,
    deliveryMedium ::
      Lude.Maybe DeliveryMediumType,
    attributeName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeDeliveryDetailsType' with the minimum fields required to make a request.
--
-- * 'attributeName' - The attribute name.
-- * 'deliveryMedium' - The delivery medium (email message or phone number).
-- * 'destination' - The destination for the code delivery details.
mkCodeDeliveryDetailsType ::
  CodeDeliveryDetailsType
mkCodeDeliveryDetailsType =
  CodeDeliveryDetailsType'
    { destination = Lude.Nothing,
      deliveryMedium = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The destination for the code delivery details.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddtDestination :: Lens.Lens' CodeDeliveryDetailsType (Lude.Maybe Lude.Text)
cddtDestination = Lens.lens (destination :: CodeDeliveryDetailsType -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: CodeDeliveryDetailsType)
{-# DEPRECATED cddtDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The delivery medium (email message or phone number).
--
-- /Note:/ Consider using 'deliveryMedium' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddtDeliveryMedium :: Lens.Lens' CodeDeliveryDetailsType (Lude.Maybe DeliveryMediumType)
cddtDeliveryMedium = Lens.lens (deliveryMedium :: CodeDeliveryDetailsType -> Lude.Maybe DeliveryMediumType) (\s a -> s {deliveryMedium = a} :: CodeDeliveryDetailsType)
{-# DEPRECATED cddtDeliveryMedium "Use generic-lens or generic-optics with 'deliveryMedium' instead." #-}

-- | The attribute name.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddtAttributeName :: Lens.Lens' CodeDeliveryDetailsType (Lude.Maybe Lude.Text)
cddtAttributeName = Lens.lens (attributeName :: CodeDeliveryDetailsType -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: CodeDeliveryDetailsType)
{-# DEPRECATED cddtAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromJSON CodeDeliveryDetailsType where
  parseJSON =
    Lude.withObject
      "CodeDeliveryDetailsType"
      ( \x ->
          CodeDeliveryDetailsType'
            Lude.<$> (x Lude..:? "Destination")
            Lude.<*> (x Lude..:? "DeliveryMedium")
            Lude.<*> (x Lude..:? "AttributeName")
      )
