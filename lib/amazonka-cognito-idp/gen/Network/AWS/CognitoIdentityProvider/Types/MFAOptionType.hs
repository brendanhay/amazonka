{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
  ( MFAOptionType (..),

    -- * Smart constructor
    mkMFAOptionType,

    -- * Lenses
    motDeliveryMedium,
    motAttributeName,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /This data type is no longer supported./ You can use it only for SMS MFA configurations. You can't use it for TOTP software token MFA configurations.
--
-- /See:/ 'mkMFAOptionType' smart constructor.
data MFAOptionType = MFAOptionType'
  { deliveryMedium ::
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

-- | Creates a value of 'MFAOptionType' with the minimum fields required to make a request.
--
-- * 'attributeName' - The attribute name of the MFA option type. The only valid value is @phone_number@ .
-- * 'deliveryMedium' - The delivery medium to send the MFA code. You can use this parameter to set only the @SMS@ delivery medium value.
mkMFAOptionType ::
  MFAOptionType
mkMFAOptionType =
  MFAOptionType'
    { deliveryMedium = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The delivery medium to send the MFA code. You can use this parameter to set only the @SMS@ delivery medium value.
--
-- /Note:/ Consider using 'deliveryMedium' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
motDeliveryMedium :: Lens.Lens' MFAOptionType (Lude.Maybe DeliveryMediumType)
motDeliveryMedium = Lens.lens (deliveryMedium :: MFAOptionType -> Lude.Maybe DeliveryMediumType) (\s a -> s {deliveryMedium = a} :: MFAOptionType)
{-# DEPRECATED motDeliveryMedium "Use generic-lens or generic-optics with 'deliveryMedium' instead." #-}

-- | The attribute name of the MFA option type. The only valid value is @phone_number@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
motAttributeName :: Lens.Lens' MFAOptionType (Lude.Maybe Lude.Text)
motAttributeName = Lens.lens (attributeName :: MFAOptionType -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: MFAOptionType)
{-# DEPRECATED motAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromJSON MFAOptionType where
  parseJSON =
    Lude.withObject
      "MFAOptionType"
      ( \x ->
          MFAOptionType'
            Lude.<$> (x Lude..:? "DeliveryMedium")
            Lude.<*> (x Lude..:? "AttributeName")
      )

instance Lude.ToJSON MFAOptionType where
  toJSON MFAOptionType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeliveryMedium" Lude..=) Lude.<$> deliveryMedium,
            ("AttributeName" Lude..=) Lude.<$> attributeName
          ]
      )
