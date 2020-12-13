{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BillingGroupProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BillingGroupProperties
  ( BillingGroupProperties (..),

    -- * Smart constructor
    mkBillingGroupProperties,

    -- * Lenses
    bgpBillingGroupDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The properties of a billing group.
--
-- /See:/ 'mkBillingGroupProperties' smart constructor.
newtype BillingGroupProperties = BillingGroupProperties'
  { -- | The description of the billing group.
    billingGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BillingGroupProperties' with the minimum fields required to make a request.
--
-- * 'billingGroupDescription' - The description of the billing group.
mkBillingGroupProperties ::
  BillingGroupProperties
mkBillingGroupProperties =
  BillingGroupProperties' {billingGroupDescription = Lude.Nothing}

-- | The description of the billing group.
--
-- /Note:/ Consider using 'billingGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpBillingGroupDescription :: Lens.Lens' BillingGroupProperties (Lude.Maybe Lude.Text)
bgpBillingGroupDescription = Lens.lens (billingGroupDescription :: BillingGroupProperties -> Lude.Maybe Lude.Text) (\s a -> s {billingGroupDescription = a} :: BillingGroupProperties)
{-# DEPRECATED bgpBillingGroupDescription "Use generic-lens or generic-optics with 'billingGroupDescription' instead." #-}

instance Lude.FromJSON BillingGroupProperties where
  parseJSON =
    Lude.withObject
      "BillingGroupProperties"
      ( \x ->
          BillingGroupProperties'
            Lude.<$> (x Lude..:? "billingGroupDescription")
      )

instance Lude.ToJSON BillingGroupProperties where
  toJSON BillingGroupProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("billingGroupDescription" Lude..=)
              Lude.<$> billingGroupDescription
          ]
      )
