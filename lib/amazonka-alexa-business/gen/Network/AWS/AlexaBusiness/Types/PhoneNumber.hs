{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PhoneNumber
  ( PhoneNumber (..),

    -- * Smart constructor
    mkPhoneNumber,

    -- * Lenses
    pnType,
    pnNumber,
  )
where

import Network.AWS.AlexaBusiness.Types.PhoneNumberType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The phone number for the contact containing the raw number and phone number type.
--
-- /See:/ 'mkPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { -- | The type of the phone number.
    type' :: PhoneNumberType,
    -- | The raw value of the phone number.
    number :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PhoneNumber' with the minimum fields required to make a request.
--
-- * 'type'' - The type of the phone number.
-- * 'number' - The raw value of the phone number.
mkPhoneNumber ::
  -- | 'type''
  PhoneNumberType ->
  -- | 'number'
  Lude.Sensitive Lude.Text ->
  PhoneNumber
mkPhoneNumber pType_ pNumber_ =
  PhoneNumber' {type' = pType_, number = pNumber_}

-- | The type of the phone number.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnType :: Lens.Lens' PhoneNumber PhoneNumberType
pnType = Lens.lens (type' :: PhoneNumber -> PhoneNumberType) (\s a -> s {type' = a} :: PhoneNumber)
{-# DEPRECATED pnType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The raw value of the phone number.
--
-- /Note:/ Consider using 'number' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnNumber :: Lens.Lens' PhoneNumber (Lude.Sensitive Lude.Text)
pnNumber = Lens.lens (number :: PhoneNumber -> Lude.Sensitive Lude.Text) (\s a -> s {number = a} :: PhoneNumber)
{-# DEPRECATED pnNumber "Use generic-lens or generic-optics with 'number' instead." #-}

instance Lude.FromJSON PhoneNumber where
  parseJSON =
    Lude.withObject
      "PhoneNumber"
      ( \x ->
          PhoneNumber'
            Lude.<$> (x Lude..: "Type") Lude.<*> (x Lude..: "Number")
      )

instance Lude.ToJSON PhoneNumber where
  toJSON PhoneNumber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Number" Lude..= number)
          ]
      )
