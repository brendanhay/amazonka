{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Country
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Country
  ( Country (..),

    -- * Smart constructor
    mkCountry,

    -- * Lenses
    cCountryName,
    cCountryCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the country where the remote IP address is located.
--
-- /See:/ 'mkCountry' smart constructor.
data Country = Country'
  { -- | The country name of the remote IP address.
    countryName :: Lude.Maybe Lude.Text,
    -- | The country code of the remote IP address.
    countryCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Country' with the minimum fields required to make a request.
--
-- * 'countryName' - The country name of the remote IP address.
-- * 'countryCode' - The country code of the remote IP address.
mkCountry ::
  Country
mkCountry =
  Country' {countryName = Lude.Nothing, countryCode = Lude.Nothing}

-- | The country name of the remote IP address.
--
-- /Note:/ Consider using 'countryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCountryName :: Lens.Lens' Country (Lude.Maybe Lude.Text)
cCountryName = Lens.lens (countryName :: Country -> Lude.Maybe Lude.Text) (\s a -> s {countryName = a} :: Country)
{-# DEPRECATED cCountryName "Use generic-lens or generic-optics with 'countryName' instead." #-}

-- | The country code of the remote IP address.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCountryCode :: Lens.Lens' Country (Lude.Maybe Lude.Text)
cCountryCode = Lens.lens (countryCode :: Country -> Lude.Maybe Lude.Text) (\s a -> s {countryCode = a} :: Country)
{-# DEPRECATED cCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

instance Lude.FromJSON Country where
  parseJSON =
    Lude.withObject
      "Country"
      ( \x ->
          Country'
            Lude.<$> (x Lude..:? "countryName") Lude.<*> (x Lude..:? "countryCode")
      )
