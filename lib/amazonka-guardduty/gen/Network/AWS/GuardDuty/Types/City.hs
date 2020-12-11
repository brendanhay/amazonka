-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.City
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.City
  ( City (..),

    -- * Smart constructor
    mkCity,

    -- * Lenses
    cCityName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the city associated with the IP address.
--
-- /See:/ 'mkCity' smart constructor.
newtype City = City' {cityName :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'City' with the minimum fields required to make a request.
--
-- * 'cityName' - The city name of the remote IP address.
mkCity ::
  City
mkCity = City' {cityName = Lude.Nothing}

-- | The city name of the remote IP address.
--
-- /Note:/ Consider using 'cityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCityName :: Lens.Lens' City (Lude.Maybe Lude.Text)
cCityName = Lens.lens (cityName :: City -> Lude.Maybe Lude.Text) (\s a -> s {cityName = a} :: City)
{-# DEPRECATED cCityName "Use generic-lens or generic-optics with 'cityName' instead." #-}

instance Lude.FromJSON City where
  parseJSON =
    Lude.withObject
      "City"
      (\x -> City' Lude.<$> (x Lude..:? "cityName"))
