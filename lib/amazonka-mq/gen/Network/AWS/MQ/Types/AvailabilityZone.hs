{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Name of the availability zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { -- | Id for the availability zone.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- * 'name' - Id for the availability zone.
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone = AvailabilityZone' {name = Lude.Nothing}

-- | Id for the availability zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azName = Lens.lens (name :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AvailabilityZone)
{-# DEPRECATED azName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AvailabilityZone where
  parseJSON =
    Lude.withObject
      "AvailabilityZone"
      (\x -> AvailabilityZone' Lude.<$> (x Lude..:? "name"))
