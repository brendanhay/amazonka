{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Availability Zone in which the cluster is launched.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { -- | The name of the Availability Zone.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- * 'name' - The name of the Availability Zone.
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone = AvailabilityZone' {name = Lude.Nothing}

-- | The name of the Availability Zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azName = Lens.lens (name :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AvailabilityZone)
{-# DEPRECATED azName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML AvailabilityZone where
  parseXML x = AvailabilityZone' Lude.<$> (x Lude..@? "Name")
