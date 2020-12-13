{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesId
  ( ReservedInstancesId (..),

    -- * Smart constructor
    mkReservedInstancesId,

    -- * Lenses
    riiReservedInstancesId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the ID of a Reserved Instance.
--
-- /See:/ 'mkReservedInstancesId' smart constructor.
newtype ReservedInstancesId = ReservedInstancesId'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstancesId' with the minimum fields required to make a request.
--
-- * 'reservedInstancesId' - The ID of the Reserved Instance.
mkReservedInstancesId ::
  ReservedInstancesId
mkReservedInstancesId =
  ReservedInstancesId' {reservedInstancesId = Lude.Nothing}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riiReservedInstancesId :: Lens.Lens' ReservedInstancesId (Lude.Maybe Lude.Text)
riiReservedInstancesId = Lens.lens (reservedInstancesId :: ReservedInstancesId -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: ReservedInstancesId)
{-# DEPRECATED riiReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

instance Lude.FromXML ReservedInstancesId where
  parseXML x =
    ReservedInstancesId' Lude.<$> (x Lude..@? "reservedInstancesId")
