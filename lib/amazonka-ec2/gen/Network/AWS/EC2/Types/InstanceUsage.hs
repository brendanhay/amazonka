{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceUsage
  ( InstanceUsage (..),

    -- * Smart constructor
    mkInstanceUsage,

    -- * Lenses
    iuAccountId,
    iuUsedInstanceCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the Capacity Reservation usage.
--
-- /See:/ 'mkInstanceUsage' smart constructor.
data InstanceUsage = InstanceUsage'
  { accountId ::
      Lude.Maybe Lude.Text,
    usedInstanceCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceUsage' with the minimum fields required to make a request.
--
-- * 'accountId' - The ID of the AWS account that is making use of the Capacity Reservation.
-- * 'usedInstanceCount' - The number of instances the AWS account currently has in the Capacity Reservation.
mkInstanceUsage ::
  InstanceUsage
mkInstanceUsage =
  InstanceUsage'
    { accountId = Lude.Nothing,
      usedInstanceCount = Lude.Nothing
    }

-- | The ID of the AWS account that is making use of the Capacity Reservation.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuAccountId :: Lens.Lens' InstanceUsage (Lude.Maybe Lude.Text)
iuAccountId = Lens.lens (accountId :: InstanceUsage -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: InstanceUsage)
{-# DEPRECATED iuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The number of instances the AWS account currently has in the Capacity Reservation.
--
-- /Note:/ Consider using 'usedInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuUsedInstanceCount :: Lens.Lens' InstanceUsage (Lude.Maybe Lude.Int)
iuUsedInstanceCount = Lens.lens (usedInstanceCount :: InstanceUsage -> Lude.Maybe Lude.Int) (\s a -> s {usedInstanceCount = a} :: InstanceUsage)
{-# DEPRECATED iuUsedInstanceCount "Use generic-lens or generic-optics with 'usedInstanceCount' instead." #-}

instance Lude.FromXML InstanceUsage where
  parseXML x =
    InstanceUsage'
      Lude.<$> (x Lude..@? "accountId") Lude.<*> (x Lude..@? "usedInstanceCount")
