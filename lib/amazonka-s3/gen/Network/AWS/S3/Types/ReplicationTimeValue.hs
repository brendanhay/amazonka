-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTimeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationTimeValue
  ( ReplicationTimeValue (..),

    -- * Smart constructor
    mkReplicationTimeValue,

    -- * Lenses
    rtvMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | A container specifying the time value for S3 Replication Time Control (S3 RTC) and replication metrics @EventThreshold@ .
--
-- /See:/ 'mkReplicationTimeValue' smart constructor.
newtype ReplicationTimeValue = ReplicationTimeValue'
  { minutes ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTimeValue' with the minimum fields required to make a request.
--
-- * 'minutes' - Contains an integer specifying time in minutes.
--
-- Valid values: 15 minutes.
mkReplicationTimeValue ::
  ReplicationTimeValue
mkReplicationTimeValue =
  ReplicationTimeValue' {minutes = Lude.Nothing}

-- | Contains an integer specifying time in minutes.
--
-- Valid values: 15 minutes.
--
-- /Note:/ Consider using 'minutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtvMinutes :: Lens.Lens' ReplicationTimeValue (Lude.Maybe Lude.Int)
rtvMinutes = Lens.lens (minutes :: ReplicationTimeValue -> Lude.Maybe Lude.Int) (\s a -> s {minutes = a} :: ReplicationTimeValue)
{-# DEPRECATED rtvMinutes "Use generic-lens or generic-optics with 'minutes' instead." #-}

instance Lude.FromXML ReplicationTimeValue where
  parseXML x = ReplicationTimeValue' Lude.<$> (x Lude..@? "Minutes")

instance Lude.ToXML ReplicationTimeValue where
  toXML ReplicationTimeValue' {..} =
    Lude.mconcat ["Minutes" Lude.@= minutes]
