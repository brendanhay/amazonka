{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleExpiration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleExpiration
  ( LifecycleExpiration (..),

    -- * Smart constructor
    mkLifecycleExpiration,

    -- * Lenses
    leDays,
    leDate,
    leExpiredObjectDeleteMarker,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for the expiration for the lifecycle of the object.
--
-- /See:/ 'mkLifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { -- | Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
    days :: Lude.Maybe Lude.Int,
    -- | Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
    date :: Lude.Maybe Lude.DateTime,
    -- | Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
    expiredObjectDeleteMarker :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleExpiration' with the minimum fields required to make a request.
--
-- * 'days' - Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
-- * 'date' - Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
-- * 'expiredObjectDeleteMarker' - Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
mkLifecycleExpiration ::
  LifecycleExpiration
mkLifecycleExpiration =
  LifecycleExpiration'
    { days = Lude.Nothing,
      date = Lude.Nothing,
      expiredObjectDeleteMarker = Lude.Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leDays :: Lens.Lens' LifecycleExpiration (Lude.Maybe Lude.Int)
leDays = Lens.lens (days :: LifecycleExpiration -> Lude.Maybe Lude.Int) (\s a -> s {days = a} :: LifecycleExpiration)
{-# DEPRECATED leDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leDate :: Lens.Lens' LifecycleExpiration (Lude.Maybe Lude.DateTime)
leDate = Lens.lens (date :: LifecycleExpiration -> Lude.Maybe Lude.DateTime) (\s a -> s {date = a} :: LifecycleExpiration)
{-# DEPRECATED leDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
--
-- /Note:/ Consider using 'expiredObjectDeleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leExpiredObjectDeleteMarker :: Lens.Lens' LifecycleExpiration (Lude.Maybe Lude.Bool)
leExpiredObjectDeleteMarker = Lens.lens (expiredObjectDeleteMarker :: LifecycleExpiration -> Lude.Maybe Lude.Bool) (\s a -> s {expiredObjectDeleteMarker = a} :: LifecycleExpiration)
{-# DEPRECATED leExpiredObjectDeleteMarker "Use generic-lens or generic-optics with 'expiredObjectDeleteMarker' instead." #-}

instance Lude.FromXML LifecycleExpiration where
  parseXML x =
    LifecycleExpiration'
      Lude.<$> (x Lude..@? "Days")
      Lude.<*> (x Lude..@? "Date")
      Lude.<*> (x Lude..@? "ExpiredObjectDeleteMarker")

instance Lude.ToXML LifecycleExpiration where
  toXML LifecycleExpiration' {..} =
    Lude.mconcat
      [ "Days" Lude.@= days,
        "Date" Lude.@= date,
        "ExpiredObjectDeleteMarker" Lude.@= expiredObjectDeleteMarker
      ]
