-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveDescription
  ( TimeToLiveDescription (..),

    -- * Smart constructor
    mkTimeToLiveDescription,

    -- * Lenses
    ttldTimeToLiveStatus,
    ttldAttributeName,
  )
where

import Network.AWS.DynamoDB.Types.TimeToLiveStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of the Time to Live (TTL) status on the specified table.
--
-- /See:/ 'mkTimeToLiveDescription' smart constructor.
data TimeToLiveDescription = TimeToLiveDescription'
  { timeToLiveStatus ::
      Lude.Maybe TimeToLiveStatus,
    attributeName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeToLiveDescription' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the TTL attribute for items in the table.
-- * 'timeToLiveStatus' - The TTL status for the table.
mkTimeToLiveDescription ::
  TimeToLiveDescription
mkTimeToLiveDescription =
  TimeToLiveDescription'
    { timeToLiveStatus = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The TTL status for the table.
--
-- /Note:/ Consider using 'timeToLiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttldTimeToLiveStatus :: Lens.Lens' TimeToLiveDescription (Lude.Maybe TimeToLiveStatus)
ttldTimeToLiveStatus = Lens.lens (timeToLiveStatus :: TimeToLiveDescription -> Lude.Maybe TimeToLiveStatus) (\s a -> s {timeToLiveStatus = a} :: TimeToLiveDescription)
{-# DEPRECATED ttldTimeToLiveStatus "Use generic-lens or generic-optics with 'timeToLiveStatus' instead." #-}

-- | The name of the TTL attribute for items in the table.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttldAttributeName :: Lens.Lens' TimeToLiveDescription (Lude.Maybe Lude.Text)
ttldAttributeName = Lens.lens (attributeName :: TimeToLiveDescription -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: TimeToLiveDescription)
{-# DEPRECATED ttldAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromJSON TimeToLiveDescription where
  parseJSON =
    Lude.withObject
      "TimeToLiveDescription"
      ( \x ->
          TimeToLiveDescription'
            Lude.<$> (x Lude..:? "TimeToLiveStatus")
            Lude.<*> (x Lude..:? "AttributeName")
      )
