-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDescription
  ( EventDescription (..),

    -- * Smart constructor
    mkEventDescription,

    -- * Lenses
    edLatestDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detailed description of the event. Included in the information returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation.
--
-- /See:/ 'mkEventDescription' smart constructor.
newtype EventDescription = EventDescription'
  { latestDescription ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- * 'latestDescription' - The most recent description of the event.
mkEventDescription ::
  EventDescription
mkEventDescription =
  EventDescription' {latestDescription = Lude.Nothing}

-- | The most recent description of the event.
--
-- /Note:/ Consider using 'latestDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edLatestDescription :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edLatestDescription = Lens.lens (latestDescription :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {latestDescription = a} :: EventDescription)
{-# DEPRECATED edLatestDescription "Use generic-lens or generic-optics with 'latestDescription' instead." #-}

instance Lude.FromJSON EventDescription where
  parseJSON =
    Lude.withObject
      "EventDescription"
      ( \x ->
          EventDescription' Lude.<$> (x Lude..:? "latestDescription")
      )
