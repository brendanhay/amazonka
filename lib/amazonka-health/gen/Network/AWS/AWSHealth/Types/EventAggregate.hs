{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAggregate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventAggregate
  ( EventAggregate (..),

    -- * Smart constructor
    mkEventAggregate,

    -- * Lenses
    eaCount,
    eaAggregateValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of events of each issue type. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates> operation.
--
-- /See:/ 'mkEventAggregate' smart constructor.
data EventAggregate = EventAggregate'
  { -- | The number of events of the associated issue type.
    count :: Lude.Maybe Lude.Int,
    -- | The issue type for the associated count.
    aggregateValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventAggregate' with the minimum fields required to make a request.
--
-- * 'count' - The number of events of the associated issue type.
-- * 'aggregateValue' - The issue type for the associated count.
mkEventAggregate ::
  EventAggregate
mkEventAggregate =
  EventAggregate'
    { count = Lude.Nothing,
      aggregateValue = Lude.Nothing
    }

-- | The number of events of the associated issue type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaCount :: Lens.Lens' EventAggregate (Lude.Maybe Lude.Int)
eaCount = Lens.lens (count :: EventAggregate -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: EventAggregate)
{-# DEPRECATED eaCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The issue type for the associated count.
--
-- /Note:/ Consider using 'aggregateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaAggregateValue :: Lens.Lens' EventAggregate (Lude.Maybe Lude.Text)
eaAggregateValue = Lens.lens (aggregateValue :: EventAggregate -> Lude.Maybe Lude.Text) (\s a -> s {aggregateValue = a} :: EventAggregate)
{-# DEPRECATED eaAggregateValue "Use generic-lens or generic-optics with 'aggregateValue' instead." #-}

instance Lude.FromJSON EventAggregate where
  parseJSON =
    Lude.withObject
      "EventAggregate"
      ( \x ->
          EventAggregate'
            Lude.<$> (x Lude..:? "count") Lude.<*> (x Lude..:? "aggregateValue")
      )
