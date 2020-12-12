{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityAggregate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityAggregate
  ( EntityAggregate (..),

    -- * Smart constructor
    mkEntityAggregate,

    -- * Lenses
    eCount,
    eEventARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of entities that are affected by one or more events. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEntityAggregates.html DescribeEntityAggregates> operation.
--
-- /See:/ 'mkEntityAggregate' smart constructor.
data EntityAggregate = EntityAggregate'
  { count ::
      Lude.Maybe Lude.Int,
    eventARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityAggregate' with the minimum fields required to make a request.
--
-- * 'count' - The number of entities that match the criteria for the specified events.
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
mkEntityAggregate ::
  EntityAggregate
mkEntityAggregate =
  EntityAggregate' {count = Lude.Nothing, eventARN = Lude.Nothing}

-- | The number of entities that match the criteria for the specified events.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCount :: Lens.Lens' EntityAggregate (Lude.Maybe Lude.Int)
eCount = Lens.lens (count :: EntityAggregate -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: EntityAggregate)
{-# DEPRECATED eCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventARN :: Lens.Lens' EntityAggregate (Lude.Maybe Lude.Text)
eEventARN = Lens.lens (eventARN :: EntityAggregate -> Lude.Maybe Lude.Text) (\s a -> s {eventARN = a} :: EntityAggregate)
{-# DEPRECATED eEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

instance Lude.FromJSON EntityAggregate where
  parseJSON =
    Lude.withObject
      "EntityAggregate"
      ( \x ->
          EntityAggregate'
            Lude.<$> (x Lude..:? "count") Lude.<*> (x Lude..:? "eventArn")
      )
