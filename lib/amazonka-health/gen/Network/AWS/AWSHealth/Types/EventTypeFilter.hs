-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventTypeFilter
  ( EventTypeFilter (..),

    -- * Smart constructor
    mkEventTypeFilter,

    -- * Lenses
    etfEventTypeCategories,
    etfEventTypeCodes,
    etfServices,
  )
where

import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventTypes.html DescribeEventTypes> operation.
--
-- /See:/ 'mkEventTypeFilter' smart constructor.
data EventTypeFilter = EventTypeFilter'
  { eventTypeCategories ::
      Lude.Maybe (Lude.NonEmpty EventTypeCategory),
    eventTypeCodes :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    services :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventTypeFilter' with the minimum fields required to make a request.
--
-- * 'eventTypeCategories' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
-- * 'eventTypeCodes' - A list of event type codes.
-- * 'services' - The AWS services associated with the event. For example, @EC2@ , @RDS@ .
mkEventTypeFilter ::
  EventTypeFilter
mkEventTypeFilter =
  EventTypeFilter'
    { eventTypeCategories = Lude.Nothing,
      eventTypeCodes = Lude.Nothing,
      services = Lude.Nothing
    }

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- /Note:/ Consider using 'eventTypeCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfEventTypeCategories :: Lens.Lens' EventTypeFilter (Lude.Maybe (Lude.NonEmpty EventTypeCategory))
etfEventTypeCategories = Lens.lens (eventTypeCategories :: EventTypeFilter -> Lude.Maybe (Lude.NonEmpty EventTypeCategory)) (\s a -> s {eventTypeCategories = a} :: EventTypeFilter)
{-# DEPRECATED etfEventTypeCategories "Use generic-lens or generic-optics with 'eventTypeCategories' instead." #-}

-- | A list of event type codes.
--
-- /Note:/ Consider using 'eventTypeCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfEventTypeCodes :: Lens.Lens' EventTypeFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
etfEventTypeCodes = Lens.lens (eventTypeCodes :: EventTypeFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {eventTypeCodes = a} :: EventTypeFilter)
{-# DEPRECATED etfEventTypeCodes "Use generic-lens or generic-optics with 'eventTypeCodes' instead." #-}

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etfServices :: Lens.Lens' EventTypeFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
etfServices = Lens.lens (services :: EventTypeFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {services = a} :: EventTypeFilter)
{-# DEPRECATED etfServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Lude.ToJSON EventTypeFilter where
  toJSON EventTypeFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("eventTypeCategories" Lude..=) Lude.<$> eventTypeCategories,
            ("eventTypeCodes" Lude..=) Lude.<$> eventTypeCodes,
            ("services" Lude..=) Lude.<$> services
          ]
      )
