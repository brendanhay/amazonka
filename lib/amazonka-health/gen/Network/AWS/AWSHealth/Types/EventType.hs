-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventType
  ( EventType (..),

    -- * Smart constructor
    mkEventType,

    -- * Lenses
    etService,
    etCategory,
    etCode,
  )
where

import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, @issue@ ), the service (for example, @EC2@ ), and the event type code (for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
--
-- /See:/ 'mkEventType' smart constructor.
data EventType = EventType'
  { service :: Lude.Maybe Lude.Text,
    category :: Lude.Maybe EventTypeCategory,
    code :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventType' with the minimum fields required to make a request.
--
-- * 'category' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
-- * 'code' - The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
-- * 'service' - The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
mkEventType ::
  EventType
mkEventType =
  EventType'
    { service = Lude.Nothing,
      category = Lude.Nothing,
      code = Lude.Nothing
    }

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etService :: Lens.Lens' EventType (Lude.Maybe Lude.Text)
etService = Lens.lens (service :: EventType -> Lude.Maybe Lude.Text) (\s a -> s {service = a} :: EventType)
{-# DEPRECATED etService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCategory :: Lens.Lens' EventType (Lude.Maybe EventTypeCategory)
etCategory = Lens.lens (category :: EventType -> Lude.Maybe EventTypeCategory) (\s a -> s {category = a} :: EventType)
{-# DEPRECATED etCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etCode :: Lens.Lens' EventType (Lude.Maybe Lude.Text)
etCode = Lens.lens (code :: EventType -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: EventType)
{-# DEPRECATED etCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON EventType where
  parseJSON =
    Lude.withObject
      "EventType"
      ( \x ->
          EventType'
            Lude.<$> (x Lude..:? "service")
            Lude.<*> (x Lude..:? "category")
            Lude.<*> (x Lude..:? "code")
      )
