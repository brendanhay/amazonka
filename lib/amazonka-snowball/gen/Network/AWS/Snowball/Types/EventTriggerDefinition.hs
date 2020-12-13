{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.EventTriggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EventTriggerDefinition
  ( EventTriggerDefinition (..),

    -- * Smart constructor
    mkEventTriggerDefinition,

    -- * Lenses
    etdEventResourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The container for the 'EventTriggerDefinition$EventResourceARN' .
--
-- /See:/ 'mkEventTriggerDefinition' smart constructor.
newtype EventTriggerDefinition = EventTriggerDefinition'
  { -- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
    eventResourceARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventTriggerDefinition' with the minimum fields required to make a request.
--
-- * 'eventResourceARN' - The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
mkEventTriggerDefinition ::
  EventTriggerDefinition
mkEventTriggerDefinition =
  EventTriggerDefinition' {eventResourceARN = Lude.Nothing}

-- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
--
-- /Note:/ Consider using 'eventResourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etdEventResourceARN :: Lens.Lens' EventTriggerDefinition (Lude.Maybe Lude.Text)
etdEventResourceARN = Lens.lens (eventResourceARN :: EventTriggerDefinition -> Lude.Maybe Lude.Text) (\s a -> s {eventResourceARN = a} :: EventTriggerDefinition)
{-# DEPRECATED etdEventResourceARN "Use generic-lens or generic-optics with 'eventResourceARN' instead." #-}

instance Lude.FromJSON EventTriggerDefinition where
  parseJSON =
    Lude.withObject
      "EventTriggerDefinition"
      ( \x ->
          EventTriggerDefinition' Lude.<$> (x Lude..:? "EventResourceARN")
      )

instance Lude.ToJSON EventTriggerDefinition where
  toJSON EventTriggerDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [("EventResourceARN" Lude..=) Lude.<$> eventResourceARN]
      )
