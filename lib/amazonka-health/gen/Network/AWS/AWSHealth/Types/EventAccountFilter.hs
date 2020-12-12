{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAccountFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventAccountFilter
  ( EventAccountFilter (..),

    -- * Smart constructor
    mkEventAccountFilter,

    -- * Lenses
    eafAwsAccountId,
    eafEventARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values used to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> and <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> operations.
--
-- /See:/ 'mkEventAccountFilter' smart constructor.
data EventAccountFilter = EventAccountFilter'
  { awsAccountId ::
      Lude.Maybe Lude.Text,
    eventARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventAccountFilter' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
mkEventAccountFilter ::
  -- | 'eventARN'
  Lude.Text ->
  EventAccountFilter
mkEventAccountFilter pEventARN_ =
  EventAccountFilter'
    { awsAccountId = Lude.Nothing,
      eventARN = pEventARN_
    }

-- | The 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafAwsAccountId :: Lens.Lens' EventAccountFilter (Lude.Maybe Lude.Text)
eafAwsAccountId = Lens.lens (awsAccountId :: EventAccountFilter -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: EventAccountFilter)
{-# DEPRECATED eafAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafEventARN :: Lens.Lens' EventAccountFilter Lude.Text
eafEventARN = Lens.lens (eventARN :: EventAccountFilter -> Lude.Text) (\s a -> s {eventARN = a} :: EventAccountFilter)
{-# DEPRECATED eafEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

instance Lude.ToJSON EventAccountFilter where
  toJSON EventAccountFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("awsAccountId" Lude..=) Lude.<$> awsAccountId,
            Lude.Just ("eventArn" Lude..= eventARN)
          ]
      )
