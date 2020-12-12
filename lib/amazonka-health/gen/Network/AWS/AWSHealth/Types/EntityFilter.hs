{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityFilter
  ( EntityFilter (..),

    -- * Smart constructor
    mkEntityFilter,

    -- * Lenses
    eStatusCodes,
    eEntityARNs,
    eEntityValues,
    eTags,
    eLastUpdatedTimes,
    eEventARNs,
  )
where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EntityStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter> operation.
--
-- /See:/ 'mkEntityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { statusCodes ::
      Lude.Maybe (Lude.NonEmpty EntityStatusCode),
    entityARNs :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    entityValues :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    tags :: Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)],
    lastUpdatedTimes :: Lude.Maybe (Lude.NonEmpty DateTimeRange),
    eventARNs :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityFilter' with the minimum fields required to make a request.
--
-- * 'entityARNs' - A list of entity ARNs (unique identifiers).
-- * 'entityValues' - A list of IDs for affected entities.
-- * 'eventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
-- * 'lastUpdatedTimes' - A list of the most recent dates and times that the entity was updated.
-- * 'statusCodes' - A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
-- * 'tags' - A map of entity tags attached to the affected entity.
mkEntityFilter ::
  -- | 'eventARNs'
  Lude.NonEmpty Lude.Text ->
  EntityFilter
mkEntityFilter pEventARNs_ =
  EntityFilter'
    { statusCodes = Lude.Nothing,
      entityARNs = Lude.Nothing,
      entityValues = Lude.Nothing,
      tags = Lude.Nothing,
      lastUpdatedTimes = Lude.Nothing,
      eventARNs = pEventARNs_
    }

-- | A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
--
-- /Note:/ Consider using 'statusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatusCodes :: Lens.Lens' EntityFilter (Lude.Maybe (Lude.NonEmpty EntityStatusCode))
eStatusCodes = Lens.lens (statusCodes :: EntityFilter -> Lude.Maybe (Lude.NonEmpty EntityStatusCode)) (\s a -> s {statusCodes = a} :: EntityFilter)
{-# DEPRECATED eStatusCodes "Use generic-lens or generic-optics with 'statusCodes' instead." #-}

-- | A list of entity ARNs (unique identifiers).
--
-- /Note:/ Consider using 'entityARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEntityARNs :: Lens.Lens' EntityFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
eEntityARNs = Lens.lens (entityARNs :: EntityFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {entityARNs = a} :: EntityFilter)
{-# DEPRECATED eEntityARNs "Use generic-lens or generic-optics with 'entityARNs' instead." #-}

-- | A list of IDs for affected entities.
--
-- /Note:/ Consider using 'entityValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEntityValues :: Lens.Lens' EntityFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
eEntityValues = Lens.lens (entityValues :: EntityFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {entityValues = a} :: EntityFilter)
{-# DEPRECATED eEntityValues "Use generic-lens or generic-optics with 'entityValues' instead." #-}

-- | A map of entity tags attached to the affected entity.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' EntityFilter (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
eTags = Lens.lens (tags :: EntityFilter -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {tags = a} :: EntityFilter)
{-# DEPRECATED eTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of the most recent dates and times that the entity was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastUpdatedTimes :: Lens.Lens' EntityFilter (Lude.Maybe (Lude.NonEmpty DateTimeRange))
eLastUpdatedTimes = Lens.lens (lastUpdatedTimes :: EntityFilter -> Lude.Maybe (Lude.NonEmpty DateTimeRange)) (\s a -> s {lastUpdatedTimes = a} :: EntityFilter)
{-# DEPRECATED eLastUpdatedTimes "Use generic-lens or generic-optics with 'lastUpdatedTimes' instead." #-}

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
--
-- /Note:/ Consider using 'eventARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventARNs :: Lens.Lens' EntityFilter (Lude.NonEmpty Lude.Text)
eEventARNs = Lens.lens (eventARNs :: EntityFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {eventARNs = a} :: EntityFilter)
{-# DEPRECATED eEventARNs "Use generic-lens or generic-optics with 'eventARNs' instead." #-}

instance Lude.ToJSON EntityFilter where
  toJSON EntityFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("statusCodes" Lude..=) Lude.<$> statusCodes,
            ("entityArns" Lude..=) Lude.<$> entityARNs,
            ("entityValues" Lude..=) Lude.<$> entityValues,
            ("tags" Lude..=) Lude.<$> tags,
            ("lastUpdatedTimes" Lude..=) Lude.<$> lastUpdatedTimes,
            Lude.Just ("eventArns" Lude..= eventARNs)
          ]
      )
