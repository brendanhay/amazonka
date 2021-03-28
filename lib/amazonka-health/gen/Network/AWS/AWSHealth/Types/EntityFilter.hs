{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EntityFilter
  ( EntityFilter (..)
  -- * Smart constructor
  , mkEntityFilter
  -- * Lenses
  , efEventArns
  , efEntityArns
  , efEntityValues
  , efLastUpdatedTimes
  , efStatusCodes
  , efTags
  ) where

import qualified Network.AWS.AWSHealth.Types.DateTimeRange as Types
import qualified Network.AWS.AWSHealth.Types.EntityArn as Types
import qualified Network.AWS.AWSHealth.Types.EntityStatusCode as Types
import qualified Network.AWS.AWSHealth.Types.EntityValue as Types
import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.AWSHealth.Types.TagKey as Types
import qualified Network.AWS.AWSHealth.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter> operation.
--
-- /See:/ 'mkEntityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { eventArns :: Core.NonEmpty Types.EventArn
    -- ^ A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@ 
  , entityArns :: Core.Maybe (Core.NonEmpty Types.EntityArn)
    -- ^ A list of entity ARNs (unique identifiers).
  , entityValues :: Core.Maybe (Core.NonEmpty Types.EntityValue)
    -- ^ A list of IDs for affected entities.
  , lastUpdatedTimes :: Core.Maybe (Core.NonEmpty Types.DateTimeRange)
    -- ^ A list of the most recent dates and times that the entity was updated.
  , statusCodes :: Core.Maybe (Core.NonEmpty Types.EntityStatusCode)
    -- ^ A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
  , tags :: Core.Maybe [Core.HashMap Types.TagKey Types.TagValue]
    -- ^ A map of entity tags attached to the affected entity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EntityFilter' value with any optional fields omitted.
mkEntityFilter
    :: Core.NonEmpty Types.EventArn -- ^ 'eventArns'
    -> EntityFilter
mkEntityFilter eventArns
  = EntityFilter'{eventArns, entityArns = Core.Nothing,
                  entityValues = Core.Nothing, lastUpdatedTimes = Core.Nothing,
                  statusCodes = Core.Nothing, tags = Core.Nothing}

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@ 
--
-- /Note:/ Consider using 'eventArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEventArns :: Lens.Lens' EntityFilter (Core.NonEmpty Types.EventArn)
efEventArns = Lens.field @"eventArns"
{-# INLINEABLE efEventArns #-}
{-# DEPRECATED eventArns "Use generic-lens or generic-optics with 'eventArns' instead"  #-}

-- | A list of entity ARNs (unique identifiers).
--
-- /Note:/ Consider using 'entityArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEntityArns :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty Types.EntityArn))
efEntityArns = Lens.field @"entityArns"
{-# INLINEABLE efEntityArns #-}
{-# DEPRECATED entityArns "Use generic-lens or generic-optics with 'entityArns' instead"  #-}

-- | A list of IDs for affected entities.
--
-- /Note:/ Consider using 'entityValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEntityValues :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty Types.EntityValue))
efEntityValues = Lens.field @"entityValues"
{-# INLINEABLE efEntityValues #-}
{-# DEPRECATED entityValues "Use generic-lens or generic-optics with 'entityValues' instead"  #-}

-- | A list of the most recent dates and times that the entity was updated.
--
-- /Note:/ Consider using 'lastUpdatedTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efLastUpdatedTimes :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty Types.DateTimeRange))
efLastUpdatedTimes = Lens.field @"lastUpdatedTimes"
{-# INLINEABLE efLastUpdatedTimes #-}
{-# DEPRECATED lastUpdatedTimes "Use generic-lens or generic-optics with 'lastUpdatedTimes' instead"  #-}

-- | A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
--
-- /Note:/ Consider using 'statusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efStatusCodes :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty Types.EntityStatusCode))
efStatusCodes = Lens.field @"statusCodes"
{-# INLINEABLE efStatusCodes #-}
{-# DEPRECATED statusCodes "Use generic-lens or generic-optics with 'statusCodes' instead"  #-}

-- | A map of entity tags attached to the affected entity.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efTags :: Lens.Lens' EntityFilter (Core.Maybe [Core.HashMap Types.TagKey Types.TagValue])
efTags = Lens.field @"tags"
{-# INLINEABLE efTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON EntityFilter where
        toJSON EntityFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("eventArns" Core..= eventArns),
                  ("entityArns" Core..=) Core.<$> entityArns,
                  ("entityValues" Core..=) Core.<$> entityValues,
                  ("lastUpdatedTimes" Core..=) Core.<$> lastUpdatedTimes,
                  ("statusCodes" Core..=) Core.<$> statusCodes,
                  ("tags" Core..=) Core.<$> tags])
