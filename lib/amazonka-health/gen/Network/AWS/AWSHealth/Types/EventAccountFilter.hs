{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAccountFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventAccountFilter
  ( EventAccountFilter (..)
  -- * Smart constructor
  , mkEventAccountFilter
  -- * Lenses
  , eafEventArn
  , eafAwsAccountId
  ) where

import qualified Network.AWS.AWSHealth.Types.AccountId as Types
import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values used to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> and <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> operations.
--
-- /See:/ 'mkEventAccountFilter' smart constructor.
data EventAccountFilter = EventAccountFilter'
  { eventArn :: Types.EventArn
    -- ^ The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
  , awsAccountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit AWS account numbers that contains the affected entities.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventAccountFilter' value with any optional fields omitted.
mkEventAccountFilter
    :: Types.EventArn -- ^ 'eventArn'
    -> EventAccountFilter
mkEventAccountFilter eventArn
  = EventAccountFilter'{eventArn, awsAccountId = Core.Nothing}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
--
-- /Note:/ Consider using 'eventArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafEventArn :: Lens.Lens' EventAccountFilter Types.EventArn
eafEventArn = Lens.field @"eventArn"
{-# INLINEABLE eafEventArn #-}
{-# DEPRECATED eventArn "Use generic-lens or generic-optics with 'eventArn' instead"  #-}

-- | The 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafAwsAccountId :: Lens.Lens' EventAccountFilter (Core.Maybe Types.AccountId)
eafAwsAccountId = Lens.field @"awsAccountId"
{-# INLINEABLE eafAwsAccountId #-}
{-# DEPRECATED awsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead"  #-}

instance Core.FromJSON EventAccountFilter where
        toJSON EventAccountFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("eventArn" Core..= eventArn),
                  ("awsAccountId" Core..=) Core.<$> awsAccountId])
