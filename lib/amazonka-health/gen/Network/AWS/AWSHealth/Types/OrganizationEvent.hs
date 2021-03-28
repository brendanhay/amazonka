{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.OrganizationEvent
  ( OrganizationEvent (..)
  -- * Smart constructor
  , mkOrganizationEvent
  -- * Lenses
  , oeArn
  , oeEndTime
  , oeEventScopeCode
  , oeEventTypeCategory
  , oeEventTypeCode
  , oeLastUpdatedTime
  , oeRegion
  , oeService
  , oeStartTime
  , oeStatusCode
  ) where

import qualified Network.AWS.AWSHealth.Types.Arn as Types
import qualified Network.AWS.AWSHealth.Types.EventScopeCode as Types
import qualified Network.AWS.AWSHealth.Types.EventStatusCode as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCategory as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCode as Types
import qualified Network.AWS.AWSHealth.Types.Region as Types
import qualified Network.AWS.AWSHealth.Types.Service as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about an event, returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization> operation.
--
-- /See:/ 'mkOrganizationEvent' smart constructor.
data OrganizationEvent = OrganizationEvent'
  { arn :: Core.Maybe Types.Arn
    -- ^ The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the event ended.
  , eventScopeCode :: Core.Maybe Types.EventScopeCode
    -- ^ This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
--
--
--     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.
--
--
--     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.
--
--
--     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
--
--
  , eventTypeCategory :: Core.Maybe Types.EventTypeCategory
    -- ^ The category of the event type.
  , eventTypeCode :: Core.Maybe Types.EventTypeCode
    -- ^ The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
  , lastUpdatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The most recent date and time that the event was updated.
  , region :: Core.Maybe Types.Region
    -- ^ The AWS Region name of the event.
  , service :: Core.Maybe Types.Service
    -- ^ The AWS service that is affected by the event. For example, EC2, RDS.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the event began.
  , statusCode :: Core.Maybe Types.EventStatusCode
    -- ^ The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OrganizationEvent' value with any optional fields omitted.
mkOrganizationEvent
    :: OrganizationEvent
mkOrganizationEvent
  = OrganizationEvent'{arn = Core.Nothing, endTime = Core.Nothing,
                       eventScopeCode = Core.Nothing, eventTypeCategory = Core.Nothing,
                       eventTypeCode = Core.Nothing, lastUpdatedTime = Core.Nothing,
                       region = Core.Nothing, service = Core.Nothing,
                       startTime = Core.Nothing, statusCode = Core.Nothing}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeArn :: Lens.Lens' OrganizationEvent (Core.Maybe Types.Arn)
oeArn = Lens.field @"arn"
{-# INLINEABLE oeArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time that the event ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEndTime :: Lens.Lens' OrganizationEvent (Core.Maybe Core.NominalDiffTime)
oeEndTime = Lens.field @"endTime"
{-# INLINEABLE oeEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
--
--
--     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.
--
--
--     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.
--
--
--     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
--
--
--
-- /Note:/ Consider using 'eventScopeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEventScopeCode :: Lens.Lens' OrganizationEvent (Core.Maybe Types.EventScopeCode)
oeEventScopeCode = Lens.field @"eventScopeCode"
{-# INLINEABLE oeEventScopeCode #-}
{-# DEPRECATED eventScopeCode "Use generic-lens or generic-optics with 'eventScopeCode' instead"  #-}

-- | The category of the event type.
--
-- /Note:/ Consider using 'eventTypeCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEventTypeCategory :: Lens.Lens' OrganizationEvent (Core.Maybe Types.EventTypeCategory)
oeEventTypeCategory = Lens.field @"eventTypeCategory"
{-# INLINEABLE oeEventTypeCategory #-}
{-# DEPRECATED eventTypeCategory "Use generic-lens or generic-optics with 'eventTypeCategory' instead"  #-}

-- | The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'eventTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEventTypeCode :: Lens.Lens' OrganizationEvent (Core.Maybe Types.EventTypeCode)
oeEventTypeCode = Lens.field @"eventTypeCode"
{-# INLINEABLE oeEventTypeCode #-}
{-# DEPRECATED eventTypeCode "Use generic-lens or generic-optics with 'eventTypeCode' instead"  #-}

-- | The most recent date and time that the event was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeLastUpdatedTime :: Lens.Lens' OrganizationEvent (Core.Maybe Core.NominalDiffTime)
oeLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# INLINEABLE oeLastUpdatedTime #-}
{-# DEPRECATED lastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead"  #-}

-- | The AWS Region name of the event.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeRegion :: Lens.Lens' OrganizationEvent (Core.Maybe Types.Region)
oeRegion = Lens.field @"region"
{-# INLINEABLE oeRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The AWS service that is affected by the event. For example, EC2, RDS.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeService :: Lens.Lens' OrganizationEvent (Core.Maybe Types.Service)
oeService = Lens.field @"service"
{-# INLINEABLE oeService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The date and time that the event began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeStartTime :: Lens.Lens' OrganizationEvent (Core.Maybe Core.NominalDiffTime)
oeStartTime = Lens.field @"startTime"
{-# INLINEABLE oeStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeStatusCode :: Lens.Lens' OrganizationEvent (Core.Maybe Types.EventStatusCode)
oeStatusCode = Lens.field @"statusCode"
{-# INLINEABLE oeStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.FromJSON OrganizationEvent where
        parseJSON
          = Core.withObject "OrganizationEvent" Core.$
              \ x ->
                OrganizationEvent' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "endTime" Core.<*>
                    x Core..:? "eventScopeCode"
                    Core.<*> x Core..:? "eventTypeCategory"
                    Core.<*> x Core..:? "eventTypeCode"
                    Core.<*> x Core..:? "lastUpdatedTime"
                    Core.<*> x Core..:? "region"
                    Core.<*> x Core..:? "service"
                    Core.<*> x Core..:? "startTime"
                    Core.<*> x Core..:? "statusCode"
