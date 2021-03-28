{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
  ( OrganizationAffectedEntitiesErrorItem (..)
  -- * Smart constructor
  , mkOrganizationAffectedEntitiesErrorItem
  -- * Lenses
  , oaeeiAwsAccountId
  , oaeeiErrorMessage
  , oaeeiErrorName
  , oaeeiEventArn
  ) where

import qualified Network.AWS.AWSHealth.Types.AwsAccountId as Types
import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> operation cannot find or process a specific entity.
--
-- /See:/ 'mkOrganizationAffectedEntitiesErrorItem' smart constructor.
data OrganizationAffectedEntitiesErrorItem = OrganizationAffectedEntitiesErrorItem'
  { awsAccountId :: Core.Maybe Types.AwsAccountId
    -- ^ The 12-digit AWS account numbers that contains the affected entities.
  , errorMessage :: Core.Maybe Core.Text
    -- ^ The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
  , errorName :: Core.Maybe Core.Text
    -- ^ The name of the error.
  , eventArn :: Core.Maybe Types.EventArn
    -- ^ The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationAffectedEntitiesErrorItem' value with any optional fields omitted.
mkOrganizationAffectedEntitiesErrorItem
    :: OrganizationAffectedEntitiesErrorItem
mkOrganizationAffectedEntitiesErrorItem
  = OrganizationAffectedEntitiesErrorItem'{awsAccountId =
                                             Core.Nothing,
                                           errorMessage = Core.Nothing, errorName = Core.Nothing,
                                           eventArn = Core.Nothing}

-- | The 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiAwsAccountId :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Core.Maybe Types.AwsAccountId)
oaeeiAwsAccountId = Lens.field @"awsAccountId"
{-# INLINEABLE oaeeiAwsAccountId #-}
{-# DEPRECATED awsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead"  #-}

-- | The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiErrorMessage :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Core.Maybe Core.Text)
oaeeiErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE oaeeiErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The name of the error.
--
-- /Note:/ Consider using 'errorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiErrorName :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Core.Maybe Core.Text)
oaeeiErrorName = Lens.field @"errorName"
{-# INLINEABLE oaeeiErrorName #-}
{-# DEPRECATED errorName "Use generic-lens or generic-optics with 'errorName' instead"  #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
--
-- /Note:/ Consider using 'eventArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaeeiEventArn :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Core.Maybe Types.EventArn)
oaeeiEventArn = Lens.field @"eventArn"
{-# INLINEABLE oaeeiEventArn #-}
{-# DEPRECATED eventArn "Use generic-lens or generic-optics with 'eventArn' instead"  #-}

instance Core.FromJSON OrganizationAffectedEntitiesErrorItem where
        parseJSON
          = Core.withObject "OrganizationAffectedEntitiesErrorItem" Core.$
              \ x ->
                OrganizationAffectedEntitiesErrorItem' Core.<$>
                  (x Core..:? "awsAccountId") Core.<*> x Core..:? "errorMessage"
                    Core.<*> x Core..:? "errorName"
                    Core.<*> x Core..:? "eventArn"
