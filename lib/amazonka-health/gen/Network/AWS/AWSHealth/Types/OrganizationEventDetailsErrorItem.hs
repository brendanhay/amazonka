{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
  ( OrganizationEventDetailsErrorItem (..),

    -- * Smart constructor
    mkOrganizationEventDetailsErrorItem,

    -- * Lenses
    oedeiAwsAccountId,
    oedeiErrorMessage,
    oedeiErrorName,
    oedeiEventArn,
  )
where

import qualified Network.AWS.AWSHealth.Types.AwsAccountId as Types
import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.AWSHealth.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
--
-- /See:/ 'mkOrganizationEventDetailsErrorItem' smart constructor.
data OrganizationEventDetailsErrorItem = OrganizationEventDetailsErrorItem'
  { -- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
    awsAccountId :: Core.Maybe Types.AwsAccountId,
    -- | A message that describes the error.
    errorMessage :: Core.Maybe Types.String,
    -- | The name of the error.
    errorName :: Core.Maybe Types.String,
    -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Core.Maybe Types.EventArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationEventDetailsErrorItem' value with any optional fields omitted.
mkOrganizationEventDetailsErrorItem ::
  OrganizationEventDetailsErrorItem
mkOrganizationEventDetailsErrorItem =
  OrganizationEventDetailsErrorItem'
    { awsAccountId = Core.Nothing,
      errorMessage = Core.Nothing,
      errorName = Core.Nothing,
      eventArn = Core.Nothing
    }

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiAwsAccountId :: Lens.Lens' OrganizationEventDetailsErrorItem (Core.Maybe Types.AwsAccountId)
oedeiAwsAccountId = Lens.field @"awsAccountId"
{-# DEPRECATED oedeiAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | A message that describes the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiErrorMessage :: Lens.Lens' OrganizationEventDetailsErrorItem (Core.Maybe Types.String)
oedeiErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED oedeiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The name of the error.
--
-- /Note:/ Consider using 'errorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiErrorName :: Lens.Lens' OrganizationEventDetailsErrorItem (Core.Maybe Types.String)
oedeiErrorName = Lens.field @"errorName"
{-# DEPRECATED oedeiErrorName "Use generic-lens or generic-optics with 'errorName' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedeiEventArn :: Lens.Lens' OrganizationEventDetailsErrorItem (Core.Maybe Types.EventArn)
oedeiEventArn = Lens.field @"eventArn"
{-# DEPRECATED oedeiEventArn "Use generic-lens or generic-optics with 'eventArn' instead." #-}

instance Core.FromJSON OrganizationEventDetailsErrorItem where
  parseJSON =
    Core.withObject "OrganizationEventDetailsErrorItem" Core.$
      \x ->
        OrganizationEventDetailsErrorItem'
          Core.<$> (x Core..:? "awsAccountId")
          Core.<*> (x Core..:? "errorMessage")
          Core.<*> (x Core..:? "errorName")
          Core.<*> (x Core..:? "eventArn")
