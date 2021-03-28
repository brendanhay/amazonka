{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.WorkmailAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.WorkmailAction
  ( WorkmailAction (..)
  -- * Smart constructor
  , mkWorkmailAction
  -- * Lenses
  , waOrganizationArn
  , waTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.OrganizationArn as Types
import qualified Network.AWS.SES.Types.TopicArn as Types

-- | When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.
--
-- For information using a receipt rule to call Amazon WorkMail, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkWorkmailAction' smart constructor.
data WorkmailAction = WorkmailAction'
  { organizationArn :: Types.OrganizationArn
    -- ^ The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <https://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
  , topicArn :: Core.Maybe Types.TopicArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkmailAction' value with any optional fields omitted.
mkWorkmailAction
    :: Types.OrganizationArn -- ^ 'organizationArn'
    -> WorkmailAction
mkWorkmailAction organizationArn
  = WorkmailAction'{organizationArn, topicArn = Core.Nothing}

-- | The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <https://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
--
-- /Note:/ Consider using 'organizationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waOrganizationArn :: Lens.Lens' WorkmailAction Types.OrganizationArn
waOrganizationArn = Lens.field @"organizationArn"
{-# INLINEABLE waOrganizationArn #-}
{-# DEPRECATED organizationArn "Use generic-lens or generic-optics with 'organizationArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waTopicArn :: Lens.Lens' WorkmailAction (Core.Maybe Types.TopicArn)
waTopicArn = Lens.field @"topicArn"
{-# INLINEABLE waTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery WorkmailAction where
        toQuery WorkmailAction{..}
          = Core.toQueryPair "OrganizationArn" organizationArn Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TopicArn") topicArn

instance Core.FromXML WorkmailAction where
        parseXML x
          = WorkmailAction' Core.<$>
              (x Core..@ "OrganizationArn") Core.<*> x Core..@? "TopicArn"
