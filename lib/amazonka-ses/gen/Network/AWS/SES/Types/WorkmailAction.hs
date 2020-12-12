{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.WorkmailAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.WorkmailAction
  ( WorkmailAction (..),

    -- * Smart constructor
    mkWorkmailAction,

    -- * Lenses
    waTopicARN,
    waOrganizationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.
--
-- For information using a receipt rule to call Amazon WorkMail, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkWorkmailAction' smart constructor.
data WorkmailAction = WorkmailAction'
  { topicARN ::
      Lude.Maybe Lude.Text,
    organizationARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkmailAction' with the minimum fields required to make a request.
--
-- * 'organizationARN' - The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <https://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
mkWorkmailAction ::
  -- | 'organizationARN'
  Lude.Text ->
  WorkmailAction
mkWorkmailAction pOrganizationARN_ =
  WorkmailAction'
    { topicARN = Lude.Nothing,
      organizationARN = pOrganizationARN_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waTopicARN :: Lens.Lens' WorkmailAction (Lude.Maybe Lude.Text)
waTopicARN = Lens.lens (topicARN :: WorkmailAction -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: WorkmailAction)
{-# DEPRECATED waTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <https://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
--
-- /Note:/ Consider using 'organizationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waOrganizationARN :: Lens.Lens' WorkmailAction Lude.Text
waOrganizationARN = Lens.lens (organizationARN :: WorkmailAction -> Lude.Text) (\s a -> s {organizationARN = a} :: WorkmailAction)
{-# DEPRECATED waOrganizationARN "Use generic-lens or generic-optics with 'organizationARN' instead." #-}

instance Lude.FromXML WorkmailAction where
  parseXML x =
    WorkmailAction'
      Lude.<$> (x Lude..@? "TopicArn") Lude.<*> (x Lude..@ "OrganizationArn")

instance Lude.ToQuery WorkmailAction where
  toQuery WorkmailAction' {..} =
    Lude.mconcat
      [ "TopicArn" Lude.=: topicARN,
        "OrganizationArn" Lude.=: organizationARN
      ]
