{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.WorkmailAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.WorkmailAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.
--
--
-- For information using a receipt rule to call Amazon WorkMail, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'workmailAction' smart constructor.
data WorkmailAction = WorkmailAction'
  { _waTopicARN :: !(Maybe Text),
    _waOrganizationARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkmailAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'waOrganizationARN' - The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <https://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
workmailAction ::
  -- | 'waOrganizationARN'
  Text ->
  WorkmailAction
workmailAction pOrganizationARN_ =
  WorkmailAction'
    { _waTopicARN = Nothing,
      _waOrganizationARN = pOrganizationARN_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
waTopicARN :: Lens' WorkmailAction (Maybe Text)
waTopicARN = lens _waTopicARN (\s a -> s {_waTopicARN = a})

-- | The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <https://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
waOrganizationARN :: Lens' WorkmailAction Text
waOrganizationARN = lens _waOrganizationARN (\s a -> s {_waOrganizationARN = a})

instance FromXML WorkmailAction where
  parseXML x =
    WorkmailAction'
      <$> (x .@? "TopicArn") <*> (x .@ "OrganizationArn")

instance Hashable WorkmailAction

instance NFData WorkmailAction

instance ToQuery WorkmailAction where
  toQuery WorkmailAction' {..} =
    mconcat
      [ "TopicArn" =: _waTopicARN,
        "OrganizationArn" =: _waOrganizationARN
      ]
