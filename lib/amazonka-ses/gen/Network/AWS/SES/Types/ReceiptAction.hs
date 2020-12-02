{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptAction where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.AddHeaderAction
import Network.AWS.SES.Types.BounceAction
import Network.AWS.SES.Types.LambdaAction
import Network.AWS.SES.Types.S3Action
import Network.AWS.SES.Types.SNSAction
import Network.AWS.SES.Types.StopAction
import Network.AWS.SES.Types.WorkmailAction

-- | An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.
--
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptAction' smart constructor.
data ReceiptAction = ReceiptAction'
  { _raAddHeaderAction ::
      !(Maybe AddHeaderAction),
    _raSNSAction :: !(Maybe SNSAction),
    _raWorkmailAction :: !(Maybe WorkmailAction),
    _raBounceAction :: !(Maybe BounceAction),
    _raLambdaAction :: !(Maybe LambdaAction),
    _raStopAction :: !(Maybe StopAction),
    _raS3Action :: !(Maybe S3Action)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReceiptAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAddHeaderAction' - Adds a header to the received email.
--
-- * 'raSNSAction' - Publishes the email content within a notification to Amazon SNS.
--
-- * 'raWorkmailAction' - Calls Amazon WorkMail and, optionally, publishes a notification to Amazon Amazon SNS.
--
-- * 'raBounceAction' - Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- * 'raLambdaAction' - Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
--
-- * 'raStopAction' - Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
--
-- * 'raS3Action' - Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
receiptAction ::
  ReceiptAction
receiptAction =
  ReceiptAction'
    { _raAddHeaderAction = Nothing,
      _raSNSAction = Nothing,
      _raWorkmailAction = Nothing,
      _raBounceAction = Nothing,
      _raLambdaAction = Nothing,
      _raStopAction = Nothing,
      _raS3Action = Nothing
    }

-- | Adds a header to the received email.
raAddHeaderAction :: Lens' ReceiptAction (Maybe AddHeaderAction)
raAddHeaderAction = lens _raAddHeaderAction (\s a -> s {_raAddHeaderAction = a})

-- | Publishes the email content within a notification to Amazon SNS.
raSNSAction :: Lens' ReceiptAction (Maybe SNSAction)
raSNSAction = lens _raSNSAction (\s a -> s {_raSNSAction = a})

-- | Calls Amazon WorkMail and, optionally, publishes a notification to Amazon Amazon SNS.
raWorkmailAction :: Lens' ReceiptAction (Maybe WorkmailAction)
raWorkmailAction = lens _raWorkmailAction (\s a -> s {_raWorkmailAction = a})

-- | Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
raBounceAction :: Lens' ReceiptAction (Maybe BounceAction)
raBounceAction = lens _raBounceAction (\s a -> s {_raBounceAction = a})

-- | Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
raLambdaAction :: Lens' ReceiptAction (Maybe LambdaAction)
raLambdaAction = lens _raLambdaAction (\s a -> s {_raLambdaAction = a})

-- | Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
raStopAction :: Lens' ReceiptAction (Maybe StopAction)
raStopAction = lens _raStopAction (\s a -> s {_raStopAction = a})

-- | Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
raS3Action :: Lens' ReceiptAction (Maybe S3Action)
raS3Action = lens _raS3Action (\s a -> s {_raS3Action = a})

instance FromXML ReceiptAction where
  parseXML x =
    ReceiptAction'
      <$> (x .@? "AddHeaderAction")
      <*> (x .@? "SNSAction")
      <*> (x .@? "WorkmailAction")
      <*> (x .@? "BounceAction")
      <*> (x .@? "LambdaAction")
      <*> (x .@? "StopAction")
      <*> (x .@? "S3Action")

instance Hashable ReceiptAction

instance NFData ReceiptAction

instance ToQuery ReceiptAction where
  toQuery ReceiptAction' {..} =
    mconcat
      [ "AddHeaderAction" =: _raAddHeaderAction,
        "SNSAction" =: _raSNSAction,
        "WorkmailAction" =: _raWorkmailAction,
        "BounceAction" =: _raBounceAction,
        "LambdaAction" =: _raLambdaAction,
        "StopAction" =: _raStopAction,
        "S3Action" =: _raS3Action
      ]
