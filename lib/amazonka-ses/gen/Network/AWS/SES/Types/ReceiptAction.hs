-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptAction
  ( ReceiptAction (..),

    -- * Smart constructor
    mkReceiptAction,

    -- * Lenses
    raAddHeaderAction,
    raSNSAction,
    raWorkmailAction,
    raBounceAction,
    raLambdaAction,
    raStopAction,
    raS3Action,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.AddHeaderAction
import Network.AWS.SES.Types.BounceAction
import Network.AWS.SES.Types.LambdaAction
import Network.AWS.SES.Types.S3Action
import Network.AWS.SES.Types.SNSAction
import Network.AWS.SES.Types.StopAction
import Network.AWS.SES.Types.WorkmailAction

-- | An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptAction' smart constructor.
data ReceiptAction = ReceiptAction'
  { addHeaderAction ::
      Lude.Maybe AddHeaderAction,
    snsAction :: Lude.Maybe SNSAction,
    workmailAction :: Lude.Maybe WorkmailAction,
    bounceAction :: Lude.Maybe BounceAction,
    lambdaAction :: Lude.Maybe LambdaAction,
    stopAction :: Lude.Maybe StopAction,
    s3Action :: Lude.Maybe S3Action
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReceiptAction' with the minimum fields required to make a request.
--
-- * 'addHeaderAction' - Adds a header to the received email.
-- * 'bounceAction' - Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
-- * 'lambdaAction' - Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
-- * 's3Action' - Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
-- * 'snsAction' - Publishes the email content within a notification to Amazon SNS.
-- * 'stopAction' - Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
-- * 'workmailAction' - Calls Amazon WorkMail and, optionally, publishes a notification to Amazon Amazon SNS.
mkReceiptAction ::
  ReceiptAction
mkReceiptAction =
  ReceiptAction'
    { addHeaderAction = Lude.Nothing,
      snsAction = Lude.Nothing,
      workmailAction = Lude.Nothing,
      bounceAction = Lude.Nothing,
      lambdaAction = Lude.Nothing,
      stopAction = Lude.Nothing,
      s3Action = Lude.Nothing
    }

-- | Adds a header to the received email.
--
-- /Note:/ Consider using 'addHeaderAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAddHeaderAction :: Lens.Lens' ReceiptAction (Lude.Maybe AddHeaderAction)
raAddHeaderAction = Lens.lens (addHeaderAction :: ReceiptAction -> Lude.Maybe AddHeaderAction) (\s a -> s {addHeaderAction = a} :: ReceiptAction)
{-# DEPRECATED raAddHeaderAction "Use generic-lens or generic-optics with 'addHeaderAction' instead." #-}

-- | Publishes the email content within a notification to Amazon SNS.
--
-- /Note:/ Consider using 'snsAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raSNSAction :: Lens.Lens' ReceiptAction (Lude.Maybe SNSAction)
raSNSAction = Lens.lens (snsAction :: ReceiptAction -> Lude.Maybe SNSAction) (\s a -> s {snsAction = a} :: ReceiptAction)
{-# DEPRECATED raSNSAction "Use generic-lens or generic-optics with 'snsAction' instead." #-}

-- | Calls Amazon WorkMail and, optionally, publishes a notification to Amazon Amazon SNS.
--
-- /Note:/ Consider using 'workmailAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raWorkmailAction :: Lens.Lens' ReceiptAction (Lude.Maybe WorkmailAction)
raWorkmailAction = Lens.lens (workmailAction :: ReceiptAction -> Lude.Maybe WorkmailAction) (\s a -> s {workmailAction = a} :: ReceiptAction)
{-# DEPRECATED raWorkmailAction "Use generic-lens or generic-optics with 'workmailAction' instead." #-}

-- | Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- /Note:/ Consider using 'bounceAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raBounceAction :: Lens.Lens' ReceiptAction (Lude.Maybe BounceAction)
raBounceAction = Lens.lens (bounceAction :: ReceiptAction -> Lude.Maybe BounceAction) (\s a -> s {bounceAction = a} :: ReceiptAction)
{-# DEPRECATED raBounceAction "Use generic-lens or generic-optics with 'bounceAction' instead." #-}

-- | Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
--
-- /Note:/ Consider using 'lambdaAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raLambdaAction :: Lens.Lens' ReceiptAction (Lude.Maybe LambdaAction)
raLambdaAction = Lens.lens (lambdaAction :: ReceiptAction -> Lude.Maybe LambdaAction) (\s a -> s {lambdaAction = a} :: ReceiptAction)
{-# DEPRECATED raLambdaAction "Use generic-lens or generic-optics with 'lambdaAction' instead." #-}

-- | Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
--
-- /Note:/ Consider using 'stopAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raStopAction :: Lens.Lens' ReceiptAction (Lude.Maybe StopAction)
raStopAction = Lens.lens (stopAction :: ReceiptAction -> Lude.Maybe StopAction) (\s a -> s {stopAction = a} :: ReceiptAction)
{-# DEPRECATED raStopAction "Use generic-lens or generic-optics with 'stopAction' instead." #-}

-- | Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
--
-- /Note:/ Consider using 's3Action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raS3Action :: Lens.Lens' ReceiptAction (Lude.Maybe S3Action)
raS3Action = Lens.lens (s3Action :: ReceiptAction -> Lude.Maybe S3Action) (\s a -> s {s3Action = a} :: ReceiptAction)
{-# DEPRECATED raS3Action "Use generic-lens or generic-optics with 's3Action' instead." #-}

instance Lude.FromXML ReceiptAction where
  parseXML x =
    ReceiptAction'
      Lude.<$> (x Lude..@? "AddHeaderAction")
      Lude.<*> (x Lude..@? "SNSAction")
      Lude.<*> (x Lude..@? "WorkmailAction")
      Lude.<*> (x Lude..@? "BounceAction")
      Lude.<*> (x Lude..@? "LambdaAction")
      Lude.<*> (x Lude..@? "StopAction")
      Lude.<*> (x Lude..@? "S3Action")

instance Lude.ToQuery ReceiptAction where
  toQuery ReceiptAction' {..} =
    Lude.mconcat
      [ "AddHeaderAction" Lude.=: addHeaderAction,
        "SNSAction" Lude.=: snsAction,
        "WorkmailAction" Lude.=: workmailAction,
        "BounceAction" Lude.=: bounceAction,
        "LambdaAction" Lude.=: lambdaAction,
        "StopAction" Lude.=: stopAction,
        "S3Action" Lude.=: s3Action
      ]
