{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ReceiptAction
  ( ReceiptAction (..)
  -- * Smart constructor
  , mkReceiptAction
  -- * Lenses
  , raAddHeaderAction
  , raBounceAction
  , raLambdaAction
  , raS3Action
  , raSNSAction
  , raStopAction
  , raWorkmailAction
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.AddHeaderAction as Types
import qualified Network.AWS.SES.Types.BounceAction as Types
import qualified Network.AWS.SES.Types.LambdaAction as Types
import qualified Network.AWS.SES.Types.S3Action as Types
import qualified Network.AWS.SES.Types.SNSAction as Types
import qualified Network.AWS.SES.Types.StopAction as Types
import qualified Network.AWS.SES.Types.WorkmailAction as Types

-- | An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.
--
-- For information about setting up receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptAction' smart constructor.
data ReceiptAction = ReceiptAction'
  { addHeaderAction :: Core.Maybe Types.AddHeaderAction
    -- ^ Adds a header to the received email.
  , bounceAction :: Core.Maybe Types.BounceAction
    -- ^ Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
  , lambdaAction :: Core.Maybe Types.LambdaAction
    -- ^ Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
  , s3Action :: Core.Maybe Types.S3Action
    -- ^ Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
  , sNSAction :: Core.Maybe Types.SNSAction
    -- ^ Publishes the email content within a notification to Amazon SNS.
  , stopAction :: Core.Maybe Types.StopAction
    -- ^ Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
  , workmailAction :: Core.Maybe Types.WorkmailAction
    -- ^ Calls Amazon WorkMail and, optionally, publishes a notification to Amazon Amazon SNS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReceiptAction' value with any optional fields omitted.
mkReceiptAction
    :: ReceiptAction
mkReceiptAction
  = ReceiptAction'{addHeaderAction = Core.Nothing,
                   bounceAction = Core.Nothing, lambdaAction = Core.Nothing,
                   s3Action = Core.Nothing, sNSAction = Core.Nothing,
                   stopAction = Core.Nothing, workmailAction = Core.Nothing}

-- | Adds a header to the received email.
--
-- /Note:/ Consider using 'addHeaderAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAddHeaderAction :: Lens.Lens' ReceiptAction (Core.Maybe Types.AddHeaderAction)
raAddHeaderAction = Lens.field @"addHeaderAction"
{-# INLINEABLE raAddHeaderAction #-}
{-# DEPRECATED addHeaderAction "Use generic-lens or generic-optics with 'addHeaderAction' instead"  #-}

-- | Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- /Note:/ Consider using 'bounceAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raBounceAction :: Lens.Lens' ReceiptAction (Core.Maybe Types.BounceAction)
raBounceAction = Lens.field @"bounceAction"
{-# INLINEABLE raBounceAction #-}
{-# DEPRECATED bounceAction "Use generic-lens or generic-optics with 'bounceAction' instead"  #-}

-- | Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
--
-- /Note:/ Consider using 'lambdaAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raLambdaAction :: Lens.Lens' ReceiptAction (Core.Maybe Types.LambdaAction)
raLambdaAction = Lens.field @"lambdaAction"
{-# INLINEABLE raLambdaAction #-}
{-# DEPRECATED lambdaAction "Use generic-lens or generic-optics with 'lambdaAction' instead"  #-}

-- | Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
--
-- /Note:/ Consider using 's3Action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raS3Action :: Lens.Lens' ReceiptAction (Core.Maybe Types.S3Action)
raS3Action = Lens.field @"s3Action"
{-# INLINEABLE raS3Action #-}
{-# DEPRECATED s3Action "Use generic-lens or generic-optics with 's3Action' instead"  #-}

-- | Publishes the email content within a notification to Amazon SNS.
--
-- /Note:/ Consider using 'sNSAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raSNSAction :: Lens.Lens' ReceiptAction (Core.Maybe Types.SNSAction)
raSNSAction = Lens.field @"sNSAction"
{-# INLINEABLE raSNSAction #-}
{-# DEPRECATED sNSAction "Use generic-lens or generic-optics with 'sNSAction' instead"  #-}

-- | Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
--
-- /Note:/ Consider using 'stopAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raStopAction :: Lens.Lens' ReceiptAction (Core.Maybe Types.StopAction)
raStopAction = Lens.field @"stopAction"
{-# INLINEABLE raStopAction #-}
{-# DEPRECATED stopAction "Use generic-lens or generic-optics with 'stopAction' instead"  #-}

-- | Calls Amazon WorkMail and, optionally, publishes a notification to Amazon Amazon SNS.
--
-- /Note:/ Consider using 'workmailAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raWorkmailAction :: Lens.Lens' ReceiptAction (Core.Maybe Types.WorkmailAction)
raWorkmailAction = Lens.field @"workmailAction"
{-# INLINEABLE raWorkmailAction #-}
{-# DEPRECATED workmailAction "Use generic-lens or generic-optics with 'workmailAction' instead"  #-}

instance Core.ToQuery ReceiptAction where
        toQuery ReceiptAction{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AddHeaderAction")
              addHeaderAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "BounceAction")
                bounceAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LambdaAction")
                lambdaAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "S3Action") s3Action
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SNSAction") sNSAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StopAction") stopAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "WorkmailAction")
                workmailAction

instance Core.FromXML ReceiptAction where
        parseXML x
          = ReceiptAction' Core.<$>
              (x Core..@? "AddHeaderAction") Core.<*> x Core..@? "BounceAction"
                Core.<*> x Core..@? "LambdaAction"
                Core.<*> x Core..@? "S3Action"
                Core.<*> x Core..@? "SNSAction"
                Core.<*> x Core..@? "StopAction"
                Core.<*> x Core..@? "WorkmailAction"
