{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SqsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.SqsAction
  ( SqsAction (..)
  -- * Smart constructor
  , mkSqsAction
  -- * Lenses
  , saRoleArn
  , saQueueUrl
  , saUseBase64
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.QueueUrl as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to publish data to an Amazon SQS queue.
--
-- /See:/ 'mkSqsAction' smart constructor.
data SqsAction = SqsAction'
  { roleArn :: Types.AwsArn
    -- ^ The ARN of the IAM role that grants access.
  , queueUrl :: Types.QueueUrl
    -- ^ The URL of the Amazon SQS queue.
  , useBase64 :: Core.Maybe Core.Bool
    -- ^ Specifies whether to use Base64 encoding.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SqsAction' value with any optional fields omitted.
mkSqsAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.QueueUrl -- ^ 'queueUrl'
    -> SqsAction
mkSqsAction roleArn queueUrl
  = SqsAction'{roleArn, queueUrl, useBase64 = Core.Nothing}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saRoleArn :: Lens.Lens' SqsAction Types.AwsArn
saRoleArn = Lens.field @"roleArn"
{-# INLINEABLE saRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The URL of the Amazon SQS queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saQueueUrl :: Lens.Lens' SqsAction Types.QueueUrl
saQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE saQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | Specifies whether to use Base64 encoding.
--
-- /Note:/ Consider using 'useBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saUseBase64 :: Lens.Lens' SqsAction (Core.Maybe Core.Bool)
saUseBase64 = Lens.field @"useBase64"
{-# INLINEABLE saUseBase64 #-}
{-# DEPRECATED useBase64 "Use generic-lens or generic-optics with 'useBase64' instead"  #-}

instance Core.FromJSON SqsAction where
        toJSON SqsAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("queueUrl" Core..= queueUrl),
                  ("useBase64" Core..=) Core.<$> useBase64])

instance Core.FromJSON SqsAction where
        parseJSON
          = Core.withObject "SqsAction" Core.$
              \ x ->
                SqsAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "queueUrl" Core.<*>
                    x Core..:? "useBase64"
