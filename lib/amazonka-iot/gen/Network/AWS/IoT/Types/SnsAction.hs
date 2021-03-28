{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SnsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.SnsAction
  ( SnsAction (..)
  -- * Smart constructor
  , mkSnsAction
  -- * Lenses
  , safTargetArn
  , safRoleArn
  , safMessageFormat
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.MessageFormat as Types
import qualified Network.AWS.IoT.Types.TargetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to publish to an Amazon SNS topic.
--
-- /See:/ 'mkSnsAction' smart constructor.
data SnsAction = SnsAction'
  { targetArn :: Types.TargetArn
    -- ^ The ARN of the SNS topic.
  , roleArn :: Types.AwsArn
    -- ^ The ARN of the IAM role that grants access.
  , messageFormat :: Core.Maybe Types.MessageFormat
    -- ^ (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnsAction' value with any optional fields omitted.
mkSnsAction
    :: Types.TargetArn -- ^ 'targetArn'
    -> Types.AwsArn -- ^ 'roleArn'
    -> SnsAction
mkSnsAction targetArn roleArn
  = SnsAction'{targetArn, roleArn, messageFormat = Core.Nothing}

-- | The ARN of the SNS topic.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safTargetArn :: Lens.Lens' SnsAction Types.TargetArn
safTargetArn = Lens.field @"targetArn"
{-# INLINEABLE safTargetArn #-}
{-# DEPRECATED targetArn "Use generic-lens or generic-optics with 'targetArn' instead"  #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safRoleArn :: Lens.Lens' SnsAction Types.AwsArn
safRoleArn = Lens.field @"roleArn"
{-# INLINEABLE safRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safMessageFormat :: Lens.Lens' SnsAction (Core.Maybe Types.MessageFormat)
safMessageFormat = Lens.field @"messageFormat"
{-# INLINEABLE safMessageFormat #-}
{-# DEPRECATED messageFormat "Use generic-lens or generic-optics with 'messageFormat' instead"  #-}

instance Core.FromJSON SnsAction where
        toJSON SnsAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("targetArn" Core..= targetArn),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("messageFormat" Core..=) Core.<$> messageFormat])

instance Core.FromJSON SnsAction where
        parseJSON
          = Core.withObject "SnsAction" Core.$
              \ x ->
                SnsAction' Core.<$>
                  (x Core..: "targetArn") Core.<*> x Core..: "roleArn" Core.<*>
                    x Core..:? "messageFormat"
