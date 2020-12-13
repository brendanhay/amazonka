{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
  ( CloudWatchLoggingOption (..),

    -- * Smart constructor
    mkCloudWatchLoggingOption,

    -- * Lenses
    cwloLogStreamARN,
    cwloRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.
--
-- /See:/ 'mkCloudWatchLoggingOption' smart constructor.
data CloudWatchLoggingOption = CloudWatchLoggingOption'
  { -- | ARN of the CloudWatch log to receive application messages.
    logStreamARN :: Lude.Text,
    -- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- * 'logStreamARN' - ARN of the CloudWatch log to receive application messages.
-- * 'roleARN' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
mkCloudWatchLoggingOption ::
  -- | 'logStreamARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CloudWatchLoggingOption
mkCloudWatchLoggingOption pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOption'
    { logStreamARN = pLogStreamARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the CloudWatch log to receive application messages.
--
-- /Note:/ Consider using 'logStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloLogStreamARN :: Lens.Lens' CloudWatchLoggingOption Lude.Text
cwloLogStreamARN = Lens.lens (logStreamARN :: CloudWatchLoggingOption -> Lude.Text) (\s a -> s {logStreamARN = a} :: CloudWatchLoggingOption)
{-# DEPRECATED cwloLogStreamARN "Use generic-lens or generic-optics with 'logStreamARN' instead." #-}

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwloRoleARN :: Lens.Lens' CloudWatchLoggingOption Lude.Text
cwloRoleARN = Lens.lens (roleARN :: CloudWatchLoggingOption -> Lude.Text) (\s a -> s {roleARN = a} :: CloudWatchLoggingOption)
{-# DEPRECATED cwloRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON CloudWatchLoggingOption where
  toJSON CloudWatchLoggingOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LogStreamARN" Lude..= logStreamARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
