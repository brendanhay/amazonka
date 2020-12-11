-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
  ( CloudWatchLoggingOptionDescription (..),

    -- * Smart constructor
    mkCloudWatchLoggingOptionDescription,

    -- * Lenses
    cwlodCloudWatchLoggingOptionId,
    cwlodLogStreamARN,
    cwlodRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Description of the CloudWatch logging option.
--
-- /See:/ 'mkCloudWatchLoggingOptionDescription' smart constructor.
data CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription'
  { cloudWatchLoggingOptionId ::
      Lude.Maybe Lude.Text,
    logStreamARN ::
      Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLoggingOptionDescription' with the minimum fields required to make a request.
--
-- * 'cloudWatchLoggingOptionId' - ID of the CloudWatch logging option description.
-- * 'logStreamARN' - ARN of the CloudWatch log to receive application messages.
-- * 'roleARN' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
mkCloudWatchLoggingOptionDescription ::
  -- | 'logStreamARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CloudWatchLoggingOptionDescription
mkCloudWatchLoggingOptionDescription pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOptionDescription'
    { cloudWatchLoggingOptionId =
        Lude.Nothing,
      logStreamARN = pLogStreamARN_,
      roleARN = pRoleARN_
    }

-- | ID of the CloudWatch logging option description.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlodCloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionDescription (Lude.Maybe Lude.Text)
cwlodCloudWatchLoggingOptionId = Lens.lens (cloudWatchLoggingOptionId :: CloudWatchLoggingOptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLoggingOptionId = a} :: CloudWatchLoggingOptionDescription)
{-# DEPRECATED cwlodCloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead." #-}

-- | ARN of the CloudWatch log to receive application messages.
--
-- /Note:/ Consider using 'logStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlodLogStreamARN :: Lens.Lens' CloudWatchLoggingOptionDescription Lude.Text
cwlodLogStreamARN = Lens.lens (logStreamARN :: CloudWatchLoggingOptionDescription -> Lude.Text) (\s a -> s {logStreamARN = a} :: CloudWatchLoggingOptionDescription)
{-# DEPRECATED cwlodLogStreamARN "Use generic-lens or generic-optics with 'logStreamARN' instead." #-}

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwlodRoleARN :: Lens.Lens' CloudWatchLoggingOptionDescription Lude.Text
cwlodRoleARN = Lens.lens (roleARN :: CloudWatchLoggingOptionDescription -> Lude.Text) (\s a -> s {roleARN = a} :: CloudWatchLoggingOptionDescription)
{-# DEPRECATED cwlodRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CloudWatchLoggingOptionDescription where
  parseJSON =
    Lude.withObject
      "CloudWatchLoggingOptionDescription"
      ( \x ->
          CloudWatchLoggingOptionDescription'
            Lude.<$> (x Lude..:? "CloudWatchLoggingOptionId")
            Lude.<*> (x Lude..: "LogStreamARN")
            Lude.<*> (x Lude..: "RoleARN")
      )
