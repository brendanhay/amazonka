{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
  ( CloudWatchLogsLogGroup (..),

    -- * Smart constructor
    mkCloudWatchLogsLogGroup,

    -- * Lenses
    cwllgLogGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkCloudWatchLogsLogGroup' smart constructor.
newtype CloudWatchLogsLogGroup = CloudWatchLogsLogGroup'
  { logGroupARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLogsLogGroup' with the minimum fields required to make a request.
--
-- * 'logGroupARN' - The ARN of the the CloudWatch log group to which you want your logs emitted to. The ARN must end with @:*@
mkCloudWatchLogsLogGroup ::
  CloudWatchLogsLogGroup
mkCloudWatchLogsLogGroup =
  CloudWatchLogsLogGroup' {logGroupARN = Lude.Nothing}

-- | The ARN of the the CloudWatch log group to which you want your logs emitted to. The ARN must end with @:*@
--
-- /Note:/ Consider using 'logGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllgLogGroupARN :: Lens.Lens' CloudWatchLogsLogGroup (Lude.Maybe Lude.Text)
cwllgLogGroupARN = Lens.lens (logGroupARN :: CloudWatchLogsLogGroup -> Lude.Maybe Lude.Text) (\s a -> s {logGroupARN = a} :: CloudWatchLogsLogGroup)
{-# DEPRECATED cwllgLogGroupARN "Use generic-lens or generic-optics with 'logGroupARN' instead." #-}

instance Lude.FromJSON CloudWatchLogsLogGroup where
  parseJSON =
    Lude.withObject
      "CloudWatchLogsLogGroup"
      ( \x ->
          CloudWatchLogsLogGroup' Lude.<$> (x Lude..:? "logGroupArn")
      )

instance Lude.ToJSON CloudWatchLogsLogGroup where
  toJSON CloudWatchLogsLogGroup' {..} =
    Lude.object
      (Lude.catMaybes [("logGroupArn" Lude..=) Lude.<$> logGroupARN])
