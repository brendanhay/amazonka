{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchLogsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchLogsAction
  ( CloudwatchLogsAction (..),

    -- * Smart constructor
    mkCloudwatchLogsAction,

    -- * Lenses
    claRoleARN,
    claLogGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action that sends data to CloudWatch Logs.
--
-- /See:/ 'mkCloudwatchLogsAction' smart constructor.
data CloudwatchLogsAction = CloudwatchLogsAction'
  { roleARN ::
      Lude.Text,
    logGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudwatchLogsAction' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The CloudWatch log group to which the action sends data.
-- * 'roleARN' - The IAM role that allows access to the CloudWatch log.
mkCloudwatchLogsAction ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'logGroupName'
  Lude.Text ->
  CloudwatchLogsAction
mkCloudwatchLogsAction pRoleARN_ pLogGroupName_ =
  CloudwatchLogsAction'
    { roleARN = pRoleARN_,
      logGroupName = pLogGroupName_
    }

-- | The IAM role that allows access to the CloudWatch log.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claRoleARN :: Lens.Lens' CloudwatchLogsAction Lude.Text
claRoleARN = Lens.lens (roleARN :: CloudwatchLogsAction -> Lude.Text) (\s a -> s {roleARN = a} :: CloudwatchLogsAction)
{-# DEPRECATED claRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The CloudWatch log group to which the action sends data.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLogGroupName :: Lens.Lens' CloudwatchLogsAction Lude.Text
claLogGroupName = Lens.lens (logGroupName :: CloudwatchLogsAction -> Lude.Text) (\s a -> s {logGroupName = a} :: CloudwatchLogsAction)
{-# DEPRECATED claLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.FromJSON CloudwatchLogsAction where
  parseJSON =
    Lude.withObject
      "CloudwatchLogsAction"
      ( \x ->
          CloudwatchLogsAction'
            Lude.<$> (x Lude..: "roleArn") Lude.<*> (x Lude..: "logGroupName")
      )

instance Lude.ToJSON CloudwatchLogsAction where
  toJSON CloudwatchLogsAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("logGroupName" Lude..= logGroupName)
          ]
      )
