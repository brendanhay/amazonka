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
    claLogGroupName,
    claRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action that sends data to CloudWatch Logs.
--
-- /See:/ 'mkCloudwatchLogsAction' smart constructor.
data CloudwatchLogsAction = CloudwatchLogsAction'
  { -- | The CloudWatch log group to which the action sends data.
    logGroupName :: Lude.Text,
    -- | The IAM role that allows access to the CloudWatch log.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudwatchLogsAction' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The CloudWatch log group to which the action sends data.
-- * 'roleARN' - The IAM role that allows access to the CloudWatch log.
mkCloudwatchLogsAction ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CloudwatchLogsAction
mkCloudwatchLogsAction pLogGroupName_ pRoleARN_ =
  CloudwatchLogsAction'
    { logGroupName = pLogGroupName_,
      roleARN = pRoleARN_
    }

-- | The CloudWatch log group to which the action sends data.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLogGroupName :: Lens.Lens' CloudwatchLogsAction Lude.Text
claLogGroupName = Lens.lens (logGroupName :: CloudwatchLogsAction -> Lude.Text) (\s a -> s {logGroupName = a} :: CloudwatchLogsAction)
{-# DEPRECATED claLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The IAM role that allows access to the CloudWatch log.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claRoleARN :: Lens.Lens' CloudwatchLogsAction Lude.Text
claRoleARN = Lens.lens (roleARN :: CloudwatchLogsAction -> Lude.Text) (\s a -> s {roleARN = a} :: CloudwatchLogsAction)
{-# DEPRECATED claRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CloudwatchLogsAction where
  parseJSON =
    Lude.withObject
      "CloudwatchLogsAction"
      ( \x ->
          CloudwatchLogsAction'
            Lude.<$> (x Lude..: "logGroupName") Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON CloudwatchLogsAction where
  toJSON CloudwatchLogsAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
