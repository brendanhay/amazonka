{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogDestination
  ( LogDestination (..),

    -- * Smart constructor
    mkLogDestination,

    -- * Lenses
    ldCloudWatchLogsLogGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup

-- |
--
-- /See:/ 'mkLogDestination' smart constructor.
newtype LogDestination = LogDestination'
  { -- | An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
    cloudWatchLogsLogGroup :: Lude.Maybe CloudWatchLogsLogGroup
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogDestination' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogsLogGroup' - An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
mkLogDestination ::
  LogDestination
mkLogDestination =
  LogDestination' {cloudWatchLogsLogGroup = Lude.Nothing}

-- | An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldCloudWatchLogsLogGroup :: Lens.Lens' LogDestination (Lude.Maybe CloudWatchLogsLogGroup)
ldCloudWatchLogsLogGroup = Lens.lens (cloudWatchLogsLogGroup :: LogDestination -> Lude.Maybe CloudWatchLogsLogGroup) (\s a -> s {cloudWatchLogsLogGroup = a} :: LogDestination)
{-# DEPRECATED ldCloudWatchLogsLogGroup "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroup' instead." #-}

instance Lude.FromJSON LogDestination where
  parseJSON =
    Lude.withObject
      "LogDestination"
      ( \x ->
          LogDestination' Lude.<$> (x Lude..:? "cloudWatchLogsLogGroup")
      )

instance Lude.ToJSON LogDestination where
  toJSON LogDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cloudWatchLogsLogGroup" Lude..=)
              Lude.<$> cloudWatchLogsLogGroup
          ]
      )
