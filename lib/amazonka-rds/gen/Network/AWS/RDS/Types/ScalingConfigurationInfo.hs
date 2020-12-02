{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ScalingConfigurationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ScalingConfigurationInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Shows the scaling configuration for an Aurora DB cluster in @serverless@ DB engine mode.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
--
-- /See:/ 'scalingConfigurationInfo' smart constructor.
data ScalingConfigurationInfo = ScalingConfigurationInfo'
  { _sciSecondsUntilAutoPause ::
      !(Maybe Int),
    _sciTimeoutAction :: !(Maybe Text),
    _sciAutoPause :: !(Maybe Bool),
    _sciMaxCapacity :: !(Maybe Int),
    _sciMinCapacity :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingConfigurationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sciSecondsUntilAutoPause' - The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
--
-- * 'sciTimeoutAction' - The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- * 'sciAutoPause' - A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode. When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
--
-- * 'sciMaxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- * 'sciMinCapacity' - The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
scalingConfigurationInfo ::
  ScalingConfigurationInfo
scalingConfigurationInfo =
  ScalingConfigurationInfo'
    { _sciSecondsUntilAutoPause = Nothing,
      _sciTimeoutAction = Nothing,
      _sciAutoPause = Nothing,
      _sciMaxCapacity = Nothing,
      _sciMinCapacity = Nothing
    }

-- | The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
sciSecondsUntilAutoPause :: Lens' ScalingConfigurationInfo (Maybe Int)
sciSecondsUntilAutoPause = lens _sciSecondsUntilAutoPause (\s a -> s {_sciSecondsUntilAutoPause = a})

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
sciTimeoutAction :: Lens' ScalingConfigurationInfo (Maybe Text)
sciTimeoutAction = lens _sciTimeoutAction (\s a -> s {_sciTimeoutAction = a})

-- | A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode. When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
sciAutoPause :: Lens' ScalingConfigurationInfo (Maybe Bool)
sciAutoPause = lens _sciAutoPause (\s a -> s {_sciAutoPause = a})

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
sciMaxCapacity :: Lens' ScalingConfigurationInfo (Maybe Int)
sciMaxCapacity = lens _sciMaxCapacity (\s a -> s {_sciMaxCapacity = a})

-- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
sciMinCapacity :: Lens' ScalingConfigurationInfo (Maybe Int)
sciMinCapacity = lens _sciMinCapacity (\s a -> s {_sciMinCapacity = a})

instance FromXML ScalingConfigurationInfo where
  parseXML x =
    ScalingConfigurationInfo'
      <$> (x .@? "SecondsUntilAutoPause")
      <*> (x .@? "TimeoutAction")
      <*> (x .@? "AutoPause")
      <*> (x .@? "MaxCapacity")
      <*> (x .@? "MinCapacity")

instance Hashable ScalingConfigurationInfo

instance NFData ScalingConfigurationInfo
