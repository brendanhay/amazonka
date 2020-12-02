{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ScalingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the scaling configuration of an Aurora Serverless DB cluster.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
--
-- /See:/ 'scalingConfiguration' smart constructor.
data ScalingConfiguration = ScalingConfiguration'
  { _scSecondsUntilAutoPause ::
      !(Maybe Int),
    _scTimeoutAction :: !(Maybe Text),
    _scAutoPause :: !(Maybe Bool),
    _scMaxCapacity :: !(Maybe Int),
    _scMinCapacity :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scSecondsUntilAutoPause' - The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
--
-- * 'scTimeoutAction' - The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ . @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible. @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period. /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- * 'scAutoPause' - A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
--
-- * 'scMaxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode. For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ . For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ . The maximum capacity must be greater than or equal to the minimum capacity.
--
-- * 'scMinCapacity' - The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode. For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ . For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ . The minimum capacity must be less than or equal to the maximum capacity.
scalingConfiguration ::
  ScalingConfiguration
scalingConfiguration =
  ScalingConfiguration'
    { _scSecondsUntilAutoPause = Nothing,
      _scTimeoutAction = Nothing,
      _scAutoPause = Nothing,
      _scMaxCapacity = Nothing,
      _scMinCapacity = Nothing
    }

-- | The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
scSecondsUntilAutoPause :: Lens' ScalingConfiguration (Maybe Int)
scSecondsUntilAutoPause = lens _scSecondsUntilAutoPause (\s a -> s {_scSecondsUntilAutoPause = a})

-- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ . @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible. @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period. /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
scTimeoutAction :: Lens' ScalingConfiguration (Maybe Text)
scTimeoutAction = lens _scTimeoutAction (\s a -> s {_scTimeoutAction = a})

-- | A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
scAutoPause :: Lens' ScalingConfiguration (Maybe Bool)
scAutoPause = lens _scAutoPause (\s a -> s {_scAutoPause = a})

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode. For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ . For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ . The maximum capacity must be greater than or equal to the minimum capacity.
scMaxCapacity :: Lens' ScalingConfiguration (Maybe Int)
scMaxCapacity = lens _scMaxCapacity (\s a -> s {_scMaxCapacity = a})

-- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode. For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ . For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ . The minimum capacity must be less than or equal to the maximum capacity.
scMinCapacity :: Lens' ScalingConfiguration (Maybe Int)
scMinCapacity = lens _scMinCapacity (\s a -> s {_scMinCapacity = a})

instance Hashable ScalingConfiguration

instance NFData ScalingConfiguration

instance ToQuery ScalingConfiguration where
  toQuery ScalingConfiguration' {..} =
    mconcat
      [ "SecondsUntilAutoPause" =: _scSecondsUntilAutoPause,
        "TimeoutAction" =: _scTimeoutAction,
        "AutoPause" =: _scAutoPause,
        "MaxCapacity" =: _scMaxCapacity,
        "MinCapacity" =: _scMinCapacity
      ]
