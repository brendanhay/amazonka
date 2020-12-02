{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RunCommandParameters where

import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command.
--
--
--
-- /See:/ 'runCommandParameters' smart constructor.
newtype RunCommandParameters = RunCommandParameters'
  { _rcpRunCommandTargets ::
      List1 RunCommandTarget
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RunCommandParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcpRunCommandTargets' - Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
runCommandParameters ::
  -- | 'rcpRunCommandTargets'
  NonEmpty RunCommandTarget ->
  RunCommandParameters
runCommandParameters pRunCommandTargets_ =
  RunCommandParameters'
    { _rcpRunCommandTargets =
        _List1 # pRunCommandTargets_
    }

-- | Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
rcpRunCommandTargets :: Lens' RunCommandParameters (NonEmpty RunCommandTarget)
rcpRunCommandTargets = lens _rcpRunCommandTargets (\s a -> s {_rcpRunCommandTargets = a}) . _List1

instance FromJSON RunCommandParameters where
  parseJSON =
    withObject
      "RunCommandParameters"
      (\x -> RunCommandParameters' <$> (x .: "RunCommandTargets"))

instance Hashable RunCommandParameters

instance NFData RunCommandParameters

instance ToJSON RunCommandParameters where
  toJSON RunCommandParameters' {..} =
    object
      (catMaybes [Just ("RunCommandTargets" .= _rcpRunCommandTargets)])
