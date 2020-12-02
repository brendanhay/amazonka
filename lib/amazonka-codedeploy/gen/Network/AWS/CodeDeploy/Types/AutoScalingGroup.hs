{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoScalingGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an Auto Scaling group.
--
--
--
-- /See:/ 'autoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { _asgHook ::
      !(Maybe Text),
    _asgName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgHook' - An Auto Scaling lifecycle event hook name.
--
-- * 'asgName' - The Auto Scaling group name.
autoScalingGroup ::
  AutoScalingGroup
autoScalingGroup =
  AutoScalingGroup' {_asgHook = Nothing, _asgName = Nothing}

-- | An Auto Scaling lifecycle event hook name.
asgHook :: Lens' AutoScalingGroup (Maybe Text)
asgHook = lens _asgHook (\s a -> s {_asgHook = a})

-- | The Auto Scaling group name.
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\s a -> s {_asgName = a})

instance FromJSON AutoScalingGroup where
  parseJSON =
    withObject
      "AutoScalingGroup"
      (\x -> AutoScalingGroup' <$> (x .:? "hook") <*> (x .:? "name"))

instance Hashable AutoScalingGroup

instance NFData AutoScalingGroup
