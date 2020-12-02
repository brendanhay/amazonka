{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateChange where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance state change.
--
--
--
-- /See:/ 'instanceStateChange' smart constructor.
data InstanceStateChange = InstanceStateChange'
  { _iscInstanceId ::
      !(Maybe Text),
    _iscCurrentState :: !(Maybe InstanceState),
    _iscPreviousState :: !(Maybe InstanceState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscInstanceId' - The ID of the instance.
--
-- * 'iscCurrentState' - The current state of the instance.
--
-- * 'iscPreviousState' - The previous state of the instance.
instanceStateChange ::
  InstanceStateChange
instanceStateChange =
  InstanceStateChange'
    { _iscInstanceId = Nothing,
      _iscCurrentState = Nothing,
      _iscPreviousState = Nothing
    }

-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\s a -> s {_iscInstanceId = a})

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\s a -> s {_iscCurrentState = a})

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState = lens _iscPreviousState (\s a -> s {_iscPreviousState = a})

instance FromXML InstanceStateChange where
  parseXML x =
    InstanceStateChange'
      <$> (x .@? "instanceId")
      <*> (x .@? "currentState")
      <*> (x .@? "previousState")

instance Hashable InstanceStateChange

instance NFData InstanceStateChange
