{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusDetails where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.StatusName
import Network.AWS.EC2.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the instance status.
--
--
--
-- /See:/ 'instanceStatusDetails' smart constructor.
data InstanceStatusDetails = InstanceStatusDetails'
  { _isdStatus ::
      !(Maybe StatusType),
    _isdImpairedSince :: !(Maybe ISO8601),
    _isdName :: !(Maybe StatusName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isdStatus' - The status.
--
-- * 'isdImpairedSince' - The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
--
-- * 'isdName' - The type of instance status.
instanceStatusDetails ::
  InstanceStatusDetails
instanceStatusDetails =
  InstanceStatusDetails'
    { _isdStatus = Nothing,
      _isdImpairedSince = Nothing,
      _isdName = Nothing
    }

-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus = lens _isdStatus (\s a -> s {_isdStatus = a})

-- | The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe UTCTime)
isdImpairedSince = lens _isdImpairedSince (\s a -> s {_isdImpairedSince = a}) . mapping _Time

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe StatusName)
isdName = lens _isdName (\s a -> s {_isdName = a})

instance FromXML InstanceStatusDetails where
  parseXML x =
    InstanceStatusDetails'
      <$> (x .@? "status") <*> (x .@? "impairedSince") <*> (x .@? "name")

instance Hashable InstanceStatusDetails

instance NFData InstanceStatusDetails
