{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesPlacement where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the placement for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesPlacement' smart constructor.
data ScheduledInstancesPlacement = ScheduledInstancesPlacement'
  { _sipAvailabilityZone ::
      !(Maybe Text),
    _sipGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipAvailabilityZone' - The Availability Zone.
--
-- * 'sipGroupName' - The name of the placement group.
scheduledInstancesPlacement ::
  ScheduledInstancesPlacement
scheduledInstancesPlacement =
  ScheduledInstancesPlacement'
    { _sipAvailabilityZone = Nothing,
      _sipGroupName = Nothing
    }

-- | The Availability Zone.
sipAvailabilityZone :: Lens' ScheduledInstancesPlacement (Maybe Text)
sipAvailabilityZone = lens _sipAvailabilityZone (\s a -> s {_sipAvailabilityZone = a})

-- | The name of the placement group.
sipGroupName :: Lens' ScheduledInstancesPlacement (Maybe Text)
sipGroupName = lens _sipGroupName (\s a -> s {_sipGroupName = a})

instance Hashable ScheduledInstancesPlacement

instance NFData ScheduledInstancesPlacement

instance ToQuery ScheduledInstancesPlacement where
  toQuery ScheduledInstancesPlacement' {..} =
    mconcat
      [ "AvailabilityZone" =: _sipAvailabilityZone,
        "GroupName" =: _sipGroupName
      ]
