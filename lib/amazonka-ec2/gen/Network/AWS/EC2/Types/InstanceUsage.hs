{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceUsage where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the Capacity Reservation usage.
--
--
--
-- /See:/ 'instanceUsage' smart constructor.
data InstanceUsage = InstanceUsage'
  { _iuAccountId :: !(Maybe Text),
    _iuUsedInstanceCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iuAccountId' - The ID of the AWS account that is making use of the Capacity Reservation.
--
-- * 'iuUsedInstanceCount' - The number of instances the AWS account currently has in the Capacity Reservation.
instanceUsage ::
  InstanceUsage
instanceUsage =
  InstanceUsage'
    { _iuAccountId = Nothing,
      _iuUsedInstanceCount = Nothing
    }

-- | The ID of the AWS account that is making use of the Capacity Reservation.
iuAccountId :: Lens' InstanceUsage (Maybe Text)
iuAccountId = lens _iuAccountId (\s a -> s {_iuAccountId = a})

-- | The number of instances the AWS account currently has in the Capacity Reservation.
iuUsedInstanceCount :: Lens' InstanceUsage (Maybe Int)
iuUsedInstanceCount = lens _iuUsedInstanceCount (\s a -> s {_iuUsedInstanceCount = a})

instance FromXML InstanceUsage where
  parseXML x =
    InstanceUsage'
      <$> (x .@? "accountId") <*> (x .@? "usedInstanceCount")

instance Hashable InstanceUsage

instance NFData InstanceUsage
