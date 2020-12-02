{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
--
--
-- /See:/ 'provisionedThroughputDescription' smart constructor.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription'
  { _ptdReadCapacityUnits ::
      !(Maybe Nat),
    _ptdLastDecreaseDateTime ::
      !(Maybe POSIX),
    _ptdWriteCapacityUnits ::
      !(Maybe Nat),
    _ptdNumberOfDecreasesToday ::
      !(Maybe Nat),
    _ptdLastIncreaseDateTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedThroughputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptdReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
--
-- * 'ptdLastDecreaseDateTime' - The date and time of the last provisioned throughput decrease for this table.
--
-- * 'ptdWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- * 'ptdNumberOfDecreasesToday' - The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'ptdLastIncreaseDateTime' - The date and time of the last provisioned throughput increase for this table.
provisionedThroughputDescription ::
  ProvisionedThroughputDescription
provisionedThroughputDescription =
  ProvisionedThroughputDescription'
    { _ptdReadCapacityUnits =
        Nothing,
      _ptdLastDecreaseDateTime = Nothing,
      _ptdWriteCapacityUnits = Nothing,
      _ptdNumberOfDecreasesToday = Nothing,
      _ptdLastIncreaseDateTime = Nothing
    }

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
ptdReadCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdReadCapacityUnits = lens _ptdReadCapacityUnits (\s a -> s {_ptdReadCapacityUnits = a}) . mapping _Nat

-- | The date and time of the last provisioned throughput decrease for this table.
ptdLastDecreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastDecreaseDateTime = lens _ptdLastDecreaseDateTime (\s a -> s {_ptdLastDecreaseDateTime = a}) . mapping _Time

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
ptdWriteCapacityUnits :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdWriteCapacityUnits = lens _ptdWriteCapacityUnits (\s a -> s {_ptdWriteCapacityUnits = a}) . mapping _Nat

-- | The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
ptdNumberOfDecreasesToday :: Lens' ProvisionedThroughputDescription (Maybe Natural)
ptdNumberOfDecreasesToday = lens _ptdNumberOfDecreasesToday (\s a -> s {_ptdNumberOfDecreasesToday = a}) . mapping _Nat

-- | The date and time of the last provisioned throughput increase for this table.
ptdLastIncreaseDateTime :: Lens' ProvisionedThroughputDescription (Maybe UTCTime)
ptdLastIncreaseDateTime = lens _ptdLastIncreaseDateTime (\s a -> s {_ptdLastIncreaseDateTime = a}) . mapping _Time

instance FromJSON ProvisionedThroughputDescription where
  parseJSON =
    withObject
      "ProvisionedThroughputDescription"
      ( \x ->
          ProvisionedThroughputDescription'
            <$> (x .:? "ReadCapacityUnits")
            <*> (x .:? "LastDecreaseDateTime")
            <*> (x .:? "WriteCapacityUnits")
            <*> (x .:? "NumberOfDecreasesToday")
            <*> (x .:? "LastIncreaseDateTime")
      )

instance Hashable ProvisionedThroughputDescription

instance NFData ProvisionedThroughputDescription
