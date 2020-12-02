{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
--
-- /See:/ 'provisionedThroughput' smart constructor.
data ProvisionedThroughput = ProvisionedThroughput'
  { _ptReadCapacityUnits ::
      !Nat,
    _ptWriteCapacityUnits :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedThroughput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- * 'ptWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
provisionedThroughput ::
  -- | 'ptReadCapacityUnits'
  Natural ->
  -- | 'ptWriteCapacityUnits'
  Natural ->
  ProvisionedThroughput
provisionedThroughput pReadCapacityUnits_ pWriteCapacityUnits_ =
  ProvisionedThroughput'
    { _ptReadCapacityUnits =
        _Nat # pReadCapacityUnits_,
      _ptWriteCapacityUnits = _Nat # pWriteCapacityUnits_
    }

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
ptReadCapacityUnits :: Lens' ProvisionedThroughput Natural
ptReadCapacityUnits = lens _ptReadCapacityUnits (\s a -> s {_ptReadCapacityUnits = a}) . _Nat

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ . If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
ptWriteCapacityUnits :: Lens' ProvisionedThroughput Natural
ptWriteCapacityUnits = lens _ptWriteCapacityUnits (\s a -> s {_ptWriteCapacityUnits = a}) . _Nat

instance FromJSON ProvisionedThroughput where
  parseJSON =
    withObject
      "ProvisionedThroughput"
      ( \x ->
          ProvisionedThroughput'
            <$> (x .: "ReadCapacityUnits") <*> (x .: "WriteCapacityUnits")
      )

instance Hashable ProvisionedThroughput

instance NFData ProvisionedThroughput

instance ToJSON ProvisionedThroughput where
  toJSON ProvisionedThroughput' {..} =
    object
      ( catMaybes
          [ Just ("ReadCapacityUnits" .= _ptReadCapacityUnits),
            Just ("WriteCapacityUnits" .= _ptWriteCapacityUnits)
          ]
      )
