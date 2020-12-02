{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Replica-specific provisioned throughput settings. If not specified, uses the source table's provisioned throughput settings.
--
--
--
-- /See:/ 'provisionedThroughputOverride' smart constructor.
newtype ProvisionedThroughputOverride = ProvisionedThroughputOverride'
  { _ptoReadCapacityUnits ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedThroughputOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptoReadCapacityUnits' - Replica-specific read capacity units. If not specified, uses the source table's read capacity settings.
provisionedThroughputOverride ::
  ProvisionedThroughputOverride
provisionedThroughputOverride =
  ProvisionedThroughputOverride' {_ptoReadCapacityUnits = Nothing}

-- | Replica-specific read capacity units. If not specified, uses the source table's read capacity settings.
ptoReadCapacityUnits :: Lens' ProvisionedThroughputOverride (Maybe Natural)
ptoReadCapacityUnits = lens _ptoReadCapacityUnits (\s a -> s {_ptoReadCapacityUnits = a}) . mapping _Nat

instance FromJSON ProvisionedThroughputOverride where
  parseJSON =
    withObject
      "ProvisionedThroughputOverride"
      ( \x ->
          ProvisionedThroughputOverride' <$> (x .:? "ReadCapacityUnits")
      )

instance Hashable ProvisionedThroughputOverride

instance NFData ProvisionedThroughputOverride

instance ToJSON ProvisionedThroughputOverride where
  toJSON ProvisionedThroughputOverride' {..} =
    object
      (catMaybes [("ReadCapacityUnits" .=) <$> _ptoReadCapacityUnits])
