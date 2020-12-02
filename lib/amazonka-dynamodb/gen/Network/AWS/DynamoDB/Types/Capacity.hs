{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Capacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Capacity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the amount of provisioned throughput capacity consumed on a table or an index.
--
--
--
-- /See:/ 'capacity' smart constructor.
data Capacity = Capacity'
  { _capReadCapacityUnits :: !(Maybe Double),
    _capCapacityUnits :: !(Maybe Double),
    _capWriteCapacityUnits :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Capacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capReadCapacityUnits' - The total number of read capacity units consumed on a table or an index.
--
-- * 'capCapacityUnits' - The total number of capacity units consumed on a table or an index.
--
-- * 'capWriteCapacityUnits' - The total number of write capacity units consumed on a table or an index.
capacity ::
  Capacity
capacity =
  Capacity'
    { _capReadCapacityUnits = Nothing,
      _capCapacityUnits = Nothing,
      _capWriteCapacityUnits = Nothing
    }

-- | The total number of read capacity units consumed on a table or an index.
capReadCapacityUnits :: Lens' Capacity (Maybe Double)
capReadCapacityUnits = lens _capReadCapacityUnits (\s a -> s {_capReadCapacityUnits = a})

-- | The total number of capacity units consumed on a table or an index.
capCapacityUnits :: Lens' Capacity (Maybe Double)
capCapacityUnits = lens _capCapacityUnits (\s a -> s {_capCapacityUnits = a})

-- | The total number of write capacity units consumed on a table or an index.
capWriteCapacityUnits :: Lens' Capacity (Maybe Double)
capWriteCapacityUnits = lens _capWriteCapacityUnits (\s a -> s {_capWriteCapacityUnits = a})

instance FromJSON Capacity where
  parseJSON =
    withObject
      "Capacity"
      ( \x ->
          Capacity'
            <$> (x .:? "ReadCapacityUnits")
            <*> (x .:? "CapacityUnits")
            <*> (x .:? "WriteCapacityUnits")
      )

instance Hashable Capacity

instance NFData Capacity
