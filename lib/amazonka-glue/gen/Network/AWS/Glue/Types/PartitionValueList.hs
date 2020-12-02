{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionValueList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionValueList where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a list of values defining partitions.
--
--
--
-- /See:/ 'partitionValueList' smart constructor.
newtype PartitionValueList = PartitionValueList'
  { _pvlValues ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartitionValueList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvlValues' - The list of values.
partitionValueList ::
  PartitionValueList
partitionValueList = PartitionValueList' {_pvlValues = mempty}

-- | The list of values.
pvlValues :: Lens' PartitionValueList [Text]
pvlValues = lens _pvlValues (\s a -> s {_pvlValues = a}) . _Coerce

instance FromJSON PartitionValueList where
  parseJSON =
    withObject
      "PartitionValueList"
      (\x -> PartitionValueList' <$> (x .:? "Values" .!= mempty))

instance Hashable PartitionValueList

instance NFData PartitionValueList

instance ToJSON PartitionValueList where
  toJSON PartitionValueList' {..} =
    object (catMaybes [Just ("Values" .= _pvlValues)])
