{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Shard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Shard where

import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.SequenceNumberRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A uniquely identified group of data records in a Kinesis data stream.
--
--
--
-- /See:/ 'shard' smart constructor.
data Shard = Shard'
  { _sAdjacentParentShardId :: !(Maybe Text),
    _sParentShardId :: !(Maybe Text),
    _sShardId :: !Text,
    _sHashKeyRange :: !HashKeyRange,
    _sSequenceNumberRange :: !SequenceNumberRange
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Shard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAdjacentParentShardId' - The shard ID of the shard adjacent to the shard's parent.
--
-- * 'sParentShardId' - The shard ID of the shard's parent.
--
-- * 'sShardId' - The unique identifier of the shard within the stream.
--
-- * 'sHashKeyRange' - The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
-- * 'sSequenceNumberRange' - The range of possible sequence numbers for the shard.
shard ::
  -- | 'sShardId'
  Text ->
  -- | 'sHashKeyRange'
  HashKeyRange ->
  -- | 'sSequenceNumberRange'
  SequenceNumberRange ->
  Shard
shard pShardId_ pHashKeyRange_ pSequenceNumberRange_ =
  Shard'
    { _sAdjacentParentShardId = Nothing,
      _sParentShardId = Nothing,
      _sShardId = pShardId_,
      _sHashKeyRange = pHashKeyRange_,
      _sSequenceNumberRange = pSequenceNumberRange_
    }

-- | The shard ID of the shard adjacent to the shard's parent.
sAdjacentParentShardId :: Lens' Shard (Maybe Text)
sAdjacentParentShardId = lens _sAdjacentParentShardId (\s a -> s {_sAdjacentParentShardId = a})

-- | The shard ID of the shard's parent.
sParentShardId :: Lens' Shard (Maybe Text)
sParentShardId = lens _sParentShardId (\s a -> s {_sParentShardId = a})

-- | The unique identifier of the shard within the stream.
sShardId :: Lens' Shard Text
sShardId = lens _sShardId (\s a -> s {_sShardId = a})

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
sHashKeyRange :: Lens' Shard HashKeyRange
sHashKeyRange = lens _sHashKeyRange (\s a -> s {_sHashKeyRange = a})

-- | The range of possible sequence numbers for the shard.
sSequenceNumberRange :: Lens' Shard SequenceNumberRange
sSequenceNumberRange = lens _sSequenceNumberRange (\s a -> s {_sSequenceNumberRange = a})

instance FromJSON Shard where
  parseJSON =
    withObject
      "Shard"
      ( \x ->
          Shard'
            <$> (x .:? "AdjacentParentShardId")
            <*> (x .:? "ParentShardId")
            <*> (x .: "ShardId")
            <*> (x .: "HashKeyRange")
            <*> (x .: "SequenceNumberRange")
      )

instance Hashable Shard

instance NFData Shard
