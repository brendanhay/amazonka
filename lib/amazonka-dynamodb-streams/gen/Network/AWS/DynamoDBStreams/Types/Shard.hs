{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Shard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Shard where

import Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A uniquely identified group of stream records within a stream.
--
--
--
-- /See:/ 'shard' smart constructor.
data Shard = Shard'
  { _sParentShardId :: !(Maybe Text),
    _sSequenceNumberRange :: !(Maybe SequenceNumberRange),
    _sShardId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Shard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sParentShardId' - The shard ID of the current shard's parent.
--
-- * 'sSequenceNumberRange' - The range of possible sequence numbers for the shard.
--
-- * 'sShardId' - The system-generated identifier for this shard.
shard ::
  Shard
shard =
  Shard'
    { _sParentShardId = Nothing,
      _sSequenceNumberRange = Nothing,
      _sShardId = Nothing
    }

-- | The shard ID of the current shard's parent.
sParentShardId :: Lens' Shard (Maybe Text)
sParentShardId = lens _sParentShardId (\s a -> s {_sParentShardId = a})

-- | The range of possible sequence numbers for the shard.
sSequenceNumberRange :: Lens' Shard (Maybe SequenceNumberRange)
sSequenceNumberRange = lens _sSequenceNumberRange (\s a -> s {_sSequenceNumberRange = a})

-- | The system-generated identifier for this shard.
sShardId :: Lens' Shard (Maybe Text)
sShardId = lens _sShardId (\s a -> s {_sShardId = a})

instance FromJSON Shard where
  parseJSON =
    withObject
      "Shard"
      ( \x ->
          Shard'
            <$> (x .:? "ParentShardId")
            <*> (x .:? "SequenceNumberRange")
            <*> (x .:? "ShardId")
      )

instance Hashable Shard

instance NFData Shard
