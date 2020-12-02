{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ChildShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ChildShard where

import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'childShard' smart constructor.
data ChildShard = ChildShard'
  { _csShardId :: !Text,
    _csParentShards :: ![Text],
    _csHashKeyRange :: !HashKeyRange
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChildShard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csShardId' - Undocumented member.
--
-- * 'csParentShards' - Undocumented member.
--
-- * 'csHashKeyRange' - Undocumented member.
childShard ::
  -- | 'csShardId'
  Text ->
  -- | 'csHashKeyRange'
  HashKeyRange ->
  ChildShard
childShard pShardId_ pHashKeyRange_ =
  ChildShard'
    { _csShardId = pShardId_,
      _csParentShards = mempty,
      _csHashKeyRange = pHashKeyRange_
    }

-- | Undocumented member.
csShardId :: Lens' ChildShard Text
csShardId = lens _csShardId (\s a -> s {_csShardId = a})

-- | Undocumented member.
csParentShards :: Lens' ChildShard [Text]
csParentShards = lens _csParentShards (\s a -> s {_csParentShards = a}) . _Coerce

-- | Undocumented member.
csHashKeyRange :: Lens' ChildShard HashKeyRange
csHashKeyRange = lens _csHashKeyRange (\s a -> s {_csHashKeyRange = a})

instance FromJSON ChildShard where
  parseJSON =
    withObject
      "ChildShard"
      ( \x ->
          ChildShard'
            <$> (x .: "ShardId")
            <*> (x .:? "ParentShards" .!= mempty)
            <*> (x .: "HashKeyRange")
      )

instance Hashable ChildShard

instance NFData ChildShard
