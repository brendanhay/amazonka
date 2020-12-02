{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.HashKeyRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.HashKeyRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
--
--
-- /See:/ 'hashKeyRange' smart constructor.
data HashKeyRange = HashKeyRange'
  { _hkrStartingHashKey :: !Text,
    _hkrEndingHashKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HashKeyRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hkrStartingHashKey' - The starting hash key of the hash key range.
--
-- * 'hkrEndingHashKey' - The ending hash key of the hash key range.
hashKeyRange ::
  -- | 'hkrStartingHashKey'
  Text ->
  -- | 'hkrEndingHashKey'
  Text ->
  HashKeyRange
hashKeyRange pStartingHashKey_ pEndingHashKey_ =
  HashKeyRange'
    { _hkrStartingHashKey = pStartingHashKey_,
      _hkrEndingHashKey = pEndingHashKey_
    }

-- | The starting hash key of the hash key range.
hkrStartingHashKey :: Lens' HashKeyRange Text
hkrStartingHashKey = lens _hkrStartingHashKey (\s a -> s {_hkrStartingHashKey = a})

-- | The ending hash key of the hash key range.
hkrEndingHashKey :: Lens' HashKeyRange Text
hkrEndingHashKey = lens _hkrEndingHashKey (\s a -> s {_hkrEndingHashKey = a})

instance FromJSON HashKeyRange where
  parseJSON =
    withObject
      "HashKeyRange"
      ( \x ->
          HashKeyRange'
            <$> (x .: "StartingHashKey") <*> (x .: "EndingHashKey")
      )

instance Hashable HashKeyRange

instance NFData HashKeyRange
