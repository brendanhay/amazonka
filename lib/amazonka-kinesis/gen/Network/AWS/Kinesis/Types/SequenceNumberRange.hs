{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SequenceNumberRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The range of possible sequence numbers for the shard.
--
--
--
-- /See:/ 'sequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { _snrEndingSequenceNumber ::
      !(Maybe Text),
    _snrStartingSequenceNumber :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SequenceNumberRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snrEndingSequenceNumber' - The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
--
-- * 'snrStartingSequenceNumber' - The starting sequence number for the range.
sequenceNumberRange ::
  -- | 'snrStartingSequenceNumber'
  Text ->
  SequenceNumberRange
sequenceNumberRange pStartingSequenceNumber_ =
  SequenceNumberRange'
    { _snrEndingSequenceNumber = Nothing,
      _snrStartingSequenceNumber = pStartingSequenceNumber_
    }

-- | The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber = lens _snrEndingSequenceNumber (\s a -> s {_snrEndingSequenceNumber = a})

-- | The starting sequence number for the range.
snrStartingSequenceNumber :: Lens' SequenceNumberRange Text
snrStartingSequenceNumber = lens _snrStartingSequenceNumber (\s a -> s {_snrStartingSequenceNumber = a})

instance FromJSON SequenceNumberRange where
  parseJSON =
    withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            <$> (x .:? "EndingSequenceNumber") <*> (x .: "StartingSequenceNumber")
      )

instance Hashable SequenceNumberRange

instance NFData SequenceNumberRange
