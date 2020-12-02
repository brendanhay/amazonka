{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.SequenceNumberRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The beginning and ending sequence numbers for the stream records contained within a shard.
--
--
--
-- /See:/ 'sequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { _snrStartingSequenceNumber ::
      !(Maybe Text),
    _snrEndingSequenceNumber :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SequenceNumberRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snrStartingSequenceNumber' - The first sequence number for the stream records contained within a shard. String contains numeric characters only.
--
-- * 'snrEndingSequenceNumber' - The last sequence number for the stream records contained within a shard. String contains numeric characters only.
sequenceNumberRange ::
  SequenceNumberRange
sequenceNumberRange =
  SequenceNumberRange'
    { _snrStartingSequenceNumber = Nothing,
      _snrEndingSequenceNumber = Nothing
    }

-- | The first sequence number for the stream records contained within a shard. String contains numeric characters only.
snrStartingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrStartingSequenceNumber = lens _snrStartingSequenceNumber (\s a -> s {_snrStartingSequenceNumber = a})

-- | The last sequence number for the stream records contained within a shard. String contains numeric characters only.
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber = lens _snrEndingSequenceNumber (\s a -> s {_snrEndingSequenceNumber = a})

instance FromJSON SequenceNumberRange where
  parseJSON =
    withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            <$> (x .:? "StartingSequenceNumber")
            <*> (x .:? "EndingSequenceNumber")
      )

instance Hashable SequenceNumberRange

instance NFData SequenceNumberRange
