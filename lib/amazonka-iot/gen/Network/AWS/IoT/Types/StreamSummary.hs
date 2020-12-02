{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of a stream.
--
--
--
-- /See:/ 'streamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { _ssStreamVersion ::
      !(Maybe Nat),
    _ssStreamARN :: !(Maybe Text),
    _ssDescription :: !(Maybe Text),
    _ssStreamId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStreamVersion' - The stream version.
--
-- * 'ssStreamARN' - The stream ARN.
--
-- * 'ssDescription' - A description of the stream.
--
-- * 'ssStreamId' - The stream ID.
streamSummary ::
  StreamSummary
streamSummary =
  StreamSummary'
    { _ssStreamVersion = Nothing,
      _ssStreamARN = Nothing,
      _ssDescription = Nothing,
      _ssStreamId = Nothing
    }

-- | The stream version.
ssStreamVersion :: Lens' StreamSummary (Maybe Natural)
ssStreamVersion = lens _ssStreamVersion (\s a -> s {_ssStreamVersion = a}) . mapping _Nat

-- | The stream ARN.
ssStreamARN :: Lens' StreamSummary (Maybe Text)
ssStreamARN = lens _ssStreamARN (\s a -> s {_ssStreamARN = a})

-- | A description of the stream.
ssDescription :: Lens' StreamSummary (Maybe Text)
ssDescription = lens _ssDescription (\s a -> s {_ssDescription = a})

-- | The stream ID.
ssStreamId :: Lens' StreamSummary (Maybe Text)
ssStreamId = lens _ssStreamId (\s a -> s {_ssStreamId = a})

instance FromJSON StreamSummary where
  parseJSON =
    withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            <$> (x .:? "streamVersion")
            <*> (x .:? "streamArn")
            <*> (x .:? "description")
            <*> (x .:? "streamId")
      )

instance Hashable StreamSummary

instance NFData StreamSummary
