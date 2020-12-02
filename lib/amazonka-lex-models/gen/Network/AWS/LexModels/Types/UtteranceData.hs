{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.UtteranceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.UtteranceData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a single utterance that was made to your bot.
--
--
--
-- /See:/ 'utteranceData' smart constructor.
data UtteranceData = UtteranceData'
  { _udFirstUtteredDate ::
      !(Maybe POSIX),
    _udCount :: !(Maybe Int),
    _udUtteranceString :: !(Maybe Text),
    _udLastUtteredDate :: !(Maybe POSIX),
    _udDistinctUsers :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UtteranceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udFirstUtteredDate' - The date that the utterance was first recorded.
--
-- * 'udCount' - The number of times that the utterance was processed.
--
-- * 'udUtteranceString' - The text that was entered by the user or the text representation of an audio clip.
--
-- * 'udLastUtteredDate' - The date that the utterance was last recorded.
--
-- * 'udDistinctUsers' - The total number of individuals that used the utterance.
utteranceData ::
  UtteranceData
utteranceData =
  UtteranceData'
    { _udFirstUtteredDate = Nothing,
      _udCount = Nothing,
      _udUtteranceString = Nothing,
      _udLastUtteredDate = Nothing,
      _udDistinctUsers = Nothing
    }

-- | The date that the utterance was first recorded.
udFirstUtteredDate :: Lens' UtteranceData (Maybe UTCTime)
udFirstUtteredDate = lens _udFirstUtteredDate (\s a -> s {_udFirstUtteredDate = a}) . mapping _Time

-- | The number of times that the utterance was processed.
udCount :: Lens' UtteranceData (Maybe Int)
udCount = lens _udCount (\s a -> s {_udCount = a})

-- | The text that was entered by the user or the text representation of an audio clip.
udUtteranceString :: Lens' UtteranceData (Maybe Text)
udUtteranceString = lens _udUtteranceString (\s a -> s {_udUtteranceString = a})

-- | The date that the utterance was last recorded.
udLastUtteredDate :: Lens' UtteranceData (Maybe UTCTime)
udLastUtteredDate = lens _udLastUtteredDate (\s a -> s {_udLastUtteredDate = a}) . mapping _Time

-- | The total number of individuals that used the utterance.
udDistinctUsers :: Lens' UtteranceData (Maybe Int)
udDistinctUsers = lens _udDistinctUsers (\s a -> s {_udDistinctUsers = a})

instance FromJSON UtteranceData where
  parseJSON =
    withObject
      "UtteranceData"
      ( \x ->
          UtteranceData'
            <$> (x .:? "firstUtteredDate")
            <*> (x .:? "count")
            <*> (x .:? "utteranceString")
            <*> (x .:? "lastUtteredDate")
            <*> (x .:? "distinctUsers")
      )

instance Hashable UtteranceData

instance NFData UtteranceData
