{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.UtteranceList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.UtteranceList where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.UtteranceData
import Network.AWS.Prelude

-- | Provides a list of utterances that have been made to a specific version of your bot. The list contains a maximum of 100 utterances.
--
--
--
-- /See:/ 'utteranceList' smart constructor.
data UtteranceList = UtteranceList'
  { _ulBotVersion :: !(Maybe Text),
    _ulUtterances :: !(Maybe [UtteranceData])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UtteranceList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulBotVersion' - The version of the bot that processed the list.
--
-- * 'ulUtterances' - One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
utteranceList ::
  UtteranceList
utteranceList =
  UtteranceList' {_ulBotVersion = Nothing, _ulUtterances = Nothing}

-- | The version of the bot that processed the list.
ulBotVersion :: Lens' UtteranceList (Maybe Text)
ulBotVersion = lens _ulBotVersion (\s a -> s {_ulBotVersion = a})

-- | One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
ulUtterances :: Lens' UtteranceList [UtteranceData]
ulUtterances = lens _ulUtterances (\s a -> s {_ulUtterances = a}) . _Default . _Coerce

instance FromJSON UtteranceList where
  parseJSON =
    withObject
      "UtteranceList"
      ( \x ->
          UtteranceList'
            <$> (x .:? "botVersion") <*> (x .:? "utterances" .!= mempty)
      )

instance Hashable UtteranceList

instance NFData UtteranceList
