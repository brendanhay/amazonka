{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotMetadata where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.LexStatus
import Network.AWS.Prelude

-- | Provides information about a bot. .
--
--
--
-- /See:/ 'botMetadata' smart constructor.
data BotMetadata = BotMetadata'
  { _bmStatus :: !(Maybe LexStatus),
    _bmCreatedDate :: !(Maybe POSIX),
    _bmName :: !(Maybe Text),
    _bmVersion :: !(Maybe Text),
    _bmLastUpdatedDate :: !(Maybe POSIX),
    _bmDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BotMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmStatus' - The status of the bot.
--
-- * 'bmCreatedDate' - The date that the bot was created.
--
-- * 'bmName' - The name of the bot.
--
-- * 'bmVersion' - The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- * 'bmLastUpdatedDate' - The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
--
-- * 'bmDescription' - A description of the bot.
botMetadata ::
  BotMetadata
botMetadata =
  BotMetadata'
    { _bmStatus = Nothing,
      _bmCreatedDate = Nothing,
      _bmName = Nothing,
      _bmVersion = Nothing,
      _bmLastUpdatedDate = Nothing,
      _bmDescription = Nothing
    }

-- | The status of the bot.
bmStatus :: Lens' BotMetadata (Maybe LexStatus)
bmStatus = lens _bmStatus (\s a -> s {_bmStatus = a})

-- | The date that the bot was created.
bmCreatedDate :: Lens' BotMetadata (Maybe UTCTime)
bmCreatedDate = lens _bmCreatedDate (\s a -> s {_bmCreatedDate = a}) . mapping _Time

-- | The name of the bot.
bmName :: Lens' BotMetadata (Maybe Text)
bmName = lens _bmName (\s a -> s {_bmName = a})

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
bmVersion :: Lens' BotMetadata (Maybe Text)
bmVersion = lens _bmVersion (\s a -> s {_bmVersion = a})

-- | The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
bmLastUpdatedDate :: Lens' BotMetadata (Maybe UTCTime)
bmLastUpdatedDate = lens _bmLastUpdatedDate (\s a -> s {_bmLastUpdatedDate = a}) . mapping _Time

-- | A description of the bot.
bmDescription :: Lens' BotMetadata (Maybe Text)
bmDescription = lens _bmDescription (\s a -> s {_bmDescription = a})

instance FromJSON BotMetadata where
  parseJSON =
    withObject
      "BotMetadata"
      ( \x ->
          BotMetadata'
            <$> (x .:? "status")
            <*> (x .:? "createdDate")
            <*> (x .:? "name")
            <*> (x .:? "version")
            <*> (x .:? "lastUpdatedDate")
            <*> (x .:? "description")
      )

instance Hashable BotMetadata

instance NFData BotMetadata
