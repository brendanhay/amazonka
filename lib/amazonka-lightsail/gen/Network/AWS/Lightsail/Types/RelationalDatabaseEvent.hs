{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEvent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an event for a database.
--
--
--
-- /See:/ 'relationalDatabaseEvent' smart constructor.
data RelationalDatabaseEvent = RelationalDatabaseEvent'
  { _rdeCreatedAt ::
      !(Maybe POSIX),
    _rdeEventCategories :: !(Maybe [Text]),
    _rdeResource :: !(Maybe Text),
    _rdeMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelationalDatabaseEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdeCreatedAt' - The timestamp when the database event was created.
--
-- * 'rdeEventCategories' - The category that the database event belongs to.
--
-- * 'rdeResource' - The database that the database event relates to.
--
-- * 'rdeMessage' - The message of the database event.
relationalDatabaseEvent ::
  RelationalDatabaseEvent
relationalDatabaseEvent =
  RelationalDatabaseEvent'
    { _rdeCreatedAt = Nothing,
      _rdeEventCategories = Nothing,
      _rdeResource = Nothing,
      _rdeMessage = Nothing
    }

-- | The timestamp when the database event was created.
rdeCreatedAt :: Lens' RelationalDatabaseEvent (Maybe UTCTime)
rdeCreatedAt = lens _rdeCreatedAt (\s a -> s {_rdeCreatedAt = a}) . mapping _Time

-- | The category that the database event belongs to.
rdeEventCategories :: Lens' RelationalDatabaseEvent [Text]
rdeEventCategories = lens _rdeEventCategories (\s a -> s {_rdeEventCategories = a}) . _Default . _Coerce

-- | The database that the database event relates to.
rdeResource :: Lens' RelationalDatabaseEvent (Maybe Text)
rdeResource = lens _rdeResource (\s a -> s {_rdeResource = a})

-- | The message of the database event.
rdeMessage :: Lens' RelationalDatabaseEvent (Maybe Text)
rdeMessage = lens _rdeMessage (\s a -> s {_rdeMessage = a})

instance FromJSON RelationalDatabaseEvent where
  parseJSON =
    withObject
      "RelationalDatabaseEvent"
      ( \x ->
          RelationalDatabaseEvent'
            <$> (x .:? "createdAt")
            <*> (x .:? "eventCategories" .!= mempty)
            <*> (x .:? "resource")
            <*> (x .:? "message")
      )

instance Hashable RelationalDatabaseEvent

instance NFData RelationalDatabaseEvent
