{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType where

import Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the event feedback type.
--
--
--
-- /See:/ 'eventFeedbackType' smart constructor.
data EventFeedbackType = EventFeedbackType'
  { _eftFeedbackDate ::
      !(Maybe POSIX),
    _eftFeedbackValue :: !FeedbackValueType,
    _eftProvider :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventFeedbackType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eftFeedbackDate' - The event feedback date.
--
-- * 'eftFeedbackValue' - The event feedback value.
--
-- * 'eftProvider' - The provider.
eventFeedbackType ::
  -- | 'eftFeedbackValue'
  FeedbackValueType ->
  -- | 'eftProvider'
  Text ->
  EventFeedbackType
eventFeedbackType pFeedbackValue_ pProvider_ =
  EventFeedbackType'
    { _eftFeedbackDate = Nothing,
      _eftFeedbackValue = pFeedbackValue_,
      _eftProvider = pProvider_
    }

-- | The event feedback date.
eftFeedbackDate :: Lens' EventFeedbackType (Maybe UTCTime)
eftFeedbackDate = lens _eftFeedbackDate (\s a -> s {_eftFeedbackDate = a}) . mapping _Time

-- | The event feedback value.
eftFeedbackValue :: Lens' EventFeedbackType FeedbackValueType
eftFeedbackValue = lens _eftFeedbackValue (\s a -> s {_eftFeedbackValue = a})

-- | The provider.
eftProvider :: Lens' EventFeedbackType Text
eftProvider = lens _eftProvider (\s a -> s {_eftProvider = a})

instance FromJSON EventFeedbackType where
  parseJSON =
    withObject
      "EventFeedbackType"
      ( \x ->
          EventFeedbackType'
            <$> (x .:? "FeedbackDate")
            <*> (x .: "FeedbackValue")
            <*> (x .: "Provider")
      )

instance Hashable EventFeedbackType

instance NFData EventFeedbackType
