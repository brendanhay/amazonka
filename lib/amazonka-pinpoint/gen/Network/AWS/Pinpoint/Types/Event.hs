{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Event where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Session
import Network.AWS.Prelude

-- | Specifies information about an event that reports data to Amazon Pinpoint.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eClientSDKVersion :: !(Maybe Text),
    _eMetrics :: !(Maybe (Map Text (Double))),
    _eAppVersionCode :: !(Maybe Text),
    _eAppTitle :: !(Maybe Text),
    _eAppPackageName :: !(Maybe Text),
    _eAttributes :: !(Maybe (Map Text (Text))),
    _eSDKName :: !(Maybe Text),
    _eSession :: !(Maybe Session),
    _eEventType :: !Text,
    _eTimestamp :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eClientSDKVersion' - The version of the SDK that's running on the client device.
--
-- * 'eMetrics' - One or more custom metrics that are associated with the event.
--
-- * 'eAppVersionCode' - The version number of the app that's recording the event.
--
-- * 'eAppTitle' - The title of the app that's recording the event.
--
-- * 'eAppPackageName' - The package name of the app that's recording the event.
--
-- * 'eAttributes' - One or more custom attributes that are associated with the event.
--
-- * 'eSDKName' - The name of the SDK that's being used to record the event.
--
-- * 'eSession' - Information about the session in which the event occurred.
--
-- * 'eEventType' - The name of the event.
--
-- * 'eTimestamp' - The date and time, in ISO 8601 format, when the event occurred.
event ::
  -- | 'eEventType'
  Text ->
  -- | 'eTimestamp'
  Text ->
  Event
event pEventType_ pTimestamp_ =
  Event'
    { _eClientSDKVersion = Nothing,
      _eMetrics = Nothing,
      _eAppVersionCode = Nothing,
      _eAppTitle = Nothing,
      _eAppPackageName = Nothing,
      _eAttributes = Nothing,
      _eSDKName = Nothing,
      _eSession = Nothing,
      _eEventType = pEventType_,
      _eTimestamp = pTimestamp_
    }

-- | The version of the SDK that's running on the client device.
eClientSDKVersion :: Lens' Event (Maybe Text)
eClientSDKVersion = lens _eClientSDKVersion (\s a -> s {_eClientSDKVersion = a})

-- | One or more custom metrics that are associated with the event.
eMetrics :: Lens' Event (HashMap Text (Double))
eMetrics = lens _eMetrics (\s a -> s {_eMetrics = a}) . _Default . _Map

-- | The version number of the app that's recording the event.
eAppVersionCode :: Lens' Event (Maybe Text)
eAppVersionCode = lens _eAppVersionCode (\s a -> s {_eAppVersionCode = a})

-- | The title of the app that's recording the event.
eAppTitle :: Lens' Event (Maybe Text)
eAppTitle = lens _eAppTitle (\s a -> s {_eAppTitle = a})

-- | The package name of the app that's recording the event.
eAppPackageName :: Lens' Event (Maybe Text)
eAppPackageName = lens _eAppPackageName (\s a -> s {_eAppPackageName = a})

-- | One or more custom attributes that are associated with the event.
eAttributes :: Lens' Event (HashMap Text (Text))
eAttributes = lens _eAttributes (\s a -> s {_eAttributes = a}) . _Default . _Map

-- | The name of the SDK that's being used to record the event.
eSDKName :: Lens' Event (Maybe Text)
eSDKName = lens _eSDKName (\s a -> s {_eSDKName = a})

-- | Information about the session in which the event occurred.
eSession :: Lens' Event (Maybe Session)
eSession = lens _eSession (\s a -> s {_eSession = a})

-- | The name of the event.
eEventType :: Lens' Event Text
eEventType = lens _eEventType (\s a -> s {_eEventType = a})

-- | The date and time, in ISO 8601 format, when the event occurred.
eTimestamp :: Lens' Event Text
eTimestamp = lens _eTimestamp (\s a -> s {_eTimestamp = a})

instance Hashable Event

instance NFData Event

instance ToJSON Event where
  toJSON Event' {..} =
    object
      ( catMaybes
          [ ("ClientSdkVersion" .=) <$> _eClientSDKVersion,
            ("Metrics" .=) <$> _eMetrics,
            ("AppVersionCode" .=) <$> _eAppVersionCode,
            ("AppTitle" .=) <$> _eAppTitle,
            ("AppPackageName" .=) <$> _eAppPackageName,
            ("Attributes" .=) <$> _eAttributes,
            ("SdkName" .=) <$> _eSDKName,
            ("Session" .=) <$> _eSession,
            Just ("EventType" .= _eEventType),
            Just ("Timestamp" .= _eTimestamp)
          ]
      )
