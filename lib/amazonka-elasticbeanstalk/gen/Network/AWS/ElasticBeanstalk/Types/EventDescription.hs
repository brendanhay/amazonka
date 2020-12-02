{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EventDescription where

import Network.AWS.ElasticBeanstalk.Types.EventSeverity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an event.
--
--
--
-- /See:/ 'eventDescription' smart constructor.
data EventDescription = EventDescription'
  { _edRequestId ::
      !(Maybe Text),
    _edTemplateName :: !(Maybe Text),
    _edSeverity :: !(Maybe EventSeverity),
    _edVersionLabel :: !(Maybe Text),
    _edPlatformARN :: !(Maybe Text),
    _edEnvironmentName :: !(Maybe Text),
    _edApplicationName :: !(Maybe Text),
    _edEventDate :: !(Maybe ISO8601),
    _edMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edRequestId' - The web service request ID for the activity of this event.
--
-- * 'edTemplateName' - The name of the configuration associated with this event.
--
-- * 'edSeverity' - The severity level of this event.
--
-- * 'edVersionLabel' - The release label for the application version associated with this event.
--
-- * 'edPlatformARN' - The ARN of the platform version.
--
-- * 'edEnvironmentName' - The name of the environment associated with this event.
--
-- * 'edApplicationName' - The application associated with the event.
--
-- * 'edEventDate' - The date when the event occurred.
--
-- * 'edMessage' - The event message.
eventDescription ::
  EventDescription
eventDescription =
  EventDescription'
    { _edRequestId = Nothing,
      _edTemplateName = Nothing,
      _edSeverity = Nothing,
      _edVersionLabel = Nothing,
      _edPlatformARN = Nothing,
      _edEnvironmentName = Nothing,
      _edApplicationName = Nothing,
      _edEventDate = Nothing,
      _edMessage = Nothing
    }

-- | The web service request ID for the activity of this event.
edRequestId :: Lens' EventDescription (Maybe Text)
edRequestId = lens _edRequestId (\s a -> s {_edRequestId = a})

-- | The name of the configuration associated with this event.
edTemplateName :: Lens' EventDescription (Maybe Text)
edTemplateName = lens _edTemplateName (\s a -> s {_edTemplateName = a})

-- | The severity level of this event.
edSeverity :: Lens' EventDescription (Maybe EventSeverity)
edSeverity = lens _edSeverity (\s a -> s {_edSeverity = a})

-- | The release label for the application version associated with this event.
edVersionLabel :: Lens' EventDescription (Maybe Text)
edVersionLabel = lens _edVersionLabel (\s a -> s {_edVersionLabel = a})

-- | The ARN of the platform version.
edPlatformARN :: Lens' EventDescription (Maybe Text)
edPlatformARN = lens _edPlatformARN (\s a -> s {_edPlatformARN = a})

-- | The name of the environment associated with this event.
edEnvironmentName :: Lens' EventDescription (Maybe Text)
edEnvironmentName = lens _edEnvironmentName (\s a -> s {_edEnvironmentName = a})

-- | The application associated with the event.
edApplicationName :: Lens' EventDescription (Maybe Text)
edApplicationName = lens _edApplicationName (\s a -> s {_edApplicationName = a})

-- | The date when the event occurred.
edEventDate :: Lens' EventDescription (Maybe UTCTime)
edEventDate = lens _edEventDate (\s a -> s {_edEventDate = a}) . mapping _Time

-- | The event message.
edMessage :: Lens' EventDescription (Maybe Text)
edMessage = lens _edMessage (\s a -> s {_edMessage = a})

instance FromXML EventDescription where
  parseXML x =
    EventDescription'
      <$> (x .@? "RequestId")
      <*> (x .@? "TemplateName")
      <*> (x .@? "Severity")
      <*> (x .@? "VersionLabel")
      <*> (x .@? "PlatformArn")
      <*> (x .@? "EnvironmentName")
      <*> (x .@? "ApplicationName")
      <*> (x .@? "EventDate")
      <*> (x .@? "Message")

instance Hashable EventDescription

instance NFData EventDescription
