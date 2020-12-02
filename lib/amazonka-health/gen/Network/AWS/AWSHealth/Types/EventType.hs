{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventType where

import Network.AWS.AWSHealth.Types.EventTypeCategory
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, @issue@ ), the service (for example, @EC2@ ), and the event type code (for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
--
--
--
-- /See:/ 'eventType' smart constructor.
data EventType = EventType'
  { _etService :: !(Maybe Text),
    _etCategory :: !(Maybe EventTypeCategory),
    _etCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etService' - The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- * 'etCategory' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- * 'etCode' - The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
eventType ::
  EventType
eventType =
  EventType'
    { _etService = Nothing,
      _etCategory = Nothing,
      _etCode = Nothing
    }

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
etService :: Lens' EventType (Maybe Text)
etService = lens _etService (\s a -> s {_etService = a})

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
etCategory :: Lens' EventType (Maybe EventTypeCategory)
etCategory = lens _etCategory (\s a -> s {_etCategory = a})

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
etCode :: Lens' EventType (Maybe Text)
etCode = lens _etCode (\s a -> s {_etCode = a})

instance FromJSON EventType where
  parseJSON =
    withObject
      "EventType"
      ( \x ->
          EventType'
            <$> (x .:? "service") <*> (x .:? "category") <*> (x .:? "code")
      )

instance Hashable EventType

instance NFData EventType
