{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an event to be submitted.
--
--
--
-- /See:/ 'putEventsRequestEntry' smart constructor.
data PutEventsRequestEntry = PutEventsRequestEntry'
  { _pereTime ::
      !(Maybe POSIX),
    _pereDetailType :: !(Maybe Text),
    _pereResources :: !(Maybe [Text]),
    _pereEventBusName :: !(Maybe Text),
    _pereSource :: !(Maybe Text),
    _pereDetail :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutEventsRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pereTime' - The time stamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no time stamp is provided, the time stamp of the 'PutEvents' call is used.
--
-- * 'pereDetailType' - Free-form string used to decide what fields to expect in the event detail.
--
-- * 'pereResources' - AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
--
-- * 'pereEventBusName' - The name or ARN of the event bus to receive the event. Only the rules that are associated with this event bus are used to match the event. If you omit this, the default event bus is used.
--
-- * 'pereSource' - The source of the event.
--
-- * 'pereDetail' - A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
putEventsRequestEntry ::
  PutEventsRequestEntry
putEventsRequestEntry =
  PutEventsRequestEntry'
    { _pereTime = Nothing,
      _pereDetailType = Nothing,
      _pereResources = Nothing,
      _pereEventBusName = Nothing,
      _pereSource = Nothing,
      _pereDetail = Nothing
    }

-- | The time stamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no time stamp is provided, the time stamp of the 'PutEvents' call is used.
pereTime :: Lens' PutEventsRequestEntry (Maybe UTCTime)
pereTime = lens _pereTime (\s a -> s {_pereTime = a}) . mapping _Time

-- | Free-form string used to decide what fields to expect in the event detail.
pereDetailType :: Lens' PutEventsRequestEntry (Maybe Text)
pereDetailType = lens _pereDetailType (\s a -> s {_pereDetailType = a})

-- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
pereResources :: Lens' PutEventsRequestEntry [Text]
pereResources = lens _pereResources (\s a -> s {_pereResources = a}) . _Default . _Coerce

-- | The name or ARN of the event bus to receive the event. Only the rules that are associated with this event bus are used to match the event. If you omit this, the default event bus is used.
pereEventBusName :: Lens' PutEventsRequestEntry (Maybe Text)
pereEventBusName = lens _pereEventBusName (\s a -> s {_pereEventBusName = a})

-- | The source of the event.
pereSource :: Lens' PutEventsRequestEntry (Maybe Text)
pereSource = lens _pereSource (\s a -> s {_pereSource = a})

-- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
pereDetail :: Lens' PutEventsRequestEntry (Maybe Text)
pereDetail = lens _pereDetail (\s a -> s {_pereDetail = a})

instance Hashable PutEventsRequestEntry

instance NFData PutEventsRequestEntry

instance ToJSON PutEventsRequestEntry where
  toJSON PutEventsRequestEntry' {..} =
    object
      ( catMaybes
          [ ("Time" .=) <$> _pereTime,
            ("DetailType" .=) <$> _pereDetailType,
            ("Resources" .=) <$> _pereResources,
            ("EventBusName" .=) <$> _pereEventBusName,
            ("Source" .=) <$> _pereSource,
            ("Detail" .=) <$> _pereDetail
          ]
      )
