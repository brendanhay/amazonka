{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EventsBatch
import Network.AWS.Prelude

-- | Specifies a batch of events to process.
--
--
--
-- /See:/ 'eventsRequest' smart constructor.
newtype EventsRequest = EventsRequest'
  { _erBatchItem ::
      Map Text (EventsBatch)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erBatchItem' - The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
eventsRequest ::
  EventsRequest
eventsRequest = EventsRequest' {_erBatchItem = mempty}

-- | The batch of events to process. For each item in a batch, the endpoint ID acts as a key that has an EventsBatch object as its value.
erBatchItem :: Lens' EventsRequest (HashMap Text (EventsBatch))
erBatchItem = lens _erBatchItem (\s a -> s {_erBatchItem = a}) . _Map

instance Hashable EventsRequest

instance NFData EventsRequest

instance ToJSON EventsRequest where
  toJSON EventsRequest' {..} =
    object (catMaybes [Just ("BatchItem" .= _erBatchItem)])
