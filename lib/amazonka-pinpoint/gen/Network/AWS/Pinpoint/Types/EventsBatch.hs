{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventsBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventsBatch where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Event
import Network.AWS.Pinpoint.Types.PublicEndpoint
import Network.AWS.Prelude

-- | Specifies a batch of endpoints and events to process.
--
--
--
-- /See:/ 'eventsBatch' smart constructor.
data EventsBatch = EventsBatch'
  { _ebEndpoint :: !PublicEndpoint,
    _ebEvents :: !(Map Text (Event))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventsBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebEndpoint' - A set of properties and attributes that are associated with the endpoint.
--
-- * 'ebEvents' - A set of properties that are associated with the event.
eventsBatch ::
  -- | 'ebEndpoint'
  PublicEndpoint ->
  EventsBatch
eventsBatch pEndpoint_ =
  EventsBatch' {_ebEndpoint = pEndpoint_, _ebEvents = mempty}

-- | A set of properties and attributes that are associated with the endpoint.
ebEndpoint :: Lens' EventsBatch PublicEndpoint
ebEndpoint = lens _ebEndpoint (\s a -> s {_ebEndpoint = a})

-- | A set of properties that are associated with the event.
ebEvents :: Lens' EventsBatch (HashMap Text (Event))
ebEvents = lens _ebEvents (\s a -> s {_ebEvents = a}) . _Map

instance Hashable EventsBatch

instance NFData EventsBatch

instance ToJSON EventsBatch where
  toJSON EventsBatch' {..} =
    object
      ( catMaybes
          [Just ("Endpoint" .= _ebEndpoint), Just ("Events" .= _ebEvents)]
      )
