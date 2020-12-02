{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Filters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Filters where

import Network.AWS.Connect.Types.Channel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the filter to apply when retrieving metrics.
--
--
--
-- /See:/ 'filters' smart constructor.
data Filters = Filters'
  { _fQueues :: !(Maybe (List1 Text)),
    _fChannels :: !(Maybe [Channel])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fQueues' - The queues to use to filter the metrics. You can specify up to 100 queues per request.
--
-- * 'fChannels' - The channel to use to filter the metrics.
filters ::
  Filters
filters = Filters' {_fQueues = Nothing, _fChannels = Nothing}

-- | The queues to use to filter the metrics. You can specify up to 100 queues per request.
fQueues :: Lens' Filters (Maybe (NonEmpty Text))
fQueues = lens _fQueues (\s a -> s {_fQueues = a}) . mapping _List1

-- | The channel to use to filter the metrics.
fChannels :: Lens' Filters [Channel]
fChannels = lens _fChannels (\s a -> s {_fChannels = a}) . _Default . _Coerce

instance Hashable Filters

instance NFData Filters

instance ToJSON Filters where
  toJSON Filters' {..} =
    object
      ( catMaybes
          [("Queues" .=) <$> _fQueues, ("Channels" .=) <$> _fChannels]
      )
