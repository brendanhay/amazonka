{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Dimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Dimensions where

import Network.AWS.Connect.Types.Channel
import Network.AWS.Connect.Types.QueueReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the dimensions for a set of metrics.
--
--
--
-- /See:/ 'dimensions' smart constructor.
data Dimensions = Dimensions'
  { _dChannel :: !(Maybe Channel),
    _dQueue :: !(Maybe QueueReference)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Dimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dChannel' - The channel used for grouping and filters.
--
-- * 'dQueue' - Information about the queue for which metrics are returned.
dimensions ::
  Dimensions
dimensions = Dimensions' {_dChannel = Nothing, _dQueue = Nothing}

-- | The channel used for grouping and filters.
dChannel :: Lens' Dimensions (Maybe Channel)
dChannel = lens _dChannel (\s a -> s {_dChannel = a})

-- | Information about the queue for which metrics are returned.
dQueue :: Lens' Dimensions (Maybe QueueReference)
dQueue = lens _dQueue (\s a -> s {_dQueue = a})

instance FromJSON Dimensions where
  parseJSON =
    withObject
      "Dimensions"
      (\x -> Dimensions' <$> (x .:? "Channel") <*> (x .:? "Queue"))

instance Hashable Dimensions

instance NFData Dimensions
