{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Edge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Edge where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An edge represents a directed connection between two AWS Glue components that are part of the workflow the edge belongs to.
--
--
--
-- /See:/ 'edge' smart constructor.
data Edge = Edge'
  { _eSourceId :: !(Maybe Text),
    _eDestinationId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Edge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceId' - The unique of the node within the workflow where the edge starts.
--
-- * 'eDestinationId' - The unique of the node within the workflow where the edge ends.
edge ::
  Edge
edge = Edge' {_eSourceId = Nothing, _eDestinationId = Nothing}

-- | The unique of the node within the workflow where the edge starts.
eSourceId :: Lens' Edge (Maybe Text)
eSourceId = lens _eSourceId (\s a -> s {_eSourceId = a})

-- | The unique of the node within the workflow where the edge ends.
eDestinationId :: Lens' Edge (Maybe Text)
eDestinationId = lens _eDestinationId (\s a -> s {_eDestinationId = a})

instance FromJSON Edge where
  parseJSON =
    withObject
      "Edge"
      (\x -> Edge' <$> (x .:? "SourceId") <*> (x .:? "DestinationId"))

instance Hashable Edge

instance NFData Edge
