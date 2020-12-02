{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionsList where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the connections used by a job.
--
--
--
-- /See:/ 'connectionsList' smart constructor.
newtype ConnectionsList = ConnectionsList'
  { _clConnections ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clConnections' - A list of connections used by the job.
connectionsList ::
  ConnectionsList
connectionsList = ConnectionsList' {_clConnections = Nothing}

-- | A list of connections used by the job.
clConnections :: Lens' ConnectionsList [Text]
clConnections = lens _clConnections (\s a -> s {_clConnections = a}) . _Default . _Coerce

instance FromJSON ConnectionsList where
  parseJSON =
    withObject
      "ConnectionsList"
      (\x -> ConnectionsList' <$> (x .:? "Connections" .!= mempty))

instance Hashable ConnectionsList

instance NFData ConnectionsList

instance ToJSON ConnectionsList where
  toJSON ConnectionsList' {..} =
    object (catMaybes [("Connections" .=) <$> _clConnections])
