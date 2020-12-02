{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GetConnectionsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GetConnectionsFilter where

import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters the connection definitions that are returned by the @GetConnections@ API operation.
--
--
--
-- /See:/ 'getConnectionsFilter' smart constructor.
data GetConnectionsFilter = GetConnectionsFilter'
  { _gcfMatchCriteria ::
      !(Maybe [Text]),
    _gcfConnectionType :: !(Maybe ConnectionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnectionsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfMatchCriteria' - A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
--
-- * 'gcfConnectionType' - The type of connections to return. Currently, SFTP is not supported.
getConnectionsFilter ::
  GetConnectionsFilter
getConnectionsFilter =
  GetConnectionsFilter'
    { _gcfMatchCriteria = Nothing,
      _gcfConnectionType = Nothing
    }

-- | A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
gcfMatchCriteria :: Lens' GetConnectionsFilter [Text]
gcfMatchCriteria = lens _gcfMatchCriteria (\s a -> s {_gcfMatchCriteria = a}) . _Default . _Coerce

-- | The type of connections to return. Currently, SFTP is not supported.
gcfConnectionType :: Lens' GetConnectionsFilter (Maybe ConnectionType)
gcfConnectionType = lens _gcfConnectionType (\s a -> s {_gcfConnectionType = a})

instance Hashable GetConnectionsFilter

instance NFData GetConnectionsFilter

instance ToJSON GetConnectionsFilter where
  toJSON GetConnectionsFilter' {..} =
    object
      ( catMaybes
          [ ("MatchCriteria" .=) <$> _gcfMatchCriteria,
            ("ConnectionType" .=) <$> _gcfConnectionType
          ]
      )
