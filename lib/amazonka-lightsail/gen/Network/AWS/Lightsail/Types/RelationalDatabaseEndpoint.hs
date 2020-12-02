{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an endpoint for a database.
--
--
--
-- /See:/ 'relationalDatabaseEndpoint' smart constructor.
data RelationalDatabaseEndpoint = RelationalDatabaseEndpoint'
  { _rdeAddress ::
      !(Maybe Text),
    _rdePort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelationalDatabaseEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdeAddress' - Specifies the DNS address of the database.
--
-- * 'rdePort' - Specifies the port that the database is listening on.
relationalDatabaseEndpoint ::
  RelationalDatabaseEndpoint
relationalDatabaseEndpoint =
  RelationalDatabaseEndpoint'
    { _rdeAddress = Nothing,
      _rdePort = Nothing
    }

-- | Specifies the DNS address of the database.
rdeAddress :: Lens' RelationalDatabaseEndpoint (Maybe Text)
rdeAddress = lens _rdeAddress (\s a -> s {_rdeAddress = a})

-- | Specifies the port that the database is listening on.
rdePort :: Lens' RelationalDatabaseEndpoint (Maybe Int)
rdePort = lens _rdePort (\s a -> s {_rdePort = a})

instance FromJSON RelationalDatabaseEndpoint where
  parseJSON =
    withObject
      "RelationalDatabaseEndpoint"
      ( \x ->
          RelationalDatabaseEndpoint'
            <$> (x .:? "address") <*> (x .:? "port")
      )

instance Hashable RelationalDatabaseEndpoint

instance NFData RelationalDatabaseEndpoint
