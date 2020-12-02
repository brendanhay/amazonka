{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Database
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Database where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains metadata information for a database in a data catalog.
--
--
--
-- /See:/ 'database' smart constructor.
data Database = Database'
  { _dParameters ::
      !(Maybe (Map Text (Text))),
    _dDescription :: !(Maybe Text),
    _dName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Database' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dParameters' - A set of custom key/value pairs.
--
-- * 'dDescription' - An optional description of the database.
--
-- * 'dName' - The name of the database.
database ::
  -- | 'dName'
  Text ->
  Database
database pName_ =
  Database'
    { _dParameters = Nothing,
      _dDescription = Nothing,
      _dName = pName_
    }

-- | A set of custom key/value pairs.
dParameters :: Lens' Database (HashMap Text (Text))
dParameters = lens _dParameters (\s a -> s {_dParameters = a}) . _Default . _Map

-- | An optional description of the database.
dDescription :: Lens' Database (Maybe Text)
dDescription = lens _dDescription (\s a -> s {_dDescription = a})

-- | The name of the database.
dName :: Lens' Database Text
dName = lens _dName (\s a -> s {_dName = a})

instance FromJSON Database where
  parseJSON =
    withObject
      "Database"
      ( \x ->
          Database'
            <$> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "Description")
            <*> (x .: "Name")
      )

instance Hashable Database

instance NFData Database
