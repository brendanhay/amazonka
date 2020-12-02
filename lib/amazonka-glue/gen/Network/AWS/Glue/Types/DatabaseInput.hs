{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DatabaseInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseInput where

import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.PrincipalPermissions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure used to create or update a database.
--
--
--
-- /See:/ 'databaseInput' smart constructor.
data DatabaseInput = DatabaseInput'
  { _diLocationURI ::
      !(Maybe Text),
    _diTargetDatabase :: !(Maybe DatabaseIdentifier),
    _diParameters :: !(Maybe (Map Text (Text))),
    _diDescription :: !(Maybe Text),
    _diCreateTableDefaultPermissions ::
      !(Maybe [PrincipalPermissions]),
    _diName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatabaseInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diLocationURI' - The location of the database (for example, an HDFS path).
--
-- * 'diTargetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for resource linking.
--
-- * 'diParameters' - These key-value pairs define parameters and properties of the database. These key-value pairs define parameters and properties of the database.
--
-- * 'diDescription' - A description of the database.
--
-- * 'diCreateTableDefaultPermissions' - Creates a set of default permissions on the table for principals.
--
-- * 'diName' - The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
databaseInput ::
  -- | 'diName'
  Text ->
  DatabaseInput
databaseInput pName_ =
  DatabaseInput'
    { _diLocationURI = Nothing,
      _diTargetDatabase = Nothing,
      _diParameters = Nothing,
      _diDescription = Nothing,
      _diCreateTableDefaultPermissions = Nothing,
      _diName = pName_
    }

-- | The location of the database (for example, an HDFS path).
diLocationURI :: Lens' DatabaseInput (Maybe Text)
diLocationURI = lens _diLocationURI (\s a -> s {_diLocationURI = a})

-- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
diTargetDatabase :: Lens' DatabaseInput (Maybe DatabaseIdentifier)
diTargetDatabase = lens _diTargetDatabase (\s a -> s {_diTargetDatabase = a})

-- | These key-value pairs define parameters and properties of the database. These key-value pairs define parameters and properties of the database.
diParameters :: Lens' DatabaseInput (HashMap Text (Text))
diParameters = lens _diParameters (\s a -> s {_diParameters = a}) . _Default . _Map

-- | A description of the database.
diDescription :: Lens' DatabaseInput (Maybe Text)
diDescription = lens _diDescription (\s a -> s {_diDescription = a})

-- | Creates a set of default permissions on the table for principals.
diCreateTableDefaultPermissions :: Lens' DatabaseInput [PrincipalPermissions]
diCreateTableDefaultPermissions = lens _diCreateTableDefaultPermissions (\s a -> s {_diCreateTableDefaultPermissions = a}) . _Default . _Coerce

-- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
diName :: Lens' DatabaseInput Text
diName = lens _diName (\s a -> s {_diName = a})

instance Hashable DatabaseInput

instance NFData DatabaseInput

instance ToJSON DatabaseInput where
  toJSON DatabaseInput' {..} =
    object
      ( catMaybes
          [ ("LocationUri" .=) <$> _diLocationURI,
            ("TargetDatabase" .=) <$> _diTargetDatabase,
            ("Parameters" .=) <$> _diParameters,
            ("Description" .=) <$> _diDescription,
            ("CreateTableDefaultPermissions" .=)
              <$> _diCreateTableDefaultPermissions,
            Just ("Name" .= _diName)
          ]
      )
