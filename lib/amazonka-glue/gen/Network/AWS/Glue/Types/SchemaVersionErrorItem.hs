{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionErrorItem where

import Network.AWS.Glue.Types.ErrorDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains the error details for an operation on a schema version.
--
--
--
-- /See:/ 'schemaVersionErrorItem' smart constructor.
data SchemaVersionErrorItem = SchemaVersionErrorItem'
  { _sveiVersionNumber ::
      !(Maybe Nat),
    _sveiErrorDetails :: !(Maybe ErrorDetails)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaVersionErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sveiVersionNumber' - The version number of the schema.
--
-- * 'sveiErrorDetails' - The details of the error for the schema version.
schemaVersionErrorItem ::
  SchemaVersionErrorItem
schemaVersionErrorItem =
  SchemaVersionErrorItem'
    { _sveiVersionNumber = Nothing,
      _sveiErrorDetails = Nothing
    }

-- | The version number of the schema.
sveiVersionNumber :: Lens' SchemaVersionErrorItem (Maybe Natural)
sveiVersionNumber = lens _sveiVersionNumber (\s a -> s {_sveiVersionNumber = a}) . mapping _Nat

-- | The details of the error for the schema version.
sveiErrorDetails :: Lens' SchemaVersionErrorItem (Maybe ErrorDetails)
sveiErrorDetails = lens _sveiErrorDetails (\s a -> s {_sveiErrorDetails = a})

instance FromJSON SchemaVersionErrorItem where
  parseJSON =
    withObject
      "SchemaVersionErrorItem"
      ( \x ->
          SchemaVersionErrorItem'
            <$> (x .:? "VersionNumber") <*> (x .:? "ErrorDetails")
      )

instance Hashable SchemaVersionErrorItem

instance NFData SchemaVersionErrorItem
