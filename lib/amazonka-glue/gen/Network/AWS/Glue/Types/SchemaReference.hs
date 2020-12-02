{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaReference where

import Network.AWS.Glue.Types.SchemaId
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that references a schema stored in the AWS Glue Schema Registry.
--
--
--
-- /See:/ 'schemaReference' smart constructor.
data SchemaReference = SchemaReference'
  { _srSchemaVersionId ::
      !(Maybe Text),
    _srSchemaId :: !(Maybe SchemaId),
    _srSchemaVersionNumber :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srSchemaVersionId' - The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
--
-- * 'srSchemaId' - A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
--
-- * 'srSchemaVersionNumber' - The version number of the schema.
schemaReference ::
  SchemaReference
schemaReference =
  SchemaReference'
    { _srSchemaVersionId = Nothing,
      _srSchemaId = Nothing,
      _srSchemaVersionNumber = Nothing
    }

-- | The unique ID assigned to a version of the schema. Either this or the @SchemaId@ has to be provided.
srSchemaVersionId :: Lens' SchemaReference (Maybe Text)
srSchemaVersionId = lens _srSchemaVersionId (\s a -> s {_srSchemaVersionId = a})

-- | A structure that contains schema identity fields. Either this or the @SchemaVersionId@ has to be provided.
srSchemaId :: Lens' SchemaReference (Maybe SchemaId)
srSchemaId = lens _srSchemaId (\s a -> s {_srSchemaId = a})

-- | The version number of the schema.
srSchemaVersionNumber :: Lens' SchemaReference (Maybe Natural)
srSchemaVersionNumber = lens _srSchemaVersionNumber (\s a -> s {_srSchemaVersionNumber = a}) . mapping _Nat

instance FromJSON SchemaReference where
  parseJSON =
    withObject
      "SchemaReference"
      ( \x ->
          SchemaReference'
            <$> (x .:? "SchemaVersionId")
            <*> (x .:? "SchemaId")
            <*> (x .:? "SchemaVersionNumber")
      )

instance Hashable SchemaReference

instance NFData SchemaReference

instance ToJSON SchemaReference where
  toJSON SchemaReference' {..} =
    object
      ( catMaybes
          [ ("SchemaVersionId" .=) <$> _srSchemaVersionId,
            ("SchemaId" .=) <$> _srSchemaId,
            ("SchemaVersionNumber" .=) <$> _srSchemaVersionNumber
          ]
      )
