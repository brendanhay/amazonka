{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.
--
--
--
-- /See:/ 'typedLinkSchemaAndFacetName' smart constructor.
data TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName'
  { _tlsafnSchemaARN ::
      !Text,
    _tlsafnTypedLinkName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedLinkSchemaAndFacetName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlsafnSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- * 'tlsafnTypedLinkName' - The unique name of the typed link facet.
typedLinkSchemaAndFacetName ::
  -- | 'tlsafnSchemaARN'
  Text ->
  -- | 'tlsafnTypedLinkName'
  Text ->
  TypedLinkSchemaAndFacetName
typedLinkSchemaAndFacetName pSchemaARN_ pTypedLinkName_ =
  TypedLinkSchemaAndFacetName'
    { _tlsafnSchemaARN = pSchemaARN_,
      _tlsafnTypedLinkName = pTypedLinkName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
tlsafnSchemaARN :: Lens' TypedLinkSchemaAndFacetName Text
tlsafnSchemaARN = lens _tlsafnSchemaARN (\s a -> s {_tlsafnSchemaARN = a})

-- | The unique name of the typed link facet.
tlsafnTypedLinkName :: Lens' TypedLinkSchemaAndFacetName Text
tlsafnTypedLinkName = lens _tlsafnTypedLinkName (\s a -> s {_tlsafnTypedLinkName = a})

instance FromJSON TypedLinkSchemaAndFacetName where
  parseJSON =
    withObject
      "TypedLinkSchemaAndFacetName"
      ( \x ->
          TypedLinkSchemaAndFacetName'
            <$> (x .: "SchemaArn") <*> (x .: "TypedLinkName")
      )

instance Hashable TypedLinkSchemaAndFacetName

instance NFData TypedLinkSchemaAndFacetName

instance ToJSON TypedLinkSchemaAndFacetName where
  toJSON TypedLinkSchemaAndFacetName' {..} =
    object
      ( catMaybes
          [ Just ("SchemaArn" .= _tlsafnSchemaARN),
            Just ("TypedLinkName" .= _tlsafnTypedLinkName)
          ]
      )
