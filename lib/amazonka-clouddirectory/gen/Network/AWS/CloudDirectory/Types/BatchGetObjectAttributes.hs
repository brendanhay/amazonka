{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Retrieves attributes within a facet that are associated with an object inside an 'BatchRead' operation. For more information, see 'GetObjectAttributes' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchGetObjectAttributes' smart constructor.
data BatchGetObjectAttributes = BatchGetObjectAttributes'
  { _bgoaObjectReference ::
      !ObjectReference,
    _bgoaSchemaFacet :: !SchemaFacet,
    _bgoaAttributeNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoaObjectReference' - Reference that identifies the object whose attributes will be retrieved.
--
-- * 'bgoaSchemaFacet' - Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
--
-- * 'bgoaAttributeNames' - List of attribute names whose values will be retrieved.
batchGetObjectAttributes ::
  -- | 'bgoaObjectReference'
  ObjectReference ->
  -- | 'bgoaSchemaFacet'
  SchemaFacet ->
  BatchGetObjectAttributes
batchGetObjectAttributes pObjectReference_ pSchemaFacet_ =
  BatchGetObjectAttributes'
    { _bgoaObjectReference =
        pObjectReference_,
      _bgoaSchemaFacet = pSchemaFacet_,
      _bgoaAttributeNames = mempty
    }

-- | Reference that identifies the object whose attributes will be retrieved.
bgoaObjectReference :: Lens' BatchGetObjectAttributes ObjectReference
bgoaObjectReference = lens _bgoaObjectReference (\s a -> s {_bgoaObjectReference = a})

-- | Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
bgoaSchemaFacet :: Lens' BatchGetObjectAttributes SchemaFacet
bgoaSchemaFacet = lens _bgoaSchemaFacet (\s a -> s {_bgoaSchemaFacet = a})

-- | List of attribute names whose values will be retrieved.
bgoaAttributeNames :: Lens' BatchGetObjectAttributes [Text]
bgoaAttributeNames = lens _bgoaAttributeNames (\s a -> s {_bgoaAttributeNames = a}) . _Coerce

instance Hashable BatchGetObjectAttributes

instance NFData BatchGetObjectAttributes

instance ToJSON BatchGetObjectAttributes where
  toJSON BatchGetObjectAttributes' {..} =
    object
      ( catMaybes
          [ Just ("ObjectReference" .= _bgoaObjectReference),
            Just ("SchemaFacet" .= _bgoaSchemaFacet),
            Just ("AttributeNames" .= _bgoaAttributeNames)
          ]
      )
