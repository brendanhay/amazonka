{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAddFacetToObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAddFacetToObject where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a batch add facet to object operation.
--
--
--
-- /See:/ 'batchAddFacetToObject' smart constructor.
data BatchAddFacetToObject = BatchAddFacetToObject'
  { _baftoSchemaFacet ::
      !SchemaFacet,
    _baftoObjectAttributeList ::
      ![AttributeKeyAndValue],
    _baftoObjectReference :: !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAddFacetToObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baftoSchemaFacet' - Represents the facet being added to the object.
--
-- * 'baftoObjectAttributeList' - The attributes to set on the object.
--
-- * 'baftoObjectReference' - A reference to the object being mutated.
batchAddFacetToObject ::
  -- | 'baftoSchemaFacet'
  SchemaFacet ->
  -- | 'baftoObjectReference'
  ObjectReference ->
  BatchAddFacetToObject
batchAddFacetToObject pSchemaFacet_ pObjectReference_ =
  BatchAddFacetToObject'
    { _baftoSchemaFacet = pSchemaFacet_,
      _baftoObjectAttributeList = mempty,
      _baftoObjectReference = pObjectReference_
    }

-- | Represents the facet being added to the object.
baftoSchemaFacet :: Lens' BatchAddFacetToObject SchemaFacet
baftoSchemaFacet = lens _baftoSchemaFacet (\s a -> s {_baftoSchemaFacet = a})

-- | The attributes to set on the object.
baftoObjectAttributeList :: Lens' BatchAddFacetToObject [AttributeKeyAndValue]
baftoObjectAttributeList = lens _baftoObjectAttributeList (\s a -> s {_baftoObjectAttributeList = a}) . _Coerce

-- | A reference to the object being mutated.
baftoObjectReference :: Lens' BatchAddFacetToObject ObjectReference
baftoObjectReference = lens _baftoObjectReference (\s a -> s {_baftoObjectReference = a})

instance Hashable BatchAddFacetToObject

instance NFData BatchAddFacetToObject

instance ToJSON BatchAddFacetToObject where
  toJSON BatchAddFacetToObject' {..} =
    object
      ( catMaybes
          [ Just ("SchemaFacet" .= _baftoSchemaFacet),
            Just ("ObjectAttributeList" .= _baftoObjectAttributeList),
            Just ("ObjectReference" .= _baftoObjectReference)
          ]
      )
