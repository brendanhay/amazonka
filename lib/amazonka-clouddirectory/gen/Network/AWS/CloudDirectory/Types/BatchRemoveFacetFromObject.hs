{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A batch operation to remove a facet from an object.
--
--
--
-- /See:/ 'batchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
  { _brffoSchemaFacet ::
      !SchemaFacet,
    _brffoObjectReference ::
      !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchRemoveFacetFromObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brffoSchemaFacet' - The facet to remove from the object.
--
-- * 'brffoObjectReference' - A reference to the object whose facet will be removed.
batchRemoveFacetFromObject ::
  -- | 'brffoSchemaFacet'
  SchemaFacet ->
  -- | 'brffoObjectReference'
  ObjectReference ->
  BatchRemoveFacetFromObject
batchRemoveFacetFromObject pSchemaFacet_ pObjectReference_ =
  BatchRemoveFacetFromObject'
    { _brffoSchemaFacet = pSchemaFacet_,
      _brffoObjectReference = pObjectReference_
    }

-- | The facet to remove from the object.
brffoSchemaFacet :: Lens' BatchRemoveFacetFromObject SchemaFacet
brffoSchemaFacet = lens _brffoSchemaFacet (\s a -> s {_brffoSchemaFacet = a})

-- | A reference to the object whose facet will be removed.
brffoObjectReference :: Lens' BatchRemoveFacetFromObject ObjectReference
brffoObjectReference = lens _brffoObjectReference (\s a -> s {_brffoObjectReference = a})

instance Hashable BatchRemoveFacetFromObject

instance NFData BatchRemoveFacetFromObject

instance ToJSON BatchRemoveFacetFromObject where
  toJSON BatchRemoveFacetFromObject' {..} =
    object
      ( catMaybes
          [ Just ("SchemaFacet" .= _brffoSchemaFacet),
            Just ("ObjectReference" .= _brffoObjectReference)
          ]
      )
