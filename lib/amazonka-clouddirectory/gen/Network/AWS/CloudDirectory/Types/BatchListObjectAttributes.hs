{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributes where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectAttributes' operation.
--
--
--
-- /See:/ 'batchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
  { _bloaFacetFilter ::
      !(Maybe SchemaFacet),
    _bloaNextToken :: !(Maybe Text),
    _bloaMaxResults :: !(Maybe Nat),
    _bloaObjectReference ::
      !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloaFacetFilter' - Used to filter the list of object attributes that are associated with a certain facet.
--
-- * 'bloaNextToken' - The pagination token.
--
-- * 'bloaMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'bloaObjectReference' - Reference of the object whose attributes need to be listed.
batchListObjectAttributes ::
  -- | 'bloaObjectReference'
  ObjectReference ->
  BatchListObjectAttributes
batchListObjectAttributes pObjectReference_ =
  BatchListObjectAttributes'
    { _bloaFacetFilter = Nothing,
      _bloaNextToken = Nothing,
      _bloaMaxResults = Nothing,
      _bloaObjectReference = pObjectReference_
    }

-- | Used to filter the list of object attributes that are associated with a certain facet.
bloaFacetFilter :: Lens' BatchListObjectAttributes (Maybe SchemaFacet)
bloaFacetFilter = lens _bloaFacetFilter (\s a -> s {_bloaFacetFilter = a})

-- | The pagination token.
bloaNextToken :: Lens' BatchListObjectAttributes (Maybe Text)
bloaNextToken = lens _bloaNextToken (\s a -> s {_bloaNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
bloaMaxResults :: Lens' BatchListObjectAttributes (Maybe Natural)
bloaMaxResults = lens _bloaMaxResults (\s a -> s {_bloaMaxResults = a}) . mapping _Nat

-- | Reference of the object whose attributes need to be listed.
bloaObjectReference :: Lens' BatchListObjectAttributes ObjectReference
bloaObjectReference = lens _bloaObjectReference (\s a -> s {_bloaObjectReference = a})

instance Hashable BatchListObjectAttributes

instance NFData BatchListObjectAttributes

instance ToJSON BatchListObjectAttributes where
  toJSON BatchListObjectAttributes' {..} =
    object
      ( catMaybes
          [ ("FacetFilter" .=) <$> _bloaFacetFilter,
            ("NextToken" .=) <$> _bloaNextToken,
            ("MaxResults" .=) <$> _bloaMaxResults,
            Just ("ObjectReference" .= _bloaObjectReference)
          ]
      )
