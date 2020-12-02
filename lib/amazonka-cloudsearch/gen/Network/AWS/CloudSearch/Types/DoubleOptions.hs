{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DoubleOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DoubleOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a double-precision 64-bit floating point field. Present if @IndexFieldType@ specifies the field is of type @double@ . All options are enabled by default.
--
--
--
-- /See:/ 'doubleOptions' smart constructor.
data DoubleOptions = DoubleOptions'
  { _dSourceField :: !(Maybe Text),
    _dReturnEnabled :: !(Maybe Bool),
    _dFacetEnabled :: !(Maybe Bool),
    _dSearchEnabled :: !(Maybe Bool),
    _dSortEnabled :: !(Maybe Bool),
    _dDefaultValue :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DoubleOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSourceField' - The name of the source field to map to the field.
--
-- * 'dReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'dFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'dSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'dSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'dDefaultValue' - A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
doubleOptions ::
  DoubleOptions
doubleOptions =
  DoubleOptions'
    { _dSourceField = Nothing,
      _dReturnEnabled = Nothing,
      _dFacetEnabled = Nothing,
      _dSearchEnabled = Nothing,
      _dSortEnabled = Nothing,
      _dDefaultValue = Nothing
    }

-- | The name of the source field to map to the field.
dSourceField :: Lens' DoubleOptions (Maybe Text)
dSourceField = lens _dSourceField (\s a -> s {_dSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
dReturnEnabled :: Lens' DoubleOptions (Maybe Bool)
dReturnEnabled = lens _dReturnEnabled (\s a -> s {_dReturnEnabled = a})

-- | Whether facet information can be returned for the field.
dFacetEnabled :: Lens' DoubleOptions (Maybe Bool)
dFacetEnabled = lens _dFacetEnabled (\s a -> s {_dFacetEnabled = a})

-- | Whether the contents of the field are searchable.
dSearchEnabled :: Lens' DoubleOptions (Maybe Bool)
dSearchEnabled = lens _dSearchEnabled (\s a -> s {_dSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
dSortEnabled :: Lens' DoubleOptions (Maybe Bool)
dSortEnabled = lens _dSortEnabled (\s a -> s {_dSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
dDefaultValue :: Lens' DoubleOptions (Maybe Double)
dDefaultValue = lens _dDefaultValue (\s a -> s {_dDefaultValue = a})

instance FromXML DoubleOptions where
  parseXML x =
    DoubleOptions'
      <$> (x .@? "SourceField")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "SortEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable DoubleOptions

instance NFData DoubleOptions

instance ToQuery DoubleOptions where
  toQuery DoubleOptions' {..} =
    mconcat
      [ "SourceField" =: _dSourceField,
        "ReturnEnabled" =: _dReturnEnabled,
        "FacetEnabled" =: _dFacetEnabled,
        "SearchEnabled" =: _dSearchEnabled,
        "SortEnabled" =: _dSortEnabled,
        "DefaultValue" =: _dDefaultValue
      ]
