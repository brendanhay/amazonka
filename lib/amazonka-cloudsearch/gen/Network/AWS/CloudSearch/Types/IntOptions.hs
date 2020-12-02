{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a 64-bit signed integer field. Present if @IndexFieldType@ specifies the field is of type @int@ . All options are enabled by default.
--
--
--
-- /See:/ 'intOptions' smart constructor.
data IntOptions = IntOptions'
  { _ioSourceField :: !(Maybe Text),
    _ioReturnEnabled :: !(Maybe Bool),
    _ioFacetEnabled :: !(Maybe Bool),
    _ioSearchEnabled :: !(Maybe Bool),
    _ioSortEnabled :: !(Maybe Bool),
    _ioDefaultValue :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ioSourceField' - The name of the source field to map to the field.
--
-- * 'ioReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'ioFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'ioSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'ioSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'ioDefaultValue' - A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
intOptions ::
  IntOptions
intOptions =
  IntOptions'
    { _ioSourceField = Nothing,
      _ioReturnEnabled = Nothing,
      _ioFacetEnabled = Nothing,
      _ioSearchEnabled = Nothing,
      _ioSortEnabled = Nothing,
      _ioDefaultValue = Nothing
    }

-- | The name of the source field to map to the field.
ioSourceField :: Lens' IntOptions (Maybe Text)
ioSourceField = lens _ioSourceField (\s a -> s {_ioSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
ioReturnEnabled :: Lens' IntOptions (Maybe Bool)
ioReturnEnabled = lens _ioReturnEnabled (\s a -> s {_ioReturnEnabled = a})

-- | Whether facet information can be returned for the field.
ioFacetEnabled :: Lens' IntOptions (Maybe Bool)
ioFacetEnabled = lens _ioFacetEnabled (\s a -> s {_ioFacetEnabled = a})

-- | Whether the contents of the field are searchable.
ioSearchEnabled :: Lens' IntOptions (Maybe Bool)
ioSearchEnabled = lens _ioSearchEnabled (\s a -> s {_ioSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
ioSortEnabled :: Lens' IntOptions (Maybe Bool)
ioSortEnabled = lens _ioSortEnabled (\s a -> s {_ioSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
ioDefaultValue :: Lens' IntOptions (Maybe Integer)
ioDefaultValue = lens _ioDefaultValue (\s a -> s {_ioDefaultValue = a})

instance FromXML IntOptions where
  parseXML x =
    IntOptions'
      <$> (x .@? "SourceField")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "SortEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable IntOptions

instance NFData IntOptions

instance ToQuery IntOptions where
  toQuery IntOptions' {..} =
    mconcat
      [ "SourceField" =: _ioSourceField,
        "ReturnEnabled" =: _ioReturnEnabled,
        "FacetEnabled" =: _ioFacetEnabled,
        "SearchEnabled" =: _ioSearchEnabled,
        "SortEnabled" =: _ioSortEnabled,
        "DefaultValue" =: _ioDefaultValue
      ]
