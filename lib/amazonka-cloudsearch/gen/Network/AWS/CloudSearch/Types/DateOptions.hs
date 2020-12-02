{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a date field. Dates and times are specified in UTC (Coordinated Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is of type @date@ . All options are enabled by default.
--
--
--
-- /See:/ 'dateOptions' smart constructor.
data DateOptions = DateOptions'
  { _doSourceField :: !(Maybe Text),
    _doReturnEnabled :: !(Maybe Bool),
    _doFacetEnabled :: !(Maybe Bool),
    _doSearchEnabled :: !(Maybe Bool),
    _doSortEnabled :: !(Maybe Bool),
    _doDefaultValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DateOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doSourceField' - Undocumented member.
--
-- * 'doReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'doFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'doSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'doSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'doDefaultValue' - A value to use for the field if the field isn't specified for a document.
dateOptions ::
  DateOptions
dateOptions =
  DateOptions'
    { _doSourceField = Nothing,
      _doReturnEnabled = Nothing,
      _doFacetEnabled = Nothing,
      _doSearchEnabled = Nothing,
      _doSortEnabled = Nothing,
      _doDefaultValue = Nothing
    }

-- | Undocumented member.
doSourceField :: Lens' DateOptions (Maybe Text)
doSourceField = lens _doSourceField (\s a -> s {_doSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
doReturnEnabled :: Lens' DateOptions (Maybe Bool)
doReturnEnabled = lens _doReturnEnabled (\s a -> s {_doReturnEnabled = a})

-- | Whether facet information can be returned for the field.
doFacetEnabled :: Lens' DateOptions (Maybe Bool)
doFacetEnabled = lens _doFacetEnabled (\s a -> s {_doFacetEnabled = a})

-- | Whether the contents of the field are searchable.
doSearchEnabled :: Lens' DateOptions (Maybe Bool)
doSearchEnabled = lens _doSearchEnabled (\s a -> s {_doSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
doSortEnabled :: Lens' DateOptions (Maybe Bool)
doSortEnabled = lens _doSortEnabled (\s a -> s {_doSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
doDefaultValue :: Lens' DateOptions (Maybe Text)
doDefaultValue = lens _doDefaultValue (\s a -> s {_doDefaultValue = a})

instance FromXML DateOptions where
  parseXML x =
    DateOptions'
      <$> (x .@? "SourceField")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "SortEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable DateOptions

instance NFData DateOptions

instance ToQuery DateOptions where
  toQuery DateOptions' {..} =
    mconcat
      [ "SourceField" =: _doSourceField,
        "ReturnEnabled" =: _doReturnEnabled,
        "FacetEnabled" =: _doFacetEnabled,
        "SearchEnabled" =: _doSearchEnabled,
        "SortEnabled" =: _doSortEnabled,
        "DefaultValue" =: _doDefaultValue
      ]
