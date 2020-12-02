{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DateArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateArrayOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a field that contains an array of dates. Present if @IndexFieldType@ specifies the field is of type @date-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'dateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { _daosSourceFields ::
      !(Maybe Text),
    _daosReturnEnabled :: !(Maybe Bool),
    _daosFacetEnabled :: !(Maybe Bool),
    _daosSearchEnabled :: !(Maybe Bool),
    _daosDefaultValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DateArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daosSourceFields' - A list of source fields to map to the field.
--
-- * 'daosReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'daosFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'daosSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'daosDefaultValue' - A value to use for the field if the field isn't specified for a document.
dateArrayOptions ::
  DateArrayOptions
dateArrayOptions =
  DateArrayOptions'
    { _daosSourceFields = Nothing,
      _daosReturnEnabled = Nothing,
      _daosFacetEnabled = Nothing,
      _daosSearchEnabled = Nothing,
      _daosDefaultValue = Nothing
    }

-- | A list of source fields to map to the field.
daosSourceFields :: Lens' DateArrayOptions (Maybe Text)
daosSourceFields = lens _daosSourceFields (\s a -> s {_daosSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
daosReturnEnabled :: Lens' DateArrayOptions (Maybe Bool)
daosReturnEnabled = lens _daosReturnEnabled (\s a -> s {_daosReturnEnabled = a})

-- | Whether facet information can be returned for the field.
daosFacetEnabled :: Lens' DateArrayOptions (Maybe Bool)
daosFacetEnabled = lens _daosFacetEnabled (\s a -> s {_daosFacetEnabled = a})

-- | Whether the contents of the field are searchable.
daosSearchEnabled :: Lens' DateArrayOptions (Maybe Bool)
daosSearchEnabled = lens _daosSearchEnabled (\s a -> s {_daosSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
daosDefaultValue :: Lens' DateArrayOptions (Maybe Text)
daosDefaultValue = lens _daosDefaultValue (\s a -> s {_daosDefaultValue = a})

instance FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      <$> (x .@? "SourceFields")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable DateArrayOptions

instance NFData DateArrayOptions

instance ToQuery DateArrayOptions where
  toQuery DateArrayOptions' {..} =
    mconcat
      [ "SourceFields" =: _daosSourceFields,
        "ReturnEnabled" =: _daosReturnEnabled,
        "FacetEnabled" =: _daosFacetEnabled,
        "SearchEnabled" =: _daosSearchEnabled,
        "DefaultValue" =: _daosDefaultValue
      ]
