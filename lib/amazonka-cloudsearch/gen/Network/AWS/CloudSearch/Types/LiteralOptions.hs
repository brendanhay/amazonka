{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for literal field. Present if @IndexFieldType@ specifies the field is of type @literal@ . All options are enabled by default.
--
--
--
-- /See:/ 'literalOptions' smart constructor.
data LiteralOptions = LiteralOptions'
  { _loSourceField ::
      !(Maybe Text),
    _loReturnEnabled :: !(Maybe Bool),
    _loFacetEnabled :: !(Maybe Bool),
    _loSearchEnabled :: !(Maybe Bool),
    _loSortEnabled :: !(Maybe Bool),
    _loDefaultValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LiteralOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loSourceField' - Undocumented member.
--
-- * 'loReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'loFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'loSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'loSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'loDefaultValue' - A value to use for the field if the field isn't specified for a document.
literalOptions ::
  LiteralOptions
literalOptions =
  LiteralOptions'
    { _loSourceField = Nothing,
      _loReturnEnabled = Nothing,
      _loFacetEnabled = Nothing,
      _loSearchEnabled = Nothing,
      _loSortEnabled = Nothing,
      _loDefaultValue = Nothing
    }

-- | Undocumented member.
loSourceField :: Lens' LiteralOptions (Maybe Text)
loSourceField = lens _loSourceField (\s a -> s {_loSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
loReturnEnabled :: Lens' LiteralOptions (Maybe Bool)
loReturnEnabled = lens _loReturnEnabled (\s a -> s {_loReturnEnabled = a})

-- | Whether facet information can be returned for the field.
loFacetEnabled :: Lens' LiteralOptions (Maybe Bool)
loFacetEnabled = lens _loFacetEnabled (\s a -> s {_loFacetEnabled = a})

-- | Whether the contents of the field are searchable.
loSearchEnabled :: Lens' LiteralOptions (Maybe Bool)
loSearchEnabled = lens _loSearchEnabled (\s a -> s {_loSearchEnabled = a})

-- | Whether the field can be used to sort the search results.
loSortEnabled :: Lens' LiteralOptions (Maybe Bool)
loSortEnabled = lens _loSortEnabled (\s a -> s {_loSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
loDefaultValue :: Lens' LiteralOptions (Maybe Text)
loDefaultValue = lens _loDefaultValue (\s a -> s {_loDefaultValue = a})

instance FromXML LiteralOptions where
  parseXML x =
    LiteralOptions'
      <$> (x .@? "SourceField")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "SortEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable LiteralOptions

instance NFData LiteralOptions

instance ToQuery LiteralOptions where
  toQuery LiteralOptions' {..} =
    mconcat
      [ "SourceField" =: _loSourceField,
        "ReturnEnabled" =: _loReturnEnabled,
        "FacetEnabled" =: _loFacetEnabled,
        "SearchEnabled" =: _loSearchEnabled,
        "SortEnabled" =: _loSortEnabled,
        "DefaultValue" =: _loDefaultValue
      ]
