{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralArrayOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a field that contains an array of literal strings. Present if @IndexFieldType@ specifies the field is of type @literal-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'literalArrayOptions' smart constructor.
data LiteralArrayOptions = LiteralArrayOptions'
  { _laoSourceFields ::
      !(Maybe Text),
    _laoReturnEnabled :: !(Maybe Bool),
    _laoFacetEnabled :: !(Maybe Bool),
    _laoSearchEnabled :: !(Maybe Bool),
    _laoDefaultValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LiteralArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laoSourceFields' - A list of source fields to map to the field.
--
-- * 'laoReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'laoFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'laoSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'laoDefaultValue' - A value to use for the field if the field isn't specified for a document.
literalArrayOptions ::
  LiteralArrayOptions
literalArrayOptions =
  LiteralArrayOptions'
    { _laoSourceFields = Nothing,
      _laoReturnEnabled = Nothing,
      _laoFacetEnabled = Nothing,
      _laoSearchEnabled = Nothing,
      _laoDefaultValue = Nothing
    }

-- | A list of source fields to map to the field.
laoSourceFields :: Lens' LiteralArrayOptions (Maybe Text)
laoSourceFields = lens _laoSourceFields (\s a -> s {_laoSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
laoReturnEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoReturnEnabled = lens _laoReturnEnabled (\s a -> s {_laoReturnEnabled = a})

-- | Whether facet information can be returned for the field.
laoFacetEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoFacetEnabled = lens _laoFacetEnabled (\s a -> s {_laoFacetEnabled = a})

-- | Whether the contents of the field are searchable.
laoSearchEnabled :: Lens' LiteralArrayOptions (Maybe Bool)
laoSearchEnabled = lens _laoSearchEnabled (\s a -> s {_laoSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
laoDefaultValue :: Lens' LiteralArrayOptions (Maybe Text)
laoDefaultValue = lens _laoDefaultValue (\s a -> s {_laoDefaultValue = a})

instance FromXML LiteralArrayOptions where
  parseXML x =
    LiteralArrayOptions'
      <$> (x .@? "SourceFields")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable LiteralArrayOptions

instance NFData LiteralArrayOptions

instance ToQuery LiteralArrayOptions where
  toQuery LiteralArrayOptions' {..} =
    mconcat
      [ "SourceFields" =: _laoSourceFields,
        "ReturnEnabled" =: _laoReturnEnabled,
        "FacetEnabled" =: _laoFacetEnabled,
        "SearchEnabled" =: _laoSearchEnabled,
        "DefaultValue" =: _laoDefaultValue
      ]
