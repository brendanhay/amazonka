{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.TextOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.TextOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for text field. Present if @IndexFieldType@ specifies the field is of type @text@ . A @text@ field is always searchable. All options are enabled by default.
--
--
--
-- /See:/ 'textOptions' smart constructor.
data TextOptions = TextOptions'
  { _toSourceField :: !(Maybe Text),
    _toReturnEnabled :: !(Maybe Bool),
    _toAnalysisScheme :: !(Maybe Text),
    _toHighlightEnabled :: !(Maybe Bool),
    _toSortEnabled :: !(Maybe Bool),
    _toDefaultValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TextOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toSourceField' - Undocumented member.
--
-- * 'toReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'toAnalysisScheme' - The name of an analysis scheme for a @text@ field.
--
-- * 'toHighlightEnabled' - Whether highlights can be returned for the field.
--
-- * 'toSortEnabled' - Whether the field can be used to sort the search results.
--
-- * 'toDefaultValue' - A value to use for the field if the field isn't specified for a document.
textOptions ::
  TextOptions
textOptions =
  TextOptions'
    { _toSourceField = Nothing,
      _toReturnEnabled = Nothing,
      _toAnalysisScheme = Nothing,
      _toHighlightEnabled = Nothing,
      _toSortEnabled = Nothing,
      _toDefaultValue = Nothing
    }

-- | Undocumented member.
toSourceField :: Lens' TextOptions (Maybe Text)
toSourceField = lens _toSourceField (\s a -> s {_toSourceField = a})

-- | Whether the contents of the field can be returned in the search results.
toReturnEnabled :: Lens' TextOptions (Maybe Bool)
toReturnEnabled = lens _toReturnEnabled (\s a -> s {_toReturnEnabled = a})

-- | The name of an analysis scheme for a @text@ field.
toAnalysisScheme :: Lens' TextOptions (Maybe Text)
toAnalysisScheme = lens _toAnalysisScheme (\s a -> s {_toAnalysisScheme = a})

-- | Whether highlights can be returned for the field.
toHighlightEnabled :: Lens' TextOptions (Maybe Bool)
toHighlightEnabled = lens _toHighlightEnabled (\s a -> s {_toHighlightEnabled = a})

-- | Whether the field can be used to sort the search results.
toSortEnabled :: Lens' TextOptions (Maybe Bool)
toSortEnabled = lens _toSortEnabled (\s a -> s {_toSortEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
toDefaultValue :: Lens' TextOptions (Maybe Text)
toDefaultValue = lens _toDefaultValue (\s a -> s {_toDefaultValue = a})

instance FromXML TextOptions where
  parseXML x =
    TextOptions'
      <$> (x .@? "SourceField")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "AnalysisScheme")
      <*> (x .@? "HighlightEnabled")
      <*> (x .@? "SortEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable TextOptions

instance NFData TextOptions

instance ToQuery TextOptions where
  toQuery TextOptions' {..} =
    mconcat
      [ "SourceField" =: _toSourceField,
        "ReturnEnabled" =: _toReturnEnabled,
        "AnalysisScheme" =: _toAnalysisScheme,
        "HighlightEnabled" =: _toHighlightEnabled,
        "SortEnabled" =: _toSortEnabled,
        "DefaultValue" =: _toDefaultValue
      ]
