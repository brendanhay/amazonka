{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntArrayOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Options for a field that contains an array of 64-bit signed integers. Present if @IndexFieldType@ specifies the field is of type @int-array@ . All options are enabled by default.
--
--
--
-- /See:/ 'intArrayOptions' smart constructor.
data IntArrayOptions = IntArrayOptions'
  { _iaoSourceFields ::
      !(Maybe Text),
    _iaoReturnEnabled :: !(Maybe Bool),
    _iaoFacetEnabled :: !(Maybe Bool),
    _iaoSearchEnabled :: !(Maybe Bool),
    _iaoDefaultValue :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntArrayOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaoSourceFields' - A list of source fields to map to the field.
--
-- * 'iaoReturnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- * 'iaoFacetEnabled' - Whether facet information can be returned for the field.
--
-- * 'iaoSearchEnabled' - Whether the contents of the field are searchable.
--
-- * 'iaoDefaultValue' - A value to use for the field if the field isn't specified for a document.
intArrayOptions ::
  IntArrayOptions
intArrayOptions =
  IntArrayOptions'
    { _iaoSourceFields = Nothing,
      _iaoReturnEnabled = Nothing,
      _iaoFacetEnabled = Nothing,
      _iaoSearchEnabled = Nothing,
      _iaoDefaultValue = Nothing
    }

-- | A list of source fields to map to the field.
iaoSourceFields :: Lens' IntArrayOptions (Maybe Text)
iaoSourceFields = lens _iaoSourceFields (\s a -> s {_iaoSourceFields = a})

-- | Whether the contents of the field can be returned in the search results.
iaoReturnEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoReturnEnabled = lens _iaoReturnEnabled (\s a -> s {_iaoReturnEnabled = a})

-- | Whether facet information can be returned for the field.
iaoFacetEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoFacetEnabled = lens _iaoFacetEnabled (\s a -> s {_iaoFacetEnabled = a})

-- | Whether the contents of the field are searchable.
iaoSearchEnabled :: Lens' IntArrayOptions (Maybe Bool)
iaoSearchEnabled = lens _iaoSearchEnabled (\s a -> s {_iaoSearchEnabled = a})

-- | A value to use for the field if the field isn't specified for a document.
iaoDefaultValue :: Lens' IntArrayOptions (Maybe Integer)
iaoDefaultValue = lens _iaoDefaultValue (\s a -> s {_iaoDefaultValue = a})

instance FromXML IntArrayOptions where
  parseXML x =
    IntArrayOptions'
      <$> (x .@? "SourceFields")
      <*> (x .@? "ReturnEnabled")
      <*> (x .@? "FacetEnabled")
      <*> (x .@? "SearchEnabled")
      <*> (x .@? "DefaultValue")

instance Hashable IntArrayOptions

instance NFData IntArrayOptions

instance ToQuery IntArrayOptions where
  toQuery IntArrayOptions' {..} =
    mconcat
      [ "SourceFields" =: _iaoSourceFields,
        "ReturnEnabled" =: _iaoReturnEnabled,
        "FacetEnabled" =: _iaoFacetEnabled,
        "SearchEnabled" =: _iaoSearchEnabled,
        "DefaultValue" =: _iaoDefaultValue
      ]
