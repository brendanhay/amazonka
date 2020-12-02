{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexField where

import Network.AWS.CloudSearch.Types.DateArrayOptions
import Network.AWS.CloudSearch.Types.DateOptions
import Network.AWS.CloudSearch.Types.DoubleArrayOptions
import Network.AWS.CloudSearch.Types.DoubleOptions
import Network.AWS.CloudSearch.Types.IndexFieldType
import Network.AWS.CloudSearch.Types.IntArrayOptions
import Network.AWS.CloudSearch.Types.IntOptions
import Network.AWS.CloudSearch.Types.LatLonOptions
import Network.AWS.CloudSearch.Types.LiteralArrayOptions
import Network.AWS.CloudSearch.Types.LiteralOptions
import Network.AWS.CloudSearch.Types.TextArrayOptions
import Network.AWS.CloudSearch.Types.TextOptions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for a field in the index, including its name, type, and options. The supported options depend on the @'IndexFieldType' @ .
--
--
--
-- /See:/ 'indexField' smart constructor.
data IndexField = IndexField'
  { _ifDoubleArrayOptions ::
      !(Maybe DoubleArrayOptions),
    _ifDateOptions :: !(Maybe DateOptions),
    _ifTextArrayOptions :: !(Maybe TextArrayOptions),
    _ifDoubleOptions :: !(Maybe DoubleOptions),
    _ifTextOptions :: !(Maybe TextOptions),
    _ifLatLonOptions :: !(Maybe LatLonOptions),
    _ifLiteralArrayOptions :: !(Maybe LiteralArrayOptions),
    _ifIntArrayOptions :: !(Maybe IntArrayOptions),
    _ifDateArrayOptions :: !(Maybe DateArrayOptions),
    _ifIntOptions :: !(Maybe IntOptions),
    _ifLiteralOptions :: !(Maybe LiteralOptions),
    _ifIndexFieldName :: !Text,
    _ifIndexFieldType :: !IndexFieldType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IndexField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifDoubleArrayOptions' - Undocumented member.
--
-- * 'ifDateOptions' - Undocumented member.
--
-- * 'ifTextArrayOptions' - Undocumented member.
--
-- * 'ifDoubleOptions' - Undocumented member.
--
-- * 'ifTextOptions' - Undocumented member.
--
-- * 'ifLatLonOptions' - Undocumented member.
--
-- * 'ifLiteralArrayOptions' - Undocumented member.
--
-- * 'ifIntArrayOptions' - Undocumented member.
--
-- * 'ifDateArrayOptions' - Undocumented member.
--
-- * 'ifIntOptions' - Undocumented member.
--
-- * 'ifLiteralOptions' - Undocumented member.
--
-- * 'ifIndexFieldName' - A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.  Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.  The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
--
-- * 'ifIndexFieldType' - Undocumented member.
indexField ::
  -- | 'ifIndexFieldName'
  Text ->
  -- | 'ifIndexFieldType'
  IndexFieldType ->
  IndexField
indexField pIndexFieldName_ pIndexFieldType_ =
  IndexField'
    { _ifDoubleArrayOptions = Nothing,
      _ifDateOptions = Nothing,
      _ifTextArrayOptions = Nothing,
      _ifDoubleOptions = Nothing,
      _ifTextOptions = Nothing,
      _ifLatLonOptions = Nothing,
      _ifLiteralArrayOptions = Nothing,
      _ifIntArrayOptions = Nothing,
      _ifDateArrayOptions = Nothing,
      _ifIntOptions = Nothing,
      _ifLiteralOptions = Nothing,
      _ifIndexFieldName = pIndexFieldName_,
      _ifIndexFieldType = pIndexFieldType_
    }

-- | Undocumented member.
ifDoubleArrayOptions :: Lens' IndexField (Maybe DoubleArrayOptions)
ifDoubleArrayOptions = lens _ifDoubleArrayOptions (\s a -> s {_ifDoubleArrayOptions = a})

-- | Undocumented member.
ifDateOptions :: Lens' IndexField (Maybe DateOptions)
ifDateOptions = lens _ifDateOptions (\s a -> s {_ifDateOptions = a})

-- | Undocumented member.
ifTextArrayOptions :: Lens' IndexField (Maybe TextArrayOptions)
ifTextArrayOptions = lens _ifTextArrayOptions (\s a -> s {_ifTextArrayOptions = a})

-- | Undocumented member.
ifDoubleOptions :: Lens' IndexField (Maybe DoubleOptions)
ifDoubleOptions = lens _ifDoubleOptions (\s a -> s {_ifDoubleOptions = a})

-- | Undocumented member.
ifTextOptions :: Lens' IndexField (Maybe TextOptions)
ifTextOptions = lens _ifTextOptions (\s a -> s {_ifTextOptions = a})

-- | Undocumented member.
ifLatLonOptions :: Lens' IndexField (Maybe LatLonOptions)
ifLatLonOptions = lens _ifLatLonOptions (\s a -> s {_ifLatLonOptions = a})

-- | Undocumented member.
ifLiteralArrayOptions :: Lens' IndexField (Maybe LiteralArrayOptions)
ifLiteralArrayOptions = lens _ifLiteralArrayOptions (\s a -> s {_ifLiteralArrayOptions = a})

-- | Undocumented member.
ifIntArrayOptions :: Lens' IndexField (Maybe IntArrayOptions)
ifIntArrayOptions = lens _ifIntArrayOptions (\s a -> s {_ifIntArrayOptions = a})

-- | Undocumented member.
ifDateArrayOptions :: Lens' IndexField (Maybe DateArrayOptions)
ifDateArrayOptions = lens _ifDateArrayOptions (\s a -> s {_ifDateArrayOptions = a})

-- | Undocumented member.
ifIntOptions :: Lens' IndexField (Maybe IntOptions)
ifIntOptions = lens _ifIntOptions (\s a -> s {_ifIntOptions = a})

-- | Undocumented member.
ifLiteralOptions :: Lens' IndexField (Maybe LiteralOptions)
ifLiteralOptions = lens _ifLiteralOptions (\s a -> s {_ifLiteralOptions = a})

-- | A string that represents the name of an index field. CloudSearch supports regular index fields as well as dynamic fields. A dynamic field's name defines a pattern that begins or ends with a wildcard. Any document fields that don't map to a regular index field but do match a dynamic field's pattern are configured with the dynamic field's indexing options.  Regular field names begin with a letter and can contain the following characters: a-z (lowercase), 0-9, and _ (underscore). Dynamic field names must begin or end with a wildcard (*). The wildcard can also be the only character in a dynamic field name. Multiple wildcards, and wildcards embedded within a string are not supported.  The name @score@ is reserved and cannot be used as a field name. To reference a document's ID, you can use the name @_id@ .
ifIndexFieldName :: Lens' IndexField Text
ifIndexFieldName = lens _ifIndexFieldName (\s a -> s {_ifIndexFieldName = a})

-- | Undocumented member.
ifIndexFieldType :: Lens' IndexField IndexFieldType
ifIndexFieldType = lens _ifIndexFieldType (\s a -> s {_ifIndexFieldType = a})

instance FromXML IndexField where
  parseXML x =
    IndexField'
      <$> (x .@? "DoubleArrayOptions")
      <*> (x .@? "DateOptions")
      <*> (x .@? "TextArrayOptions")
      <*> (x .@? "DoubleOptions")
      <*> (x .@? "TextOptions")
      <*> (x .@? "LatLonOptions")
      <*> (x .@? "LiteralArrayOptions")
      <*> (x .@? "IntArrayOptions")
      <*> (x .@? "DateArrayOptions")
      <*> (x .@? "IntOptions")
      <*> (x .@? "LiteralOptions")
      <*> (x .@ "IndexFieldName")
      <*> (x .@ "IndexFieldType")

instance Hashable IndexField

instance NFData IndexField

instance ToQuery IndexField where
  toQuery IndexField' {..} =
    mconcat
      [ "DoubleArrayOptions" =: _ifDoubleArrayOptions,
        "DateOptions" =: _ifDateOptions,
        "TextArrayOptions" =: _ifTextArrayOptions,
        "DoubleOptions" =: _ifDoubleOptions,
        "TextOptions" =: _ifTextOptions,
        "LatLonOptions" =: _ifLatLonOptions,
        "LiteralArrayOptions" =: _ifLiteralArrayOptions,
        "IntArrayOptions" =: _ifIntArrayOptions,
        "DateArrayOptions" =: _ifDateArrayOptions,
        "IntOptions" =: _ifIntOptions,
        "LiteralOptions" =: _ifLiteralOptions,
        "IndexFieldName" =: _ifIndexFieldName,
        "IndexFieldType" =: _ifIndexFieldType
      ]
