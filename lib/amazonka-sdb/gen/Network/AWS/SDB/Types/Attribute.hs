{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Attribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aAlternateValueEncoding ::
      !(Maybe Text),
    _aAlternateNameEncoding :: !(Maybe Text),
    _aName :: !Text,
    _aValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlternateValueEncoding' -
--
-- * 'aAlternateNameEncoding' -
--
-- * 'aName' - The name of the attribute.
--
-- * 'aValue' - The value of the attribute.
attribute ::
  -- | 'aName'
  Text ->
  -- | 'aValue'
  Text ->
  Attribute
attribute pName_ pValue_ =
  Attribute'
    { _aAlternateValueEncoding = Nothing,
      _aAlternateNameEncoding = Nothing,
      _aName = pName_,
      _aValue = pValue_
    }

-- |
aAlternateValueEncoding :: Lens' Attribute (Maybe Text)
aAlternateValueEncoding = lens _aAlternateValueEncoding (\s a -> s {_aAlternateValueEncoding = a})

-- |
aAlternateNameEncoding :: Lens' Attribute (Maybe Text)
aAlternateNameEncoding = lens _aAlternateNameEncoding (\s a -> s {_aAlternateNameEncoding = a})

-- | The name of the attribute.
aName :: Lens' Attribute Text
aName = lens _aName (\s a -> s {_aName = a})

-- | The value of the attribute.
aValue :: Lens' Attribute Text
aValue = lens _aValue (\s a -> s {_aValue = a})

instance FromXML Attribute where
  parseXML x =
    Attribute'
      <$> (x .@? "AlternateValueEncoding")
      <*> (x .@? "AlternateNameEncoding")
      <*> (x .@ "Name")
      <*> (x .@ "Value")

instance Hashable Attribute

instance NFData Attribute

instance ToQuery Attribute where
  toQuery Attribute' {..} =
    mconcat
      [ "AlternateValueEncoding" =: _aAlternateValueEncoding,
        "AlternateNameEncoding" =: _aAlternateNameEncoding,
        "Name" =: _aName,
        "Value" =: _aValue
      ]
