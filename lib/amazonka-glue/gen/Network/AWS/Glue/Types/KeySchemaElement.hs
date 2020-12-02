{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.KeySchemaElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.KeySchemaElement where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A partition key pair consisting of a name and a type.
--
--
--
-- /See:/ 'keySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { _kseName :: !Text,
    _kseType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeySchemaElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kseName' - The name of a partition key.
--
-- * 'kseType' - The type of a partition key.
keySchemaElement ::
  -- | 'kseName'
  Text ->
  -- | 'kseType'
  Text ->
  KeySchemaElement
keySchemaElement pName_ pType_ =
  KeySchemaElement' {_kseName = pName_, _kseType = pType_}

-- | The name of a partition key.
kseName :: Lens' KeySchemaElement Text
kseName = lens _kseName (\s a -> s {_kseName = a})

-- | The type of a partition key.
kseType :: Lens' KeySchemaElement Text
kseType = lens _kseType (\s a -> s {_kseType = a})

instance FromJSON KeySchemaElement where
  parseJSON =
    withObject
      "KeySchemaElement"
      (\x -> KeySchemaElement' <$> (x .: "Name") <*> (x .: "Type"))

instance Hashable KeySchemaElement

instance NFData KeySchemaElement
