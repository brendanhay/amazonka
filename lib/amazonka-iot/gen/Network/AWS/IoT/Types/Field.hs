{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Field
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Field where

import Network.AWS.IoT.Types.FieldType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the name and data type at a field.
--
--
--
-- /See:/ 'field' smart constructor.
data Field = Field'
  { _fName :: !(Maybe Text),
    _fType :: !(Maybe FieldType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Field' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the field.
--
-- * 'fType' - The datatype of the field.
field ::
  Field
field = Field' {_fName = Nothing, _fType = Nothing}

-- | The name of the field.
fName :: Lens' Field (Maybe Text)
fName = lens _fName (\s a -> s {_fName = a})

-- | The datatype of the field.
fType :: Lens' Field (Maybe FieldType)
fType = lens _fType (\s a -> s {_fType = a})

instance FromJSON Field where
  parseJSON =
    withObject
      "Field"
      (\x -> Field' <$> (x .:? "name") <*> (x .:? "type"))

instance Hashable Field

instance NFData Field

instance ToJSON Field where
  toJSON Field' {..} =
    object
      (catMaybes [("name" .=) <$> _fName, ("type" .=) <$> _fType])
