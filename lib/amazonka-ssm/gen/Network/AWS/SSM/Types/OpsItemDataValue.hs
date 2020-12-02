{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemDataValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemDataValue where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OpsItemDataType

-- | An object that defines the value of the key and its type in the OperationalData map.
--
--
--
-- /See:/ 'opsItemDataValue' smart constructor.
data OpsItemDataValue = OpsItemDataValue'
  { _oidvValue ::
      !(Maybe Text),
    _oidvType :: !(Maybe OpsItemDataType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsItemDataValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oidvValue' - The value of the OperationalData key.
--
-- * 'oidvType' - The type of key-value pair. Valid types include @SearchableString@ and @String@ .
opsItemDataValue ::
  OpsItemDataValue
opsItemDataValue =
  OpsItemDataValue' {_oidvValue = Nothing, _oidvType = Nothing}

-- | The value of the OperationalData key.
oidvValue :: Lens' OpsItemDataValue (Maybe Text)
oidvValue = lens _oidvValue (\s a -> s {_oidvValue = a})

-- | The type of key-value pair. Valid types include @SearchableString@ and @String@ .
oidvType :: Lens' OpsItemDataValue (Maybe OpsItemDataType)
oidvType = lens _oidvType (\s a -> s {_oidvType = a})

instance FromJSON OpsItemDataValue where
  parseJSON =
    withObject
      "OpsItemDataValue"
      (\x -> OpsItemDataValue' <$> (x .:? "Value") <*> (x .:? "Type"))

instance Hashable OpsItemDataValue

instance NFData OpsItemDataValue

instance ToJSON OpsItemDataValue where
  toJSON OpsItemDataValue' {..} =
    object
      ( catMaybes
          [("Value" .=) <$> _oidvValue, ("Type" .=) <$> _oidvType]
      )
