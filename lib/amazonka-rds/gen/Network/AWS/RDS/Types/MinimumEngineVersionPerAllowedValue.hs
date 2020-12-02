{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The minimum DB engine version required for each corresponding allowed value for an option setting.
--
--
--
-- /See:/ 'minimumEngineVersionPerAllowedValue' smart constructor.
data MinimumEngineVersionPerAllowedValue = MinimumEngineVersionPerAllowedValue'
  { _mevpavMinimumEngineVersion ::
      !(Maybe Text),
    _mevpavAllowedValue ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MinimumEngineVersionPerAllowedValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mevpavMinimumEngineVersion' - The minimum DB engine version required for the allowed value.
--
-- * 'mevpavAllowedValue' - The allowed value for an option setting.
minimumEngineVersionPerAllowedValue ::
  MinimumEngineVersionPerAllowedValue
minimumEngineVersionPerAllowedValue =
  MinimumEngineVersionPerAllowedValue'
    { _mevpavMinimumEngineVersion =
        Nothing,
      _mevpavAllowedValue = Nothing
    }

-- | The minimum DB engine version required for the allowed value.
mevpavMinimumEngineVersion :: Lens' MinimumEngineVersionPerAllowedValue (Maybe Text)
mevpavMinimumEngineVersion = lens _mevpavMinimumEngineVersion (\s a -> s {_mevpavMinimumEngineVersion = a})

-- | The allowed value for an option setting.
mevpavAllowedValue :: Lens' MinimumEngineVersionPerAllowedValue (Maybe Text)
mevpavAllowedValue = lens _mevpavAllowedValue (\s a -> s {_mevpavAllowedValue = a})

instance FromXML MinimumEngineVersionPerAllowedValue where
  parseXML x =
    MinimumEngineVersionPerAllowedValue'
      <$> (x .@? "MinimumEngineVersion") <*> (x .@? "AllowedValue")

instance Hashable MinimumEngineVersionPerAllowedValue

instance NFData MinimumEngineVersionPerAllowedValue
