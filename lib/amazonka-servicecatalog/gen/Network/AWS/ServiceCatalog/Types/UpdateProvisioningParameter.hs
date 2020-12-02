{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameter key-value pair used to update a provisioned product.
--
--
--
-- /See:/ 'updateProvisioningParameter' smart constructor.
data UpdateProvisioningParameter = UpdateProvisioningParameter'
  { _uppValue ::
      !(Maybe Text),
    _uppKey :: !(Maybe Text),
    _uppUsePreviousValue ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisioningParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uppValue' - The parameter value.
--
-- * 'uppKey' - The parameter key.
--
-- * 'uppUsePreviousValue' - If set to true, @Value@ is ignored and the previous parameter value is kept.
updateProvisioningParameter ::
  UpdateProvisioningParameter
updateProvisioningParameter =
  UpdateProvisioningParameter'
    { _uppValue = Nothing,
      _uppKey = Nothing,
      _uppUsePreviousValue = Nothing
    }

-- | The parameter value.
uppValue :: Lens' UpdateProvisioningParameter (Maybe Text)
uppValue = lens _uppValue (\s a -> s {_uppValue = a})

-- | The parameter key.
uppKey :: Lens' UpdateProvisioningParameter (Maybe Text)
uppKey = lens _uppKey (\s a -> s {_uppKey = a})

-- | If set to true, @Value@ is ignored and the previous parameter value is kept.
uppUsePreviousValue :: Lens' UpdateProvisioningParameter (Maybe Bool)
uppUsePreviousValue = lens _uppUsePreviousValue (\s a -> s {_uppUsePreviousValue = a})

instance FromJSON UpdateProvisioningParameter where
  parseJSON =
    withObject
      "UpdateProvisioningParameter"
      ( \x ->
          UpdateProvisioningParameter'
            <$> (x .:? "Value") <*> (x .:? "Key") <*> (x .:? "UsePreviousValue")
      )

instance Hashable UpdateProvisioningParameter

instance NFData UpdateProvisioningParameter

instance ToJSON UpdateProvisioningParameter where
  toJSON UpdateProvisioningParameter' {..} =
    object
      ( catMaybes
          [ ("Value" .=) <$> _uppValue,
            ("Key" .=) <$> _uppKey,
            ("UsePreviousValue" .=) <$> _uppUsePreviousValue
          ]
      )
