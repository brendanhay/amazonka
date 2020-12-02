{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a parameter used to provision a product.
--
--
--
-- /See:/ 'provisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { _ppValue ::
      !(Maybe Text),
    _ppKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppValue' - The parameter value.
--
-- * 'ppKey' - The parameter key.
provisioningParameter ::
  ProvisioningParameter
provisioningParameter =
  ProvisioningParameter' {_ppValue = Nothing, _ppKey = Nothing}

-- | The parameter value.
ppValue :: Lens' ProvisioningParameter (Maybe Text)
ppValue = lens _ppValue (\s a -> s {_ppValue = a})

-- | The parameter key.
ppKey :: Lens' ProvisioningParameter (Maybe Text)
ppKey = lens _ppKey (\s a -> s {_ppKey = a})

instance Hashable ProvisioningParameter

instance NFData ProvisioningParameter

instance ToJSON ProvisioningParameter where
  toJSON ProvisioningParameter' {..} =
    object
      (catMaybes [("Value" .=) <$> _ppValue, ("Key" .=) <$> _ppKey])
