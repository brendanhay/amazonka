{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration where

import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration settings for a remote access session, including billing method.
--
--
--
-- /See:/ 'createRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { _crascBillingMethod ::
      !( Maybe
           BillingMethod
       ),
    _crascVpceConfigurationARNs ::
      !( Maybe
           [Text]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRemoteAccessSessionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crascBillingMethod' - The billing method for the remote access session.
--
-- * 'crascVpceConfigurationARNs' - An array of ARNs included in the VPC endpoint configuration.
createRemoteAccessSessionConfiguration ::
  CreateRemoteAccessSessionConfiguration
createRemoteAccessSessionConfiguration =
  CreateRemoteAccessSessionConfiguration'
    { _crascBillingMethod =
        Nothing,
      _crascVpceConfigurationARNs = Nothing
    }

-- | The billing method for the remote access session.
crascBillingMethod :: Lens' CreateRemoteAccessSessionConfiguration (Maybe BillingMethod)
crascBillingMethod = lens _crascBillingMethod (\s a -> s {_crascBillingMethod = a})

-- | An array of ARNs included in the VPC endpoint configuration.
crascVpceConfigurationARNs :: Lens' CreateRemoteAccessSessionConfiguration [Text]
crascVpceConfigurationARNs = lens _crascVpceConfigurationARNs (\s a -> s {_crascVpceConfigurationARNs = a}) . _Default . _Coerce

instance Hashable CreateRemoteAccessSessionConfiguration

instance NFData CreateRemoteAccessSessionConfiguration

instance ToJSON CreateRemoteAccessSessionConfiguration where
  toJSON CreateRemoteAccessSessionConfiguration' {..} =
    object
      ( catMaybes
          [ ("billingMethod" .=) <$> _crascBillingMethod,
            ("vpceConfigurationArns" .=) <$> _crascVpceConfigurationARNs
          ]
      )
