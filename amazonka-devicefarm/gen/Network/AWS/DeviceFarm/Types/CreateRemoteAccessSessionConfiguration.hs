{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration where

import Network.AWS.DeviceFarm.Types.BillingMethod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration settings for a remote access session, including billing
-- method.
--
-- /See:/ 'newCreateRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { -- | The billing method for the remote access session.
    billingMethod :: Prelude.Maybe BillingMethod,
    -- | An array of ARNs included in the VPC endpoint configuration.
    vpceConfigurationArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateRemoteAccessSessionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMethod', 'createRemoteAccessSessionConfiguration_billingMethod' - The billing method for the remote access session.
--
-- 'vpceConfigurationArns', 'createRemoteAccessSessionConfiguration_vpceConfigurationArns' - An array of ARNs included in the VPC endpoint configuration.
newCreateRemoteAccessSessionConfiguration ::
  CreateRemoteAccessSessionConfiguration
newCreateRemoteAccessSessionConfiguration =
  CreateRemoteAccessSessionConfiguration'
    { billingMethod =
        Prelude.Nothing,
      vpceConfigurationArns =
        Prelude.Nothing
    }

-- | The billing method for the remote access session.
createRemoteAccessSessionConfiguration_billingMethod :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Prelude.Maybe BillingMethod)
createRemoteAccessSessionConfiguration_billingMethod = Lens.lens (\CreateRemoteAccessSessionConfiguration' {billingMethod} -> billingMethod) (\s@CreateRemoteAccessSessionConfiguration' {} a -> s {billingMethod = a} :: CreateRemoteAccessSessionConfiguration)

-- | An array of ARNs included in the VPC endpoint configuration.
createRemoteAccessSessionConfiguration_vpceConfigurationArns :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Prelude.Maybe [Prelude.Text])
createRemoteAccessSessionConfiguration_vpceConfigurationArns = Lens.lens (\CreateRemoteAccessSessionConfiguration' {vpceConfigurationArns} -> vpceConfigurationArns) (\s@CreateRemoteAccessSessionConfiguration' {} a -> s {vpceConfigurationArns = a} :: CreateRemoteAccessSessionConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.Hashable
    CreateRemoteAccessSessionConfiguration

instance
  Prelude.NFData
    CreateRemoteAccessSessionConfiguration

instance
  Prelude.ToJSON
    CreateRemoteAccessSessionConfiguration
  where
  toJSON CreateRemoteAccessSessionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("billingMethod" Prelude..=)
              Prelude.<$> billingMethod,
            ("vpceConfigurationArns" Prelude..=)
              Prelude.<$> vpceConfigurationArns
          ]
      )
