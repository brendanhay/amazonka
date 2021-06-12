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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.BillingMethod
import qualified Network.AWS.Lens as Lens

-- | Configuration settings for a remote access session, including billing
-- method.
--
-- /See:/ 'newCreateRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { -- | The billing method for the remote access session.
    billingMethod :: Core.Maybe BillingMethod,
    -- | An array of ARNs included in the VPC endpoint configuration.
    vpceConfigurationArns :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      vpceConfigurationArns =
        Core.Nothing
    }

-- | The billing method for the remote access session.
createRemoteAccessSessionConfiguration_billingMethod :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Core.Maybe BillingMethod)
createRemoteAccessSessionConfiguration_billingMethod = Lens.lens (\CreateRemoteAccessSessionConfiguration' {billingMethod} -> billingMethod) (\s@CreateRemoteAccessSessionConfiguration' {} a -> s {billingMethod = a} :: CreateRemoteAccessSessionConfiguration)

-- | An array of ARNs included in the VPC endpoint configuration.
createRemoteAccessSessionConfiguration_vpceConfigurationArns :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Core.Maybe [Core.Text])
createRemoteAccessSessionConfiguration_vpceConfigurationArns = Lens.lens (\CreateRemoteAccessSessionConfiguration' {vpceConfigurationArns} -> vpceConfigurationArns) (\s@CreateRemoteAccessSessionConfiguration' {} a -> s {vpceConfigurationArns = a} :: CreateRemoteAccessSessionConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.Hashable
    CreateRemoteAccessSessionConfiguration

instance
  Core.NFData
    CreateRemoteAccessSessionConfiguration

instance
  Core.ToJSON
    CreateRemoteAccessSessionConfiguration
  where
  toJSON CreateRemoteAccessSessionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingMethod" Core..=) Core.<$> billingMethod,
            ("vpceConfigurationArns" Core..=)
              Core.<$> vpceConfigurationArns
          ]
      )
