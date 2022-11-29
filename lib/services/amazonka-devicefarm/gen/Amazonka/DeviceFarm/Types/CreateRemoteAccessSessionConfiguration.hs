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
-- Module      : Amazonka.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types.BillingMethod
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for a remote access session, including billing
-- method.
--
-- /See:/ 'newCreateRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { -- | An array of ARNs included in the VPC endpoint configuration.
    vpceConfigurationArns :: Prelude.Maybe [Prelude.Text],
    -- | The billing method for the remote access session.
    billingMethod :: Prelude.Maybe BillingMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRemoteAccessSessionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfigurationArns', 'createRemoteAccessSessionConfiguration_vpceConfigurationArns' - An array of ARNs included in the VPC endpoint configuration.
--
-- 'billingMethod', 'createRemoteAccessSessionConfiguration_billingMethod' - The billing method for the remote access session.
newCreateRemoteAccessSessionConfiguration ::
  CreateRemoteAccessSessionConfiguration
newCreateRemoteAccessSessionConfiguration =
  CreateRemoteAccessSessionConfiguration'
    { vpceConfigurationArns =
        Prelude.Nothing,
      billingMethod = Prelude.Nothing
    }

-- | An array of ARNs included in the VPC endpoint configuration.
createRemoteAccessSessionConfiguration_vpceConfigurationArns :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Prelude.Maybe [Prelude.Text])
createRemoteAccessSessionConfiguration_vpceConfigurationArns = Lens.lens (\CreateRemoteAccessSessionConfiguration' {vpceConfigurationArns} -> vpceConfigurationArns) (\s@CreateRemoteAccessSessionConfiguration' {} a -> s {vpceConfigurationArns = a} :: CreateRemoteAccessSessionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The billing method for the remote access session.
createRemoteAccessSessionConfiguration_billingMethod :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Prelude.Maybe BillingMethod)
createRemoteAccessSessionConfiguration_billingMethod = Lens.lens (\CreateRemoteAccessSessionConfiguration' {billingMethod} -> billingMethod) (\s@CreateRemoteAccessSessionConfiguration' {} a -> s {billingMethod = a} :: CreateRemoteAccessSessionConfiguration)

instance
  Prelude.Hashable
    CreateRemoteAccessSessionConfiguration
  where
  hashWithSalt
    _salt
    CreateRemoteAccessSessionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` vpceConfigurationArns
        `Prelude.hashWithSalt` billingMethod

instance
  Prelude.NFData
    CreateRemoteAccessSessionConfiguration
  where
  rnf CreateRemoteAccessSessionConfiguration' {..} =
    Prelude.rnf vpceConfigurationArns
      `Prelude.seq` Prelude.rnf billingMethod

instance
  Core.ToJSON
    CreateRemoteAccessSessionConfiguration
  where
  toJSON CreateRemoteAccessSessionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vpceConfigurationArns" Core..=)
              Prelude.<$> vpceConfigurationArns,
            ("billingMethod" Core..=) Prelude.<$> billingMethod
          ]
      )
