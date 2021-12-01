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
-- Module      : Amazonka.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The device verifier against which it will be authenticated.
--
-- /See:/ 'newDeviceSecretVerifierConfigType' smart constructor.
data DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType'
  { -- | The password verifier.
    passwordVerifier :: Prelude.Maybe Prelude.Text,
    -- | The salt.
    salt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceSecretVerifierConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordVerifier', 'deviceSecretVerifierConfigType_passwordVerifier' - The password verifier.
--
-- 'salt', 'deviceSecretVerifierConfigType_salt' - The salt.
newDeviceSecretVerifierConfigType ::
  DeviceSecretVerifierConfigType
newDeviceSecretVerifierConfigType =
  DeviceSecretVerifierConfigType'
    { passwordVerifier =
        Prelude.Nothing,
      salt = Prelude.Nothing
    }

-- | The password verifier.
deviceSecretVerifierConfigType_passwordVerifier :: Lens.Lens' DeviceSecretVerifierConfigType (Prelude.Maybe Prelude.Text)
deviceSecretVerifierConfigType_passwordVerifier = Lens.lens (\DeviceSecretVerifierConfigType' {passwordVerifier} -> passwordVerifier) (\s@DeviceSecretVerifierConfigType' {} a -> s {passwordVerifier = a} :: DeviceSecretVerifierConfigType)

-- | The salt.
deviceSecretVerifierConfigType_salt :: Lens.Lens' DeviceSecretVerifierConfigType (Prelude.Maybe Prelude.Text)
deviceSecretVerifierConfigType_salt = Lens.lens (\DeviceSecretVerifierConfigType' {salt} -> salt) (\s@DeviceSecretVerifierConfigType' {} a -> s {salt = a} :: DeviceSecretVerifierConfigType)

instance
  Prelude.Hashable
    DeviceSecretVerifierConfigType
  where
  hashWithSalt
    salt'
    DeviceSecretVerifierConfigType' {..} =
      salt' `Prelude.hashWithSalt` salt
        `Prelude.hashWithSalt` passwordVerifier

instance
  Prelude.NFData
    DeviceSecretVerifierConfigType
  where
  rnf DeviceSecretVerifierConfigType' {..} =
    Prelude.rnf passwordVerifier
      `Prelude.seq` Prelude.rnf salt

instance Core.ToJSON DeviceSecretVerifierConfigType where
  toJSON DeviceSecretVerifierConfigType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PasswordVerifier" Core..=)
              Prelude.<$> passwordVerifier,
            ("Salt" Core..=) Prelude.<$> salt
          ]
      )
