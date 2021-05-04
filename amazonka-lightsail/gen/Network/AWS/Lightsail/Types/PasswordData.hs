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
-- Module      : Network.AWS.Lightsail.Types.PasswordData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PasswordData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The password data for the Windows Server-based instance, including the
-- ciphertext and the key pair name.
--
-- /See:/ 'newPasswordData' smart constructor.
data PasswordData = PasswordData'
  { -- | The name of the key pair that you used when creating your instance. If
    -- no key pair name was specified when creating the instance, Lightsail
    -- uses the default key pair (@LightsailDefaultKeyPair@).
    --
    -- If you are using a custom key pair, you need to use your own means of
    -- decrypting your password using the @ciphertext@. Lightsail creates the
    -- ciphertext by encrypting your password with the public key part of this
    -- key pair.
    keyPairName :: Prelude.Maybe Prelude.Text,
    -- | The encrypted password. Ciphertext will be an empty string if access to
    -- your new instance is not ready yet. When you create an instance, it can
    -- take up to 15 minutes for the instance to be ready.
    --
    -- If you use the default key pair (@LightsailDefaultKeyPair@), the
    -- decrypted password will be available in the password field.
    --
    -- If you are using a custom key pair, you need to use your own means of
    -- decryption.
    --
    -- If you change the Administrator password on the instance, Lightsail will
    -- continue to return the original ciphertext value. When accessing the
    -- instance using RDP, you need to manually enter the Administrator
    -- password after changing it from the default.
    ciphertext :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PasswordData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairName', 'passwordData_keyPairName' - The name of the key pair that you used when creating your instance. If
-- no key pair name was specified when creating the instance, Lightsail
-- uses the default key pair (@LightsailDefaultKeyPair@).
--
-- If you are using a custom key pair, you need to use your own means of
-- decrypting your password using the @ciphertext@. Lightsail creates the
-- ciphertext by encrypting your password with the public key part of this
-- key pair.
--
-- 'ciphertext', 'passwordData_ciphertext' - The encrypted password. Ciphertext will be an empty string if access to
-- your new instance is not ready yet. When you create an instance, it can
-- take up to 15 minutes for the instance to be ready.
--
-- If you use the default key pair (@LightsailDefaultKeyPair@), the
-- decrypted password will be available in the password field.
--
-- If you are using a custom key pair, you need to use your own means of
-- decryption.
--
-- If you change the Administrator password on the instance, Lightsail will
-- continue to return the original ciphertext value. When accessing the
-- instance using RDP, you need to manually enter the Administrator
-- password after changing it from the default.
newPasswordData ::
  PasswordData
newPasswordData =
  PasswordData'
    { keyPairName = Prelude.Nothing,
      ciphertext = Prelude.Nothing
    }

-- | The name of the key pair that you used when creating your instance. If
-- no key pair name was specified when creating the instance, Lightsail
-- uses the default key pair (@LightsailDefaultKeyPair@).
--
-- If you are using a custom key pair, you need to use your own means of
-- decrypting your password using the @ciphertext@. Lightsail creates the
-- ciphertext by encrypting your password with the public key part of this
-- key pair.
passwordData_keyPairName :: Lens.Lens' PasswordData (Prelude.Maybe Prelude.Text)
passwordData_keyPairName = Lens.lens (\PasswordData' {keyPairName} -> keyPairName) (\s@PasswordData' {} a -> s {keyPairName = a} :: PasswordData)

-- | The encrypted password. Ciphertext will be an empty string if access to
-- your new instance is not ready yet. When you create an instance, it can
-- take up to 15 minutes for the instance to be ready.
--
-- If you use the default key pair (@LightsailDefaultKeyPair@), the
-- decrypted password will be available in the password field.
--
-- If you are using a custom key pair, you need to use your own means of
-- decryption.
--
-- If you change the Administrator password on the instance, Lightsail will
-- continue to return the original ciphertext value. When accessing the
-- instance using RDP, you need to manually enter the Administrator
-- password after changing it from the default.
passwordData_ciphertext :: Lens.Lens' PasswordData (Prelude.Maybe Prelude.Text)
passwordData_ciphertext = Lens.lens (\PasswordData' {ciphertext} -> ciphertext) (\s@PasswordData' {} a -> s {ciphertext = a} :: PasswordData)

instance Prelude.FromJSON PasswordData where
  parseJSON =
    Prelude.withObject
      "PasswordData"
      ( \x ->
          PasswordData'
            Prelude.<$> (x Prelude..:? "keyPairName")
            Prelude.<*> (x Prelude..:? "ciphertext")
      )

instance Prelude.Hashable PasswordData

instance Prelude.NFData PasswordData
