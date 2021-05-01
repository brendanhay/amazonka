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
-- Module      : Network.AWS.IoT.Types.KeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.KeyPair where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a key pair.
--
-- /See:/ 'newKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The public key.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | The private key.
    privateKey :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKey', 'keyPair_publicKey' - The public key.
--
-- 'privateKey', 'keyPair_privateKey' - The private key.
newKeyPair ::
  KeyPair
newKeyPair =
  KeyPair'
    { publicKey = Prelude.Nothing,
      privateKey = Prelude.Nothing
    }

-- | The public key.
keyPair_publicKey :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_publicKey = Lens.lens (\KeyPair' {publicKey} -> publicKey) (\s@KeyPair' {} a -> s {publicKey = a} :: KeyPair)

-- | The private key.
keyPair_privateKey :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_privateKey = Lens.lens (\KeyPair' {privateKey} -> privateKey) (\s@KeyPair' {} a -> s {privateKey = a} :: KeyPair) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromJSON KeyPair where
  parseJSON =
    Prelude.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Prelude.<$> (x Prelude..:? "PublicKey")
            Prelude.<*> (x Prelude..:? "PrivateKey")
      )

instance Prelude.Hashable KeyPair

instance Prelude.NFData KeyPair
