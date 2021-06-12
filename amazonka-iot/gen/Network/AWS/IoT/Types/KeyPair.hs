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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a key pair.
--
-- /See:/ 'newKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The public key.
    publicKey :: Core.Maybe Core.Text,
    -- | The private key.
    privateKey :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
    { publicKey = Core.Nothing,
      privateKey = Core.Nothing
    }

-- | The public key.
keyPair_publicKey :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
keyPair_publicKey = Lens.lens (\KeyPair' {publicKey} -> publicKey) (\s@KeyPair' {} a -> s {publicKey = a} :: KeyPair)

-- | The private key.
keyPair_privateKey :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
keyPair_privateKey = Lens.lens (\KeyPair' {privateKey} -> privateKey) (\s@KeyPair' {} a -> s {privateKey = a} :: KeyPair) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON KeyPair where
  parseJSON =
    Core.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Core.<$> (x Core..:? "PublicKey")
            Core.<*> (x Core..:? "PrivateKey")
      )

instance Core.Hashable KeyPair

instance Core.NFData KeyPair
