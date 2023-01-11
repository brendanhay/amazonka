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
-- Module      : Amazonka.Signer.Types.SigningConfigurationOverrides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningConfigurationOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.EncryptionAlgorithm
import Amazonka.Signer.Types.HashAlgorithm

-- | A signing configuration that overrides the default encryption or hash
-- algorithm of a signing job.
--
-- /See:/ 'newSigningConfigurationOverrides' smart constructor.
data SigningConfigurationOverrides = SigningConfigurationOverrides'
  { -- | A specified override of the default encryption algorithm that is used in
    -- a code signing job.
    encryptionAlgorithm :: Prelude.Maybe EncryptionAlgorithm,
    -- | A specified override of the default hash algorithm that is used in a
    -- code signing job.
    hashAlgorithm :: Prelude.Maybe HashAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningConfigurationOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionAlgorithm', 'signingConfigurationOverrides_encryptionAlgorithm' - A specified override of the default encryption algorithm that is used in
-- a code signing job.
--
-- 'hashAlgorithm', 'signingConfigurationOverrides_hashAlgorithm' - A specified override of the default hash algorithm that is used in a
-- code signing job.
newSigningConfigurationOverrides ::
  SigningConfigurationOverrides
newSigningConfigurationOverrides =
  SigningConfigurationOverrides'
    { encryptionAlgorithm =
        Prelude.Nothing,
      hashAlgorithm = Prelude.Nothing
    }

-- | A specified override of the default encryption algorithm that is used in
-- a code signing job.
signingConfigurationOverrides_encryptionAlgorithm :: Lens.Lens' SigningConfigurationOverrides (Prelude.Maybe EncryptionAlgorithm)
signingConfigurationOverrides_encryptionAlgorithm = Lens.lens (\SigningConfigurationOverrides' {encryptionAlgorithm} -> encryptionAlgorithm) (\s@SigningConfigurationOverrides' {} a -> s {encryptionAlgorithm = a} :: SigningConfigurationOverrides)

-- | A specified override of the default hash algorithm that is used in a
-- code signing job.
signingConfigurationOverrides_hashAlgorithm :: Lens.Lens' SigningConfigurationOverrides (Prelude.Maybe HashAlgorithm)
signingConfigurationOverrides_hashAlgorithm = Lens.lens (\SigningConfigurationOverrides' {hashAlgorithm} -> hashAlgorithm) (\s@SigningConfigurationOverrides' {} a -> s {hashAlgorithm = a} :: SigningConfigurationOverrides)

instance Data.FromJSON SigningConfigurationOverrides where
  parseJSON =
    Data.withObject
      "SigningConfigurationOverrides"
      ( \x ->
          SigningConfigurationOverrides'
            Prelude.<$> (x Data..:? "encryptionAlgorithm")
            Prelude.<*> (x Data..:? "hashAlgorithm")
      )

instance
  Prelude.Hashable
    SigningConfigurationOverrides
  where
  hashWithSalt _salt SigningConfigurationOverrides' {..} =
    _salt `Prelude.hashWithSalt` encryptionAlgorithm
      `Prelude.hashWithSalt` hashAlgorithm

instance Prelude.NFData SigningConfigurationOverrides where
  rnf SigningConfigurationOverrides' {..} =
    Prelude.rnf encryptionAlgorithm
      `Prelude.seq` Prelude.rnf hashAlgorithm

instance Data.ToJSON SigningConfigurationOverrides where
  toJSON SigningConfigurationOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionAlgorithm" Data..=)
              Prelude.<$> encryptionAlgorithm,
            ("hashAlgorithm" Data..=) Prelude.<$> hashAlgorithm
          ]
      )
