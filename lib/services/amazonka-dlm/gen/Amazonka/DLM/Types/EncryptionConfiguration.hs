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
-- Module      : Amazonka.DLM.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Event-based policies only]__ Specifies the encryption settings for
-- cross-Region snapshot copies created by event-based policies.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the KMS key to use for EBS encryption.
    -- If this parameter is not specified, the default KMS key for the account
    -- is used.
    cmkArn :: Prelude.Maybe Prelude.Text,
    -- | To encrypt a copy of an unencrypted snapshot when encryption by default
    -- is not enabled, enable encryption using this parameter. Copies of
    -- encrypted snapshots are encrypted, even if this parameter is false or
    -- when encryption by default is not enabled.
    encrypted :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cmkArn', 'encryptionConfiguration_cmkArn' - The Amazon Resource Name (ARN) of the KMS key to use for EBS encryption.
-- If this parameter is not specified, the default KMS key for the account
-- is used.
--
-- 'encrypted', 'encryptionConfiguration_encrypted' - To encrypt a copy of an unencrypted snapshot when encryption by default
-- is not enabled, enable encryption using this parameter. Copies of
-- encrypted snapshots are encrypted, even if this parameter is false or
-- when encryption by default is not enabled.
newEncryptionConfiguration ::
  -- | 'encrypted'
  Prelude.Bool ->
  EncryptionConfiguration
newEncryptionConfiguration pEncrypted_ =
  EncryptionConfiguration'
    { cmkArn = Prelude.Nothing,
      encrypted = pEncrypted_
    }

-- | The Amazon Resource Name (ARN) of the KMS key to use for EBS encryption.
-- If this parameter is not specified, the default KMS key for the account
-- is used.
encryptionConfiguration_cmkArn :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe Prelude.Text)
encryptionConfiguration_cmkArn = Lens.lens (\EncryptionConfiguration' {cmkArn} -> cmkArn) (\s@EncryptionConfiguration' {} a -> s {cmkArn = a} :: EncryptionConfiguration)

-- | To encrypt a copy of an unencrypted snapshot when encryption by default
-- is not enabled, enable encryption using this parameter. Copies of
-- encrypted snapshots are encrypted, even if this parameter is false or
-- when encryption by default is not enabled.
encryptionConfiguration_encrypted :: Lens.Lens' EncryptionConfiguration Prelude.Bool
encryptionConfiguration_encrypted = Lens.lens (\EncryptionConfiguration' {encrypted} -> encrypted) (\s@EncryptionConfiguration' {} a -> s {encrypted = a} :: EncryptionConfiguration)

instance Data.FromJSON EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Data..:? "CmkArn")
            Prelude.<*> (x Data..: "Encrypted")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt _salt EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cmkArn
      `Prelude.hashWithSalt` encrypted

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf cmkArn
      `Prelude.seq` Prelude.rnf encrypted

instance Data.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CmkArn" Data..=) Prelude.<$> cmkArn,
            Prelude.Just ("Encrypted" Data..= encrypted)
          ]
      )
