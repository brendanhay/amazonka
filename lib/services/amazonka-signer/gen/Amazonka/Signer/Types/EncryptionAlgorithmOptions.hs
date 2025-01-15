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
-- Module      : Amazonka.Signer.Types.EncryptionAlgorithmOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.EncryptionAlgorithmOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.EncryptionAlgorithm

-- | The encryption algorithm options that are available to a code signing
-- job.
--
-- /See:/ 'newEncryptionAlgorithmOptions' smart constructor.
data EncryptionAlgorithmOptions = EncryptionAlgorithmOptions'
  { -- | The set of accepted encryption algorithms that are allowed in a code
    -- signing job.
    allowedValues :: [EncryptionAlgorithm],
    -- | The default encryption algorithm that is used by a code signing job.
    defaultValue :: EncryptionAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAlgorithmOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'encryptionAlgorithmOptions_allowedValues' - The set of accepted encryption algorithms that are allowed in a code
-- signing job.
--
-- 'defaultValue', 'encryptionAlgorithmOptions_defaultValue' - The default encryption algorithm that is used by a code signing job.
newEncryptionAlgorithmOptions ::
  -- | 'defaultValue'
  EncryptionAlgorithm ->
  EncryptionAlgorithmOptions
newEncryptionAlgorithmOptions pDefaultValue_ =
  EncryptionAlgorithmOptions'
    { allowedValues =
        Prelude.mempty,
      defaultValue = pDefaultValue_
    }

-- | The set of accepted encryption algorithms that are allowed in a code
-- signing job.
encryptionAlgorithmOptions_allowedValues :: Lens.Lens' EncryptionAlgorithmOptions [EncryptionAlgorithm]
encryptionAlgorithmOptions_allowedValues = Lens.lens (\EncryptionAlgorithmOptions' {allowedValues} -> allowedValues) (\s@EncryptionAlgorithmOptions' {} a -> s {allowedValues = a} :: EncryptionAlgorithmOptions) Prelude.. Lens.coerced

-- | The default encryption algorithm that is used by a code signing job.
encryptionAlgorithmOptions_defaultValue :: Lens.Lens' EncryptionAlgorithmOptions EncryptionAlgorithm
encryptionAlgorithmOptions_defaultValue = Lens.lens (\EncryptionAlgorithmOptions' {defaultValue} -> defaultValue) (\s@EncryptionAlgorithmOptions' {} a -> s {defaultValue = a} :: EncryptionAlgorithmOptions)

instance Data.FromJSON EncryptionAlgorithmOptions where
  parseJSON =
    Data.withObject
      "EncryptionAlgorithmOptions"
      ( \x ->
          EncryptionAlgorithmOptions'
            Prelude.<$> (x Data..:? "allowedValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "defaultValue")
      )

instance Prelude.Hashable EncryptionAlgorithmOptions where
  hashWithSalt _salt EncryptionAlgorithmOptions' {..} =
    _salt
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` defaultValue

instance Prelude.NFData EncryptionAlgorithmOptions where
  rnf EncryptionAlgorithmOptions' {..} =
    Prelude.rnf allowedValues `Prelude.seq`
      Prelude.rnf defaultValue
