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
-- Module      : Amazonka.Signer.Types.HashAlgorithmOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.HashAlgorithmOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.HashAlgorithm

-- | The hash algorithms that are available to a code signing job.
--
-- /See:/ 'newHashAlgorithmOptions' smart constructor.
data HashAlgorithmOptions = HashAlgorithmOptions'
  { -- | The set of accepted hash algorithms allowed in a code signing job.
    allowedValues :: [HashAlgorithm],
    -- | The default hash algorithm that is used in a code signing job.
    defaultValue :: HashAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HashAlgorithmOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'hashAlgorithmOptions_allowedValues' - The set of accepted hash algorithms allowed in a code signing job.
--
-- 'defaultValue', 'hashAlgorithmOptions_defaultValue' - The default hash algorithm that is used in a code signing job.
newHashAlgorithmOptions ::
  -- | 'defaultValue'
  HashAlgorithm ->
  HashAlgorithmOptions
newHashAlgorithmOptions pDefaultValue_ =
  HashAlgorithmOptions'
    { allowedValues =
        Prelude.mempty,
      defaultValue = pDefaultValue_
    }

-- | The set of accepted hash algorithms allowed in a code signing job.
hashAlgorithmOptions_allowedValues :: Lens.Lens' HashAlgorithmOptions [HashAlgorithm]
hashAlgorithmOptions_allowedValues = Lens.lens (\HashAlgorithmOptions' {allowedValues} -> allowedValues) (\s@HashAlgorithmOptions' {} a -> s {allowedValues = a} :: HashAlgorithmOptions) Prelude.. Lens.coerced

-- | The default hash algorithm that is used in a code signing job.
hashAlgorithmOptions_defaultValue :: Lens.Lens' HashAlgorithmOptions HashAlgorithm
hashAlgorithmOptions_defaultValue = Lens.lens (\HashAlgorithmOptions' {defaultValue} -> defaultValue) (\s@HashAlgorithmOptions' {} a -> s {defaultValue = a} :: HashAlgorithmOptions)

instance Data.FromJSON HashAlgorithmOptions where
  parseJSON =
    Data.withObject
      "HashAlgorithmOptions"
      ( \x ->
          HashAlgorithmOptions'
            Prelude.<$> (x Data..:? "allowedValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "defaultValue")
      )

instance Prelude.Hashable HashAlgorithmOptions where
  hashWithSalt _salt HashAlgorithmOptions' {..} =
    _salt
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` defaultValue

instance Prelude.NFData HashAlgorithmOptions where
  rnf HashAlgorithmOptions' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf defaultValue
