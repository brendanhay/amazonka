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
-- Module      : Amazonka.GroundStation.Types.KmsKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.KmsKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | AWS Key Management Service (KMS) Key.
--
-- /See:/ 'newKmsKey' smart constructor.
data KmsKey = KmsKey'
  { -- | KMS Alias Arn.
    kmsAliasArn :: Prelude.Maybe Prelude.Text,
    -- | KMS Key Arn.
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KmsKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsAliasArn', 'kmsKey_kmsAliasArn' - KMS Alias Arn.
--
-- 'kmsKeyArn', 'kmsKey_kmsKeyArn' - KMS Key Arn.
newKmsKey ::
  KmsKey
newKmsKey =
  KmsKey'
    { kmsAliasArn = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | KMS Alias Arn.
kmsKey_kmsAliasArn :: Lens.Lens' KmsKey (Prelude.Maybe Prelude.Text)
kmsKey_kmsAliasArn = Lens.lens (\KmsKey' {kmsAliasArn} -> kmsAliasArn) (\s@KmsKey' {} a -> s {kmsAliasArn = a} :: KmsKey)

-- | KMS Key Arn.
kmsKey_kmsKeyArn :: Lens.Lens' KmsKey (Prelude.Maybe Prelude.Text)
kmsKey_kmsKeyArn = Lens.lens (\KmsKey' {kmsKeyArn} -> kmsKeyArn) (\s@KmsKey' {} a -> s {kmsKeyArn = a} :: KmsKey)

instance Data.FromJSON KmsKey where
  parseJSON =
    Data.withObject
      "KmsKey"
      ( \x ->
          KmsKey'
            Prelude.<$> (x Data..:? "kmsAliasArn")
            Prelude.<*> (x Data..:? "kmsKeyArn")
      )

instance Prelude.Hashable KmsKey where
  hashWithSalt _salt KmsKey' {..} =
    _salt
      `Prelude.hashWithSalt` kmsAliasArn
      `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData KmsKey where
  rnf KmsKey' {..} =
    Prelude.rnf kmsAliasArn
      `Prelude.seq` Prelude.rnf kmsKeyArn

instance Data.ToJSON KmsKey where
  toJSON KmsKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsAliasArn" Data..=) Prelude.<$> kmsAliasArn,
            ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn
          ]
      )
