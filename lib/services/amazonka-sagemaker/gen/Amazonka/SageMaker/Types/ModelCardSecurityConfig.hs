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
-- Module      : Amazonka.SageMaker.Types.ModelCardSecurityConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardSecurityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configure the security settings to protect model card data.
--
-- /See:/ 'newModelCardSecurityConfig' smart constructor.
data ModelCardSecurityConfig = ModelCardSecurityConfig'
  { -- | A Key Management Service
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id key ID>
    -- to use for encrypting a model card.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCardSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'modelCardSecurityConfig_kmsKeyId' - A Key Management Service
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id key ID>
-- to use for encrypting a model card.
newModelCardSecurityConfig ::
  ModelCardSecurityConfig
newModelCardSecurityConfig =
  ModelCardSecurityConfig'
    { kmsKeyId =
        Prelude.Nothing
    }

-- | A Key Management Service
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id key ID>
-- to use for encrypting a model card.
modelCardSecurityConfig_kmsKeyId :: Lens.Lens' ModelCardSecurityConfig (Prelude.Maybe Prelude.Text)
modelCardSecurityConfig_kmsKeyId = Lens.lens (\ModelCardSecurityConfig' {kmsKeyId} -> kmsKeyId) (\s@ModelCardSecurityConfig' {} a -> s {kmsKeyId = a} :: ModelCardSecurityConfig)

instance Data.FromJSON ModelCardSecurityConfig where
  parseJSON =
    Data.withObject
      "ModelCardSecurityConfig"
      ( \x ->
          ModelCardSecurityConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
      )

instance Prelude.Hashable ModelCardSecurityConfig where
  hashWithSalt _salt ModelCardSecurityConfig' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData ModelCardSecurityConfig where
  rnf ModelCardSecurityConfig' {..} =
    Prelude.rnf kmsKeyId

instance Data.ToJSON ModelCardSecurityConfig where
  toJSON ModelCardSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("KmsKeyId" Data..=) Prelude.<$> kmsKeyId]
      )
