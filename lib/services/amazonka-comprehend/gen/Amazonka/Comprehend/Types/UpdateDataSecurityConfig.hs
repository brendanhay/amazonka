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
-- Module      : Amazonka.Comprehend.Types.UpdateDataSecurityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.UpdateDataSecurityConfig where

import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data security configuration.
--
-- /See:/ 'newUpdateDataSecurityConfig' smart constructor.
data UpdateDataSecurityConfig = UpdateDataSecurityConfig'
  { -- | ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
    -- models. The ModelKmsKeyId can be either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    modelKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | ID for the KMS key that Amazon Comprehend uses to encrypt the volume.
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelKmsKeyId', 'updateDataSecurityConfig_modelKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
-- models. The ModelKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'volumeKmsKeyId', 'updateDataSecurityConfig_volumeKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt the volume.
--
-- 'vpcConfig', 'updateDataSecurityConfig_vpcConfig' - Undocumented member.
newUpdateDataSecurityConfig ::
  UpdateDataSecurityConfig
newUpdateDataSecurityConfig =
  UpdateDataSecurityConfig'
    { modelKmsKeyId =
        Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
-- models. The ModelKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
updateDataSecurityConfig_modelKmsKeyId :: Lens.Lens' UpdateDataSecurityConfig (Prelude.Maybe Prelude.Text)
updateDataSecurityConfig_modelKmsKeyId = Lens.lens (\UpdateDataSecurityConfig' {modelKmsKeyId} -> modelKmsKeyId) (\s@UpdateDataSecurityConfig' {} a -> s {modelKmsKeyId = a} :: UpdateDataSecurityConfig)

-- | ID for the KMS key that Amazon Comprehend uses to encrypt the volume.
updateDataSecurityConfig_volumeKmsKeyId :: Lens.Lens' UpdateDataSecurityConfig (Prelude.Maybe Prelude.Text)
updateDataSecurityConfig_volumeKmsKeyId = Lens.lens (\UpdateDataSecurityConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@UpdateDataSecurityConfig' {} a -> s {volumeKmsKeyId = a} :: UpdateDataSecurityConfig)

-- | Undocumented member.
updateDataSecurityConfig_vpcConfig :: Lens.Lens' UpdateDataSecurityConfig (Prelude.Maybe VpcConfig)
updateDataSecurityConfig_vpcConfig = Lens.lens (\UpdateDataSecurityConfig' {vpcConfig} -> vpcConfig) (\s@UpdateDataSecurityConfig' {} a -> s {vpcConfig = a} :: UpdateDataSecurityConfig)

instance Prelude.Hashable UpdateDataSecurityConfig where
  hashWithSalt _salt UpdateDataSecurityConfig' {..} =
    _salt
      `Prelude.hashWithSalt` modelKmsKeyId
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData UpdateDataSecurityConfig where
  rnf UpdateDataSecurityConfig' {..} =
    Prelude.rnf modelKmsKeyId
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig

instance Data.ToJSON UpdateDataSecurityConfig where
  toJSON UpdateDataSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ModelKmsKeyId" Data..=) Prelude.<$> modelKmsKeyId,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig
          ]
      )
