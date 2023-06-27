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
-- Module      : Amazonka.Comprehend.Types.DataSecurityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DataSecurityConfig where

import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data security configuration.
--
-- /See:/ 'newDataSecurityConfig' smart constructor.
data DataSecurityConfig = DataSecurityConfig'
  { -- | ID for the KMS key that Amazon Comprehend uses to encrypt the data in
    -- the data lake.
    dataLakeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
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
-- Create a value of 'DataSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakeKmsKeyId', 'dataSecurityConfig_dataLakeKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt the data in
-- the data lake.
--
-- 'modelKmsKeyId', 'dataSecurityConfig_modelKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
-- models. The ModelKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'volumeKmsKeyId', 'dataSecurityConfig_volumeKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt the volume.
--
-- 'vpcConfig', 'dataSecurityConfig_vpcConfig' - Undocumented member.
newDataSecurityConfig ::
  DataSecurityConfig
newDataSecurityConfig =
  DataSecurityConfig'
    { dataLakeKmsKeyId =
        Prelude.Nothing,
      modelKmsKeyId = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | ID for the KMS key that Amazon Comprehend uses to encrypt the data in
-- the data lake.
dataSecurityConfig_dataLakeKmsKeyId :: Lens.Lens' DataSecurityConfig (Prelude.Maybe Prelude.Text)
dataSecurityConfig_dataLakeKmsKeyId = Lens.lens (\DataSecurityConfig' {dataLakeKmsKeyId} -> dataLakeKmsKeyId) (\s@DataSecurityConfig' {} a -> s {dataLakeKmsKeyId = a} :: DataSecurityConfig)

-- | ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
-- models. The ModelKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
dataSecurityConfig_modelKmsKeyId :: Lens.Lens' DataSecurityConfig (Prelude.Maybe Prelude.Text)
dataSecurityConfig_modelKmsKeyId = Lens.lens (\DataSecurityConfig' {modelKmsKeyId} -> modelKmsKeyId) (\s@DataSecurityConfig' {} a -> s {modelKmsKeyId = a} :: DataSecurityConfig)

-- | ID for the KMS key that Amazon Comprehend uses to encrypt the volume.
dataSecurityConfig_volumeKmsKeyId :: Lens.Lens' DataSecurityConfig (Prelude.Maybe Prelude.Text)
dataSecurityConfig_volumeKmsKeyId = Lens.lens (\DataSecurityConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DataSecurityConfig' {} a -> s {volumeKmsKeyId = a} :: DataSecurityConfig)

-- | Undocumented member.
dataSecurityConfig_vpcConfig :: Lens.Lens' DataSecurityConfig (Prelude.Maybe VpcConfig)
dataSecurityConfig_vpcConfig = Lens.lens (\DataSecurityConfig' {vpcConfig} -> vpcConfig) (\s@DataSecurityConfig' {} a -> s {vpcConfig = a} :: DataSecurityConfig)

instance Data.FromJSON DataSecurityConfig where
  parseJSON =
    Data.withObject
      "DataSecurityConfig"
      ( \x ->
          DataSecurityConfig'
            Prelude.<$> (x Data..:? "DataLakeKmsKeyId")
            Prelude.<*> (x Data..:? "ModelKmsKeyId")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance Prelude.Hashable DataSecurityConfig where
  hashWithSalt _salt DataSecurityConfig' {..} =
    _salt
      `Prelude.hashWithSalt` dataLakeKmsKeyId
      `Prelude.hashWithSalt` modelKmsKeyId
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData DataSecurityConfig where
  rnf DataSecurityConfig' {..} =
    Prelude.rnf dataLakeKmsKeyId
      `Prelude.seq` Prelude.rnf modelKmsKeyId
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig

instance Data.ToJSON DataSecurityConfig where
  toJSON DataSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataLakeKmsKeyId" Data..=)
              Prelude.<$> dataLakeKmsKeyId,
            ("ModelKmsKeyId" Data..=) Prelude.<$> modelKmsKeyId,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig
          ]
      )
