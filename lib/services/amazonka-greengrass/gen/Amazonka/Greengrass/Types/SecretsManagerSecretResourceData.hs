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
-- Module      : Amazonka.Greengrass.Types.SecretsManagerSecretResourceData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.SecretsManagerSecretResourceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attributes that define a secret resource, which references a secret from
-- AWS Secrets Manager. AWS IoT Greengrass stores a local, encrypted copy
-- of the secret on the Greengrass core, where it can be securely accessed
-- by connectors and Lambda functions.
--
-- /See:/ 'newSecretsManagerSecretResourceData' smart constructor.
data SecretsManagerSecretResourceData = SecretsManagerSecretResourceData'
  { -- | The ARN of the Secrets Manager secret to make available on the core. The
    -- value of the secret\'s latest version (represented by the
    -- \'\'AWSCURRENT\'\' staging label) is included by default.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Optional. The staging labels whose values you want to make available on
    -- the core, in addition to \'\'AWSCURRENT\'\'.
    additionalStagingLabelsToDownload :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecretsManagerSecretResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'secretsManagerSecretResourceData_arn' - The ARN of the Secrets Manager secret to make available on the core. The
-- value of the secret\'s latest version (represented by the
-- \'\'AWSCURRENT\'\' staging label) is included by default.
--
-- 'additionalStagingLabelsToDownload', 'secretsManagerSecretResourceData_additionalStagingLabelsToDownload' - Optional. The staging labels whose values you want to make available on
-- the core, in addition to \'\'AWSCURRENT\'\'.
newSecretsManagerSecretResourceData ::
  SecretsManagerSecretResourceData
newSecretsManagerSecretResourceData =
  SecretsManagerSecretResourceData'
    { arn =
        Prelude.Nothing,
      additionalStagingLabelsToDownload =
        Prelude.Nothing
    }

-- | The ARN of the Secrets Manager secret to make available on the core. The
-- value of the secret\'s latest version (represented by the
-- \'\'AWSCURRENT\'\' staging label) is included by default.
secretsManagerSecretResourceData_arn :: Lens.Lens' SecretsManagerSecretResourceData (Prelude.Maybe Prelude.Text)
secretsManagerSecretResourceData_arn = Lens.lens (\SecretsManagerSecretResourceData' {arn} -> arn) (\s@SecretsManagerSecretResourceData' {} a -> s {arn = a} :: SecretsManagerSecretResourceData)

-- | Optional. The staging labels whose values you want to make available on
-- the core, in addition to \'\'AWSCURRENT\'\'.
secretsManagerSecretResourceData_additionalStagingLabelsToDownload :: Lens.Lens' SecretsManagerSecretResourceData (Prelude.Maybe [Prelude.Text])
secretsManagerSecretResourceData_additionalStagingLabelsToDownload = Lens.lens (\SecretsManagerSecretResourceData' {additionalStagingLabelsToDownload} -> additionalStagingLabelsToDownload) (\s@SecretsManagerSecretResourceData' {} a -> s {additionalStagingLabelsToDownload = a} :: SecretsManagerSecretResourceData) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SecretsManagerSecretResourceData
  where
  parseJSON =
    Data.withObject
      "SecretsManagerSecretResourceData"
      ( \x ->
          SecretsManagerSecretResourceData'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> ( x
                            Data..:? "AdditionalStagingLabelsToDownload"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    SecretsManagerSecretResourceData
  where
  hashWithSalt
    _salt
    SecretsManagerSecretResourceData' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` additionalStagingLabelsToDownload

instance
  Prelude.NFData
    SecretsManagerSecretResourceData
  where
  rnf SecretsManagerSecretResourceData' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf additionalStagingLabelsToDownload

instance Data.ToJSON SecretsManagerSecretResourceData where
  toJSON SecretsManagerSecretResourceData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ARN" Data..=) Prelude.<$> arn,
            ("AdditionalStagingLabelsToDownload" Data..=)
              Prelude.<$> additionalStagingLabelsToDownload
          ]
      )
