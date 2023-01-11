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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobOutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RecommendationJobCompiledOutputConfig

-- | Provides information about the output configuration for the compiled
-- model.
--
-- /See:/ 'newRecommendationJobOutputConfig' smart constructor.
data RecommendationJobOutputConfig = RecommendationJobOutputConfig'
  { -- | Provides information about the output configuration for the compiled
    -- model.
    compiledOutputConfig :: Prelude.Maybe RecommendationJobCompiledOutputConfig,
    -- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) key that Amazon SageMaker uses to
    -- encrypt your output artifacts with Amazon S3 server-side encryption. The
    -- SageMaker execution role must have @kms:GenerateDataKey@ permission.
    --
    -- The @KmsKeyId@ can be any of the following formats:
    --
    -- -   \/\/ KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
    --
    -- -   \/\/ KMS Key Alias
    --
    --     @\"alias\/ExampleAlias\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
    --
    --     @\"arn:aws:kms:\<region>:\<account>:alias\/\<ExampleAlias>\"@
    --
    -- For more information about key identifiers, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
    -- in the Amazon Web Services Key Management Service (Amazon Web Services
    -- KMS) documentation.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compiledOutputConfig', 'recommendationJobOutputConfig_compiledOutputConfig' - Provides information about the output configuration for the compiled
-- model.
--
-- 'kmsKeyId', 'recommendationJobOutputConfig_kmsKeyId' - The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) key that Amazon SageMaker uses to
-- encrypt your output artifacts with Amazon S3 server-side encryption. The
-- SageMaker execution role must have @kms:GenerateDataKey@ permission.
--
-- The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
--
-- -   \/\/ KMS Key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
--
--     @\"arn:aws:kms:\<region>:\<account>:alias\/\<ExampleAlias>\"@
--
-- For more information about key identifiers, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
-- in the Amazon Web Services Key Management Service (Amazon Web Services
-- KMS) documentation.
newRecommendationJobOutputConfig ::
  RecommendationJobOutputConfig
newRecommendationJobOutputConfig =
  RecommendationJobOutputConfig'
    { compiledOutputConfig =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | Provides information about the output configuration for the compiled
-- model.
recommendationJobOutputConfig_compiledOutputConfig :: Lens.Lens' RecommendationJobOutputConfig (Prelude.Maybe RecommendationJobCompiledOutputConfig)
recommendationJobOutputConfig_compiledOutputConfig = Lens.lens (\RecommendationJobOutputConfig' {compiledOutputConfig} -> compiledOutputConfig) (\s@RecommendationJobOutputConfig' {} a -> s {compiledOutputConfig = a} :: RecommendationJobOutputConfig)

-- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) key that Amazon SageMaker uses to
-- encrypt your output artifacts with Amazon S3 server-side encryption. The
-- SageMaker execution role must have @kms:GenerateDataKey@ permission.
--
-- The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
--
-- -   \/\/ KMS Key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
--
--     @\"arn:aws:kms:\<region>:\<account>:alias\/\<ExampleAlias>\"@
--
-- For more information about key identifiers, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
-- in the Amazon Web Services Key Management Service (Amazon Web Services
-- KMS) documentation.
recommendationJobOutputConfig_kmsKeyId :: Lens.Lens' RecommendationJobOutputConfig (Prelude.Maybe Prelude.Text)
recommendationJobOutputConfig_kmsKeyId = Lens.lens (\RecommendationJobOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@RecommendationJobOutputConfig' {} a -> s {kmsKeyId = a} :: RecommendationJobOutputConfig)

instance
  Prelude.Hashable
    RecommendationJobOutputConfig
  where
  hashWithSalt _salt RecommendationJobOutputConfig' {..} =
    _salt `Prelude.hashWithSalt` compiledOutputConfig
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData RecommendationJobOutputConfig where
  rnf RecommendationJobOutputConfig' {..} =
    Prelude.rnf compiledOutputConfig
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToJSON RecommendationJobOutputConfig where
  toJSON RecommendationJobOutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompiledOutputConfig" Data..=)
              Prelude.<$> compiledOutputConfig,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId
          ]
      )
