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
-- Module      : Amazonka.SageMaker.Types.ProcessingOutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingOutput

-- | Configuration for uploading output from the processing container.
--
-- /See:/ 'newProcessingOutputConfig' smart constructor.
data ProcessingOutputConfig = ProcessingOutputConfig'
  { -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt the processing job output.
    -- @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS
    -- key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An array of outputs configuring the data to upload from the processing
    -- container.
    outputs :: [ProcessingOutput]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'processingOutputConfig_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt the processing job output.
-- @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS
-- key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
--
-- 'outputs', 'processingOutputConfig_outputs' - An array of outputs configuring the data to upload from the processing
-- container.
newProcessingOutputConfig ::
  ProcessingOutputConfig
newProcessingOutputConfig =
  ProcessingOutputConfig'
    { kmsKeyId = Prelude.Nothing,
      outputs = Prelude.mempty
    }

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt the processing job output.
-- @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS
-- key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
processingOutputConfig_kmsKeyId :: Lens.Lens' ProcessingOutputConfig (Prelude.Maybe Prelude.Text)
processingOutputConfig_kmsKeyId = Lens.lens (\ProcessingOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@ProcessingOutputConfig' {} a -> s {kmsKeyId = a} :: ProcessingOutputConfig)

-- | An array of outputs configuring the data to upload from the processing
-- container.
processingOutputConfig_outputs :: Lens.Lens' ProcessingOutputConfig [ProcessingOutput]
processingOutputConfig_outputs = Lens.lens (\ProcessingOutputConfig' {outputs} -> outputs) (\s@ProcessingOutputConfig' {} a -> s {outputs = a} :: ProcessingOutputConfig) Prelude.. Lens.coerced

instance Core.FromJSON ProcessingOutputConfig where
  parseJSON =
    Core.withObject
      "ProcessingOutputConfig"
      ( \x ->
          ProcessingOutputConfig'
            Prelude.<$> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Outputs" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ProcessingOutputConfig where
  hashWithSalt _salt ProcessingOutputConfig' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` outputs

instance Prelude.NFData ProcessingOutputConfig where
  rnf ProcessingOutputConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf outputs

instance Core.ToJSON ProcessingOutputConfig where
  toJSON ProcessingOutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("Outputs" Core..= outputs)
          ]
      )
