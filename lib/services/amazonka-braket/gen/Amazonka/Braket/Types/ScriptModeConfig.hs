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
-- Module      : Amazonka.Braket.Types.ScriptModeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.ScriptModeConfig where

import Amazonka.Braket.Types.CompressionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Python scripts used for entry and by an
-- Amazon Braket job.
--
-- /See:/ 'newScriptModeConfig' smart constructor.
data ScriptModeConfig = ScriptModeConfig'
  { -- | The type of compression used by the Python scripts for an Amazon Braket
    -- job.
    compressionType :: Prelude.Maybe CompressionType,
    -- | The path to the Python script that serves as the entry point for an
    -- Amazon Braket job.
    entryPoint :: Prelude.Text,
    -- | The URI that specifies the S3 path to the Python script module that
    -- contains the training script used by an Amazon Braket job.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScriptModeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compressionType', 'scriptModeConfig_compressionType' - The type of compression used by the Python scripts for an Amazon Braket
-- job.
--
-- 'entryPoint', 'scriptModeConfig_entryPoint' - The path to the Python script that serves as the entry point for an
-- Amazon Braket job.
--
-- 's3Uri', 'scriptModeConfig_s3Uri' - The URI that specifies the S3 path to the Python script module that
-- contains the training script used by an Amazon Braket job.
newScriptModeConfig ::
  -- | 'entryPoint'
  Prelude.Text ->
  -- | 's3Uri'
  Prelude.Text ->
  ScriptModeConfig
newScriptModeConfig pEntryPoint_ pS3Uri_ =
  ScriptModeConfig'
    { compressionType =
        Prelude.Nothing,
      entryPoint = pEntryPoint_,
      s3Uri = pS3Uri_
    }

-- | The type of compression used by the Python scripts for an Amazon Braket
-- job.
scriptModeConfig_compressionType :: Lens.Lens' ScriptModeConfig (Prelude.Maybe CompressionType)
scriptModeConfig_compressionType = Lens.lens (\ScriptModeConfig' {compressionType} -> compressionType) (\s@ScriptModeConfig' {} a -> s {compressionType = a} :: ScriptModeConfig)

-- | The path to the Python script that serves as the entry point for an
-- Amazon Braket job.
scriptModeConfig_entryPoint :: Lens.Lens' ScriptModeConfig Prelude.Text
scriptModeConfig_entryPoint = Lens.lens (\ScriptModeConfig' {entryPoint} -> entryPoint) (\s@ScriptModeConfig' {} a -> s {entryPoint = a} :: ScriptModeConfig)

-- | The URI that specifies the S3 path to the Python script module that
-- contains the training script used by an Amazon Braket job.
scriptModeConfig_s3Uri :: Lens.Lens' ScriptModeConfig Prelude.Text
scriptModeConfig_s3Uri = Lens.lens (\ScriptModeConfig' {s3Uri} -> s3Uri) (\s@ScriptModeConfig' {} a -> s {s3Uri = a} :: ScriptModeConfig)

instance Data.FromJSON ScriptModeConfig where
  parseJSON =
    Data.withObject
      "ScriptModeConfig"
      ( \x ->
          ScriptModeConfig'
            Prelude.<$> (x Data..:? "compressionType")
            Prelude.<*> (x Data..: "entryPoint")
            Prelude.<*> (x Data..: "s3Uri")
      )

instance Prelude.Hashable ScriptModeConfig where
  hashWithSalt _salt ScriptModeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` entryPoint
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData ScriptModeConfig where
  rnf ScriptModeConfig' {..} =
    Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf entryPoint
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON ScriptModeConfig where
  toJSON ScriptModeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("compressionType" Data..=)
              Prelude.<$> compressionType,
            Prelude.Just ("entryPoint" Data..= entryPoint),
            Prelude.Just ("s3Uri" Data..= s3Uri)
          ]
      )
