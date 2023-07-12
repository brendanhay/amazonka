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
-- Module      : Amazonka.RobOMaker.Types.SourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.Architecture

-- | Information about a source configuration.
--
-- /See:/ 'newSourceConfig' smart constructor.
data SourceConfig = SourceConfig'
  { -- | The target processor architecture for the application.
    architecture :: Prelude.Maybe Architecture,
    -- | The Amazon S3 bucket name.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The s3 object key.
    s3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'sourceConfig_architecture' - The target processor architecture for the application.
--
-- 's3Bucket', 'sourceConfig_s3Bucket' - The Amazon S3 bucket name.
--
-- 's3Key', 'sourceConfig_s3Key' - The s3 object key.
newSourceConfig ::
  SourceConfig
newSourceConfig =
  SourceConfig'
    { architecture = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The target processor architecture for the application.
sourceConfig_architecture :: Lens.Lens' SourceConfig (Prelude.Maybe Architecture)
sourceConfig_architecture = Lens.lens (\SourceConfig' {architecture} -> architecture) (\s@SourceConfig' {} a -> s {architecture = a} :: SourceConfig)

-- | The Amazon S3 bucket name.
sourceConfig_s3Bucket :: Lens.Lens' SourceConfig (Prelude.Maybe Prelude.Text)
sourceConfig_s3Bucket = Lens.lens (\SourceConfig' {s3Bucket} -> s3Bucket) (\s@SourceConfig' {} a -> s {s3Bucket = a} :: SourceConfig)

-- | The s3 object key.
sourceConfig_s3Key :: Lens.Lens' SourceConfig (Prelude.Maybe Prelude.Text)
sourceConfig_s3Key = Lens.lens (\SourceConfig' {s3Key} -> s3Key) (\s@SourceConfig' {} a -> s {s3Key = a} :: SourceConfig)

instance Prelude.Hashable SourceConfig where
  hashWithSalt _salt SourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData SourceConfig where
  rnf SourceConfig' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key

instance Data.ToJSON SourceConfig where
  toJSON SourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("architecture" Data..=) Prelude.<$> architecture,
            ("s3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("s3Key" Data..=) Prelude.<$> s3Key
          ]
      )
