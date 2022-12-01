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
-- Module      : Amazonka.FIS.Types.ExperimentS3LogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentS3LogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for experiment logging to Amazon S3.
--
-- /See:/ 'newExperimentS3LogConfiguration' smart constructor.
data ExperimentS3LogConfiguration = ExperimentS3LogConfiguration'
  { -- | The name of the destination bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The bucket prefix.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentS3LogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'experimentS3LogConfiguration_bucketName' - The name of the destination bucket.
--
-- 'prefix', 'experimentS3LogConfiguration_prefix' - The bucket prefix.
newExperimentS3LogConfiguration ::
  ExperimentS3LogConfiguration
newExperimentS3LogConfiguration =
  ExperimentS3LogConfiguration'
    { bucketName =
        Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The name of the destination bucket.
experimentS3LogConfiguration_bucketName :: Lens.Lens' ExperimentS3LogConfiguration (Prelude.Maybe Prelude.Text)
experimentS3LogConfiguration_bucketName = Lens.lens (\ExperimentS3LogConfiguration' {bucketName} -> bucketName) (\s@ExperimentS3LogConfiguration' {} a -> s {bucketName = a} :: ExperimentS3LogConfiguration)

-- | The bucket prefix.
experimentS3LogConfiguration_prefix :: Lens.Lens' ExperimentS3LogConfiguration (Prelude.Maybe Prelude.Text)
experimentS3LogConfiguration_prefix = Lens.lens (\ExperimentS3LogConfiguration' {prefix} -> prefix) (\s@ExperimentS3LogConfiguration' {} a -> s {prefix = a} :: ExperimentS3LogConfiguration)

instance Core.FromJSON ExperimentS3LogConfiguration where
  parseJSON =
    Core.withObject
      "ExperimentS3LogConfiguration"
      ( \x ->
          ExperimentS3LogConfiguration'
            Prelude.<$> (x Core..:? "bucketName")
            Prelude.<*> (x Core..:? "prefix")
      )

instance
  Prelude.Hashable
    ExperimentS3LogConfiguration
  where
  hashWithSalt _salt ExperimentS3LogConfiguration' {..} =
    _salt `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData ExperimentS3LogConfiguration where
  rnf ExperimentS3LogConfiguration' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf prefix
