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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateS3LogConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateS3LogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for experiment logging to Amazon S3.
--
-- /See:/ 'newExperimentTemplateS3LogConfiguration' smart constructor.
data ExperimentTemplateS3LogConfiguration = ExperimentTemplateS3LogConfiguration'
  { -- | The name of the destination bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The bucket prefix.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateS3LogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'experimentTemplateS3LogConfiguration_bucketName' - The name of the destination bucket.
--
-- 'prefix', 'experimentTemplateS3LogConfiguration_prefix' - The bucket prefix.
newExperimentTemplateS3LogConfiguration ::
  ExperimentTemplateS3LogConfiguration
newExperimentTemplateS3LogConfiguration =
  ExperimentTemplateS3LogConfiguration'
    { bucketName =
        Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The name of the destination bucket.
experimentTemplateS3LogConfiguration_bucketName :: Lens.Lens' ExperimentTemplateS3LogConfiguration (Prelude.Maybe Prelude.Text)
experimentTemplateS3LogConfiguration_bucketName = Lens.lens (\ExperimentTemplateS3LogConfiguration' {bucketName} -> bucketName) (\s@ExperimentTemplateS3LogConfiguration' {} a -> s {bucketName = a} :: ExperimentTemplateS3LogConfiguration)

-- | The bucket prefix.
experimentTemplateS3LogConfiguration_prefix :: Lens.Lens' ExperimentTemplateS3LogConfiguration (Prelude.Maybe Prelude.Text)
experimentTemplateS3LogConfiguration_prefix = Lens.lens (\ExperimentTemplateS3LogConfiguration' {prefix} -> prefix) (\s@ExperimentTemplateS3LogConfiguration' {} a -> s {prefix = a} :: ExperimentTemplateS3LogConfiguration)

instance
  Data.FromJSON
    ExperimentTemplateS3LogConfiguration
  where
  parseJSON =
    Data.withObject
      "ExperimentTemplateS3LogConfiguration"
      ( \x ->
          ExperimentTemplateS3LogConfiguration'
            Prelude.<$> (x Data..:? "bucketName")
            Prelude.<*> (x Data..:? "prefix")
      )

instance
  Prelude.Hashable
    ExperimentTemplateS3LogConfiguration
  where
  hashWithSalt
    _salt
    ExperimentTemplateS3LogConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` prefix

instance
  Prelude.NFData
    ExperimentTemplateS3LogConfiguration
  where
  rnf ExperimentTemplateS3LogConfiguration' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf prefix
