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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateS3LogConfigurationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateS3LogConfigurationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration for experiment logging to Amazon S3.
--
-- /See:/ 'newExperimentTemplateS3LogConfigurationInput' smart constructor.
data ExperimentTemplateS3LogConfigurationInput = ExperimentTemplateS3LogConfigurationInput'
  { -- | The bucket prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the destination bucket.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateS3LogConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'experimentTemplateS3LogConfigurationInput_prefix' - The bucket prefix.
--
-- 'bucketName', 'experimentTemplateS3LogConfigurationInput_bucketName' - The name of the destination bucket.
newExperimentTemplateS3LogConfigurationInput ::
  -- | 'bucketName'
  Prelude.Text ->
  ExperimentTemplateS3LogConfigurationInput
newExperimentTemplateS3LogConfigurationInput
  pBucketName_ =
    ExperimentTemplateS3LogConfigurationInput'
      { prefix =
          Prelude.Nothing,
        bucketName = pBucketName_
      }

-- | The bucket prefix.
experimentTemplateS3LogConfigurationInput_prefix :: Lens.Lens' ExperimentTemplateS3LogConfigurationInput (Prelude.Maybe Prelude.Text)
experimentTemplateS3LogConfigurationInput_prefix = Lens.lens (\ExperimentTemplateS3LogConfigurationInput' {prefix} -> prefix) (\s@ExperimentTemplateS3LogConfigurationInput' {} a -> s {prefix = a} :: ExperimentTemplateS3LogConfigurationInput)

-- | The name of the destination bucket.
experimentTemplateS3LogConfigurationInput_bucketName :: Lens.Lens' ExperimentTemplateS3LogConfigurationInput Prelude.Text
experimentTemplateS3LogConfigurationInput_bucketName = Lens.lens (\ExperimentTemplateS3LogConfigurationInput' {bucketName} -> bucketName) (\s@ExperimentTemplateS3LogConfigurationInput' {} a -> s {bucketName = a} :: ExperimentTemplateS3LogConfigurationInput)

instance
  Prelude.Hashable
    ExperimentTemplateS3LogConfigurationInput
  where
  hashWithSalt
    _salt
    ExperimentTemplateS3LogConfigurationInput' {..} =
      _salt `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` bucketName

instance
  Prelude.NFData
    ExperimentTemplateS3LogConfigurationInput
  where
  rnf ExperimentTemplateS3LogConfigurationInput' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName

instance
  Data.ToJSON
    ExperimentTemplateS3LogConfigurationInput
  where
  toJSON ExperimentTemplateS3LogConfigurationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("bucketName" Data..= bucketName)
          ]
      )
