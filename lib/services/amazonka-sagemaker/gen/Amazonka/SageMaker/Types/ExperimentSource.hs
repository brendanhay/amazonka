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
-- Module      : Amazonka.SageMaker.Types.ExperimentSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ExperimentSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source of the experiment.
--
-- /See:/ 'newExperimentSource' smart constructor.
data ExperimentSource = ExperimentSource'
  { -- | The source type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'experimentSource_sourceType' - The source type.
--
-- 'sourceArn', 'experimentSource_sourceArn' - The Amazon Resource Name (ARN) of the source.
newExperimentSource ::
  -- | 'sourceArn'
  Prelude.Text ->
  ExperimentSource
newExperimentSource pSourceArn_ =
  ExperimentSource'
    { sourceType = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The source type.
experimentSource_sourceType :: Lens.Lens' ExperimentSource (Prelude.Maybe Prelude.Text)
experimentSource_sourceType = Lens.lens (\ExperimentSource' {sourceType} -> sourceType) (\s@ExperimentSource' {} a -> s {sourceType = a} :: ExperimentSource)

-- | The Amazon Resource Name (ARN) of the source.
experimentSource_sourceArn :: Lens.Lens' ExperimentSource Prelude.Text
experimentSource_sourceArn = Lens.lens (\ExperimentSource' {sourceArn} -> sourceArn) (\s@ExperimentSource' {} a -> s {sourceArn = a} :: ExperimentSource)

instance Data.FromJSON ExperimentSource where
  parseJSON =
    Data.withObject
      "ExperimentSource"
      ( \x ->
          ExperimentSource'
            Prelude.<$> (x Data..:? "SourceType")
            Prelude.<*> (x Data..: "SourceArn")
      )

instance Prelude.Hashable ExperimentSource where
  hashWithSalt _salt ExperimentSource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData ExperimentSource where
  rnf ExperimentSource' {..} =
    Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceArn
