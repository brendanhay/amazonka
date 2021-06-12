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
-- Module      : Network.AWS.SageMaker.Types.ExperimentSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The source of the experiment.
--
-- /See:/ 'newExperimentSource' smart constructor.
data ExperimentSource = ExperimentSource'
  { -- | The source type.
    sourceType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ExperimentSource
newExperimentSource pSourceArn_ =
  ExperimentSource'
    { sourceType = Core.Nothing,
      sourceArn = pSourceArn_
    }

-- | The source type.
experimentSource_sourceType :: Lens.Lens' ExperimentSource (Core.Maybe Core.Text)
experimentSource_sourceType = Lens.lens (\ExperimentSource' {sourceType} -> sourceType) (\s@ExperimentSource' {} a -> s {sourceType = a} :: ExperimentSource)

-- | The Amazon Resource Name (ARN) of the source.
experimentSource_sourceArn :: Lens.Lens' ExperimentSource Core.Text
experimentSource_sourceArn = Lens.lens (\ExperimentSource' {sourceArn} -> sourceArn) (\s@ExperimentSource' {} a -> s {sourceArn = a} :: ExperimentSource)

instance Core.FromJSON ExperimentSource where
  parseJSON =
    Core.withObject
      "ExperimentSource"
      ( \x ->
          ExperimentSource'
            Core.<$> (x Core..:? "SourceType")
            Core.<*> (x Core..: "SourceArn")
      )

instance Core.Hashable ExperimentSource

instance Core.NFData ExperimentSource
