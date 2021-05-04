{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The source of the experiment.
--
-- /See:/ 'newExperimentSource' smart constructor.
data ExperimentSource = ExperimentSource'
  { -- | The source type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ExperimentSource where
  parseJSON =
    Prelude.withObject
      "ExperimentSource"
      ( \x ->
          ExperimentSource'
            Prelude.<$> (x Prelude..:? "SourceType")
            Prelude.<*> (x Prelude..: "SourceArn")
      )

instance Prelude.Hashable ExperimentSource

instance Prelude.NFData ExperimentSource
