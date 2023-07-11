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
-- Module      : Amazonka.SageMaker.Types.TrialSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source of the trial.
--
-- /See:/ 'newTrialSource' smart constructor.
data TrialSource = TrialSource'
  { -- | The source job type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'trialSource_sourceType' - The source job type.
--
-- 'sourceArn', 'trialSource_sourceArn' - The Amazon Resource Name (ARN) of the source.
newTrialSource ::
  -- | 'sourceArn'
  Prelude.Text ->
  TrialSource
newTrialSource pSourceArn_ =
  TrialSource'
    { sourceType = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The source job type.
trialSource_sourceType :: Lens.Lens' TrialSource (Prelude.Maybe Prelude.Text)
trialSource_sourceType = Lens.lens (\TrialSource' {sourceType} -> sourceType) (\s@TrialSource' {} a -> s {sourceType = a} :: TrialSource)

-- | The Amazon Resource Name (ARN) of the source.
trialSource_sourceArn :: Lens.Lens' TrialSource Prelude.Text
trialSource_sourceArn = Lens.lens (\TrialSource' {sourceArn} -> sourceArn) (\s@TrialSource' {} a -> s {sourceArn = a} :: TrialSource)

instance Data.FromJSON TrialSource where
  parseJSON =
    Data.withObject
      "TrialSource"
      ( \x ->
          TrialSource'
            Prelude.<$> (x Data..:? "SourceType")
            Prelude.<*> (x Data..: "SourceArn")
      )

instance Prelude.Hashable TrialSource where
  hashWithSalt _salt TrialSource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData TrialSource where
  rnf TrialSource' {..} =
    Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceArn
