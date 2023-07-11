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
-- Module      : Amazonka.Synthetics.Types.VisualReferenceOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.VisualReferenceOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.BaseScreenshot

-- | If this canary performs visual monitoring by comparing screenshots, this
-- structure contains the ID of the canary run that is used as the baseline
-- for screenshots, and the coordinates of any parts of those screenshots
-- that are ignored during visual monitoring comparison.
--
-- Visual monitoring is supported only on canaries running the
-- __syn-puppeteer-node-3.2__ runtime or later.
--
-- /See:/ 'newVisualReferenceOutput' smart constructor.
data VisualReferenceOutput = VisualReferenceOutput'
  { -- | The ID of the canary run that produced the baseline screenshots that are
    -- used for visual monitoring comparisons by this canary.
    baseCanaryRunId :: Prelude.Maybe Prelude.Text,
    -- | An array of screenshots that are used as the baseline for comparisons
    -- during visual monitoring.
    baseScreenshots :: Prelude.Maybe [BaseScreenshot]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualReferenceOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseCanaryRunId', 'visualReferenceOutput_baseCanaryRunId' - The ID of the canary run that produced the baseline screenshots that are
-- used for visual monitoring comparisons by this canary.
--
-- 'baseScreenshots', 'visualReferenceOutput_baseScreenshots' - An array of screenshots that are used as the baseline for comparisons
-- during visual monitoring.
newVisualReferenceOutput ::
  VisualReferenceOutput
newVisualReferenceOutput =
  VisualReferenceOutput'
    { baseCanaryRunId =
        Prelude.Nothing,
      baseScreenshots = Prelude.Nothing
    }

-- | The ID of the canary run that produced the baseline screenshots that are
-- used for visual monitoring comparisons by this canary.
visualReferenceOutput_baseCanaryRunId :: Lens.Lens' VisualReferenceOutput (Prelude.Maybe Prelude.Text)
visualReferenceOutput_baseCanaryRunId = Lens.lens (\VisualReferenceOutput' {baseCanaryRunId} -> baseCanaryRunId) (\s@VisualReferenceOutput' {} a -> s {baseCanaryRunId = a} :: VisualReferenceOutput)

-- | An array of screenshots that are used as the baseline for comparisons
-- during visual monitoring.
visualReferenceOutput_baseScreenshots :: Lens.Lens' VisualReferenceOutput (Prelude.Maybe [BaseScreenshot])
visualReferenceOutput_baseScreenshots = Lens.lens (\VisualReferenceOutput' {baseScreenshots} -> baseScreenshots) (\s@VisualReferenceOutput' {} a -> s {baseScreenshots = a} :: VisualReferenceOutput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VisualReferenceOutput where
  parseJSON =
    Data.withObject
      "VisualReferenceOutput"
      ( \x ->
          VisualReferenceOutput'
            Prelude.<$> (x Data..:? "BaseCanaryRunId")
            Prelude.<*> ( x
                            Data..:? "BaseScreenshots"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VisualReferenceOutput where
  hashWithSalt _salt VisualReferenceOutput' {..} =
    _salt
      `Prelude.hashWithSalt` baseCanaryRunId
      `Prelude.hashWithSalt` baseScreenshots

instance Prelude.NFData VisualReferenceOutput where
  rnf VisualReferenceOutput' {..} =
    Prelude.rnf baseCanaryRunId
      `Prelude.seq` Prelude.rnf baseScreenshots
