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
-- Module      : Network.AWS.Synthetics.Types.VisualReferenceOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Synthetics.Types.VisualReferenceOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Synthetics.Types.BaseScreenshot

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
  { -- | An array of screenshots that are used as the baseline for comparisons
    -- during visual monitoring.
    baseScreenshots :: Prelude.Maybe [BaseScreenshot],
    -- | The ID of the canary run that produced the screenshots that are used as
    -- the baseline for visual monitoring comparisons during future runs of
    -- this canary.
    baseCanaryRunId :: Prelude.Maybe Prelude.Text
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
-- 'baseScreenshots', 'visualReferenceOutput_baseScreenshots' - An array of screenshots that are used as the baseline for comparisons
-- during visual monitoring.
--
-- 'baseCanaryRunId', 'visualReferenceOutput_baseCanaryRunId' - The ID of the canary run that produced the screenshots that are used as
-- the baseline for visual monitoring comparisons during future runs of
-- this canary.
newVisualReferenceOutput ::
  VisualReferenceOutput
newVisualReferenceOutput =
  VisualReferenceOutput'
    { baseScreenshots =
        Prelude.Nothing,
      baseCanaryRunId = Prelude.Nothing
    }

-- | An array of screenshots that are used as the baseline for comparisons
-- during visual monitoring.
visualReferenceOutput_baseScreenshots :: Lens.Lens' VisualReferenceOutput (Prelude.Maybe [BaseScreenshot])
visualReferenceOutput_baseScreenshots = Lens.lens (\VisualReferenceOutput' {baseScreenshots} -> baseScreenshots) (\s@VisualReferenceOutput' {} a -> s {baseScreenshots = a} :: VisualReferenceOutput) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the canary run that produced the screenshots that are used as
-- the baseline for visual monitoring comparisons during future runs of
-- this canary.
visualReferenceOutput_baseCanaryRunId :: Lens.Lens' VisualReferenceOutput (Prelude.Maybe Prelude.Text)
visualReferenceOutput_baseCanaryRunId = Lens.lens (\VisualReferenceOutput' {baseCanaryRunId} -> baseCanaryRunId) (\s@VisualReferenceOutput' {} a -> s {baseCanaryRunId = a} :: VisualReferenceOutput)

instance Core.FromJSON VisualReferenceOutput where
  parseJSON =
    Core.withObject
      "VisualReferenceOutput"
      ( \x ->
          VisualReferenceOutput'
            Prelude.<$> ( x Core..:? "BaseScreenshots"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BaseCanaryRunId")
      )

instance Prelude.Hashable VisualReferenceOutput

instance Prelude.NFData VisualReferenceOutput
