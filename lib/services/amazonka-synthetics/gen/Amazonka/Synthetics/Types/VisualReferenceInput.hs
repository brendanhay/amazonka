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
-- Module      : Amazonka.Synthetics.Types.VisualReferenceInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.VisualReferenceInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.BaseScreenshot

-- | An object that specifies what screenshots to use as a baseline for
-- visual monitoring by this canary. It can optionally also specify parts
-- of the screenshots to ignore during the visual monitoring comparison.
--
-- Visual monitoring is supported only on canaries running the
-- __syn-puppeteer-node-3.2__ runtime or later. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Library_SyntheticsLogger_VisualTesting.html Visual monitoring>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Blueprints_VisualTesting.html Visual monitoring blueprint>
--
-- /See:/ 'newVisualReferenceInput' smart constructor.
data VisualReferenceInput = VisualReferenceInput'
  { -- | An array of screenshots that will be used as the baseline for visual
    -- monitoring in future runs of this canary. If there is a screenshot that
    -- you don\'t want to be used for visual monitoring, remove it from this
    -- array.
    baseScreenshots :: Prelude.Maybe [BaseScreenshot],
    -- | Specifies which canary run to use the screenshots from as the baseline
    -- for future visual monitoring with this canary. Valid values are
    -- @nextrun@ to use the screenshots from the next run after this update is
    -- made, @lastrun@ to use the screenshots from the most recent run before
    -- this update was made, or the value of @Id@ in the
    -- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_CanaryRun.html CanaryRun>
    -- from any past run of this canary.
    baseCanaryRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualReferenceInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseScreenshots', 'visualReferenceInput_baseScreenshots' - An array of screenshots that will be used as the baseline for visual
-- monitoring in future runs of this canary. If there is a screenshot that
-- you don\'t want to be used for visual monitoring, remove it from this
-- array.
--
-- 'baseCanaryRunId', 'visualReferenceInput_baseCanaryRunId' - Specifies which canary run to use the screenshots from as the baseline
-- for future visual monitoring with this canary. Valid values are
-- @nextrun@ to use the screenshots from the next run after this update is
-- made, @lastrun@ to use the screenshots from the most recent run before
-- this update was made, or the value of @Id@ in the
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_CanaryRun.html CanaryRun>
-- from any past run of this canary.
newVisualReferenceInput ::
  -- | 'baseCanaryRunId'
  Prelude.Text ->
  VisualReferenceInput
newVisualReferenceInput pBaseCanaryRunId_ =
  VisualReferenceInput'
    { baseScreenshots =
        Prelude.Nothing,
      baseCanaryRunId = pBaseCanaryRunId_
    }

-- | An array of screenshots that will be used as the baseline for visual
-- monitoring in future runs of this canary. If there is a screenshot that
-- you don\'t want to be used for visual monitoring, remove it from this
-- array.
visualReferenceInput_baseScreenshots :: Lens.Lens' VisualReferenceInput (Prelude.Maybe [BaseScreenshot])
visualReferenceInput_baseScreenshots = Lens.lens (\VisualReferenceInput' {baseScreenshots} -> baseScreenshots) (\s@VisualReferenceInput' {} a -> s {baseScreenshots = a} :: VisualReferenceInput) Prelude.. Lens.mapping Lens.coerced

-- | Specifies which canary run to use the screenshots from as the baseline
-- for future visual monitoring with this canary. Valid values are
-- @nextrun@ to use the screenshots from the next run after this update is
-- made, @lastrun@ to use the screenshots from the most recent run before
-- this update was made, or the value of @Id@ in the
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_CanaryRun.html CanaryRun>
-- from any past run of this canary.
visualReferenceInput_baseCanaryRunId :: Lens.Lens' VisualReferenceInput Prelude.Text
visualReferenceInput_baseCanaryRunId = Lens.lens (\VisualReferenceInput' {baseCanaryRunId} -> baseCanaryRunId) (\s@VisualReferenceInput' {} a -> s {baseCanaryRunId = a} :: VisualReferenceInput)

instance Prelude.Hashable VisualReferenceInput where
  hashWithSalt _salt VisualReferenceInput' {..} =
    _salt `Prelude.hashWithSalt` baseScreenshots
      `Prelude.hashWithSalt` baseCanaryRunId

instance Prelude.NFData VisualReferenceInput where
  rnf VisualReferenceInput' {..} =
    Prelude.rnf baseScreenshots
      `Prelude.seq` Prelude.rnf baseCanaryRunId

instance Data.ToJSON VisualReferenceInput where
  toJSON VisualReferenceInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaseScreenshots" Data..=)
              Prelude.<$> baseScreenshots,
            Prelude.Just
              ("BaseCanaryRunId" Data..= baseCanaryRunId)
          ]
      )
