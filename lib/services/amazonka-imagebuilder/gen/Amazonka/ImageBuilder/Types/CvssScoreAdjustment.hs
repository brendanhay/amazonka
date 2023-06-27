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
-- Module      : Amazonka.ImageBuilder.Types.CvssScoreAdjustment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.CvssScoreAdjustment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an adjustment that Amazon Inspector made to the CVSS score
-- for a finding.
--
-- /See:/ 'newCvssScoreAdjustment' smart constructor.
data CvssScoreAdjustment = CvssScoreAdjustment'
  { -- | The metric that Amazon Inspector used to adjust the CVSS score.
    metric :: Prelude.Maybe Prelude.Text,
    -- | The reason for the CVSS score adjustment.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CvssScoreAdjustment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metric', 'cvssScoreAdjustment_metric' - The metric that Amazon Inspector used to adjust the CVSS score.
--
-- 'reason', 'cvssScoreAdjustment_reason' - The reason for the CVSS score adjustment.
newCvssScoreAdjustment ::
  CvssScoreAdjustment
newCvssScoreAdjustment =
  CvssScoreAdjustment'
    { metric = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The metric that Amazon Inspector used to adjust the CVSS score.
cvssScoreAdjustment_metric :: Lens.Lens' CvssScoreAdjustment (Prelude.Maybe Prelude.Text)
cvssScoreAdjustment_metric = Lens.lens (\CvssScoreAdjustment' {metric} -> metric) (\s@CvssScoreAdjustment' {} a -> s {metric = a} :: CvssScoreAdjustment)

-- | The reason for the CVSS score adjustment.
cvssScoreAdjustment_reason :: Lens.Lens' CvssScoreAdjustment (Prelude.Maybe Prelude.Text)
cvssScoreAdjustment_reason = Lens.lens (\CvssScoreAdjustment' {reason} -> reason) (\s@CvssScoreAdjustment' {} a -> s {reason = a} :: CvssScoreAdjustment)

instance Data.FromJSON CvssScoreAdjustment where
  parseJSON =
    Data.withObject
      "CvssScoreAdjustment"
      ( \x ->
          CvssScoreAdjustment'
            Prelude.<$> (x Data..:? "metric")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable CvssScoreAdjustment where
  hashWithSalt _salt CvssScoreAdjustment' {..} =
    _salt
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` reason

instance Prelude.NFData CvssScoreAdjustment where
  rnf CvssScoreAdjustment' {..} =
    Prelude.rnf metric `Prelude.seq` Prelude.rnf reason
