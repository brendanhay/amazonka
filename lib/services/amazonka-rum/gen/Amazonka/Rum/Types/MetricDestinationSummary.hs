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
-- Module      : Amazonka.Rum.Types.MetricDestinationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.MetricDestinationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.MetricDestination

-- | A structure that displays information about one destination that
-- CloudWatch RUM sends extended metrics to.
--
-- /See:/ 'newMetricDestinationSummary' smart constructor.
data MetricDestinationSummary = MetricDestinationSummary'
  { -- | Specifies whether the destination is @CloudWatch@ or @Evidently@.
    destination :: Prelude.Maybe MetricDestination,
    -- | If the destination is @Evidently@, this specifies the ARN of the
    -- Evidently experiment that receives the metrics.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | This field appears only when the destination is @Evidently@. It
    -- specifies the ARN of the IAM role that is used to write to the Evidently
    -- experiment that receives the metrics.
    iamRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDestinationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'metricDestinationSummary_destination' - Specifies whether the destination is @CloudWatch@ or @Evidently@.
--
-- 'destinationArn', 'metricDestinationSummary_destinationArn' - If the destination is @Evidently@, this specifies the ARN of the
-- Evidently experiment that receives the metrics.
--
-- 'iamRoleArn', 'metricDestinationSummary_iamRoleArn' - This field appears only when the destination is @Evidently@. It
-- specifies the ARN of the IAM role that is used to write to the Evidently
-- experiment that receives the metrics.
newMetricDestinationSummary ::
  MetricDestinationSummary
newMetricDestinationSummary =
  MetricDestinationSummary'
    { destination =
        Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing
    }

-- | Specifies whether the destination is @CloudWatch@ or @Evidently@.
metricDestinationSummary_destination :: Lens.Lens' MetricDestinationSummary (Prelude.Maybe MetricDestination)
metricDestinationSummary_destination = Lens.lens (\MetricDestinationSummary' {destination} -> destination) (\s@MetricDestinationSummary' {} a -> s {destination = a} :: MetricDestinationSummary)

-- | If the destination is @Evidently@, this specifies the ARN of the
-- Evidently experiment that receives the metrics.
metricDestinationSummary_destinationArn :: Lens.Lens' MetricDestinationSummary (Prelude.Maybe Prelude.Text)
metricDestinationSummary_destinationArn = Lens.lens (\MetricDestinationSummary' {destinationArn} -> destinationArn) (\s@MetricDestinationSummary' {} a -> s {destinationArn = a} :: MetricDestinationSummary)

-- | This field appears only when the destination is @Evidently@. It
-- specifies the ARN of the IAM role that is used to write to the Evidently
-- experiment that receives the metrics.
metricDestinationSummary_iamRoleArn :: Lens.Lens' MetricDestinationSummary (Prelude.Maybe Prelude.Text)
metricDestinationSummary_iamRoleArn = Lens.lens (\MetricDestinationSummary' {iamRoleArn} -> iamRoleArn) (\s@MetricDestinationSummary' {} a -> s {iamRoleArn = a} :: MetricDestinationSummary)

instance Data.FromJSON MetricDestinationSummary where
  parseJSON =
    Data.withObject
      "MetricDestinationSummary"
      ( \x ->
          MetricDestinationSummary'
            Prelude.<$> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "DestinationArn")
            Prelude.<*> (x Data..:? "IamRoleArn")
      )

instance Prelude.Hashable MetricDestinationSummary where
  hashWithSalt _salt MetricDestinationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData MetricDestinationSummary where
  rnf MetricDestinationSummary' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf iamRoleArn
