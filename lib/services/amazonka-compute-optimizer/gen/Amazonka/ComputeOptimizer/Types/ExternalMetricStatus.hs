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
-- Module      : Amazonka.ComputeOptimizer.Types.ExternalMetricStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExternalMetricStatus where

import Amazonka.ComputeOptimizer.Types.ExternalMetricStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes Compute Optimizer\'s integration status with your chosen
-- external metric provider. For example, Datadog.
--
-- /See:/ 'newExternalMetricStatus' smart constructor.
data ExternalMetricStatus = ExternalMetricStatus'
  { -- | The status code for Compute Optimizer\'s integration with an external
    -- metrics provider.
    statusCode :: Prelude.Maybe ExternalMetricStatusCode,
    -- | The reason for Compute Optimizer\'s integration status with your
    -- external metric provider.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalMetricStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'externalMetricStatus_statusCode' - The status code for Compute Optimizer\'s integration with an external
-- metrics provider.
--
-- 'statusReason', 'externalMetricStatus_statusReason' - The reason for Compute Optimizer\'s integration status with your
-- external metric provider.
newExternalMetricStatus ::
  ExternalMetricStatus
newExternalMetricStatus =
  ExternalMetricStatus'
    { statusCode = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The status code for Compute Optimizer\'s integration with an external
-- metrics provider.
externalMetricStatus_statusCode :: Lens.Lens' ExternalMetricStatus (Prelude.Maybe ExternalMetricStatusCode)
externalMetricStatus_statusCode = Lens.lens (\ExternalMetricStatus' {statusCode} -> statusCode) (\s@ExternalMetricStatus' {} a -> s {statusCode = a} :: ExternalMetricStatus)

-- | The reason for Compute Optimizer\'s integration status with your
-- external metric provider.
externalMetricStatus_statusReason :: Lens.Lens' ExternalMetricStatus (Prelude.Maybe Prelude.Text)
externalMetricStatus_statusReason = Lens.lens (\ExternalMetricStatus' {statusReason} -> statusReason) (\s@ExternalMetricStatus' {} a -> s {statusReason = a} :: ExternalMetricStatus)

instance Data.FromJSON ExternalMetricStatus where
  parseJSON =
    Data.withObject
      "ExternalMetricStatus"
      ( \x ->
          ExternalMetricStatus'
            Prelude.<$> (x Data..:? "statusCode")
            Prelude.<*> (x Data..:? "statusReason")
      )

instance Prelude.Hashable ExternalMetricStatus where
  hashWithSalt _salt ExternalMetricStatus' {..} =
    _salt
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData ExternalMetricStatus where
  rnf ExternalMetricStatus' {..} =
    Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusReason
