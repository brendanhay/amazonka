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
-- Module      : Amazonka.FraudDetector.Types.ATIModelPerformance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ATIModelPerformance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Account Takeover Insights (ATI) model performance score.
--
-- /See:/ 'newATIModelPerformance' smart constructor.
data ATIModelPerformance = ATIModelPerformance'
  { -- | The anomaly separation index (ASI) score. This metric summarizes the
    -- overall ability of the model to separate anomalous activities from the
    -- normal behavior. Depending on the business, a large fraction of these
    -- anomalous activities can be malicious and correspond to the account
    -- takeover attacks. A model with no separability power will have the
    -- lowest possible ASI score of 0.5, whereas the a model with a high
    -- separability power will have the highest possible ASI score of 1.0
    asi :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ATIModelPerformance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asi', 'aTIModelPerformance_asi' - The anomaly separation index (ASI) score. This metric summarizes the
-- overall ability of the model to separate anomalous activities from the
-- normal behavior. Depending on the business, a large fraction of these
-- anomalous activities can be malicious and correspond to the account
-- takeover attacks. A model with no separability power will have the
-- lowest possible ASI score of 0.5, whereas the a model with a high
-- separability power will have the highest possible ASI score of 1.0
newATIModelPerformance ::
  ATIModelPerformance
newATIModelPerformance =
  ATIModelPerformance' {asi = Prelude.Nothing}

-- | The anomaly separation index (ASI) score. This metric summarizes the
-- overall ability of the model to separate anomalous activities from the
-- normal behavior. Depending on the business, a large fraction of these
-- anomalous activities can be malicious and correspond to the account
-- takeover attacks. A model with no separability power will have the
-- lowest possible ASI score of 0.5, whereas the a model with a high
-- separability power will have the highest possible ASI score of 1.0
aTIModelPerformance_asi :: Lens.Lens' ATIModelPerformance (Prelude.Maybe Prelude.Double)
aTIModelPerformance_asi = Lens.lens (\ATIModelPerformance' {asi} -> asi) (\s@ATIModelPerformance' {} a -> s {asi = a} :: ATIModelPerformance)

instance Core.FromJSON ATIModelPerformance where
  parseJSON =
    Core.withObject
      "ATIModelPerformance"
      ( \x ->
          ATIModelPerformance' Prelude.<$> (x Core..:? "asi")
      )

instance Prelude.Hashable ATIModelPerformance where
  hashWithSalt _salt ATIModelPerformance' {..} =
    _salt `Prelude.hashWithSalt` asi

instance Prelude.NFData ATIModelPerformance where
  rnf ATIModelPerformance' {..} = Prelude.rnf asi
