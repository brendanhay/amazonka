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
-- Module      : Amazonka.ConnectCampaigns.Types.PredictiveDialerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.PredictiveDialerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Predictive Dialer config
--
-- /See:/ 'newPredictiveDialerConfig' smart constructor.
data PredictiveDialerConfig = PredictiveDialerConfig'
  { bandwidthAllocation :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveDialerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthAllocation', 'predictiveDialerConfig_bandwidthAllocation' - Undocumented member.
newPredictiveDialerConfig ::
  -- | 'bandwidthAllocation'
  Prelude.Double ->
  PredictiveDialerConfig
newPredictiveDialerConfig pBandwidthAllocation_ =
  PredictiveDialerConfig'
    { bandwidthAllocation =
        pBandwidthAllocation_
    }

-- | Undocumented member.
predictiveDialerConfig_bandwidthAllocation :: Lens.Lens' PredictiveDialerConfig Prelude.Double
predictiveDialerConfig_bandwidthAllocation = Lens.lens (\PredictiveDialerConfig' {bandwidthAllocation} -> bandwidthAllocation) (\s@PredictiveDialerConfig' {} a -> s {bandwidthAllocation = a} :: PredictiveDialerConfig)

instance Data.FromJSON PredictiveDialerConfig where
  parseJSON =
    Data.withObject
      "PredictiveDialerConfig"
      ( \x ->
          PredictiveDialerConfig'
            Prelude.<$> (x Data..: "bandwidthAllocation")
      )

instance Prelude.Hashable PredictiveDialerConfig where
  hashWithSalt _salt PredictiveDialerConfig' {..} =
    _salt `Prelude.hashWithSalt` bandwidthAllocation

instance Prelude.NFData PredictiveDialerConfig where
  rnf PredictiveDialerConfig' {..} =
    Prelude.rnf bandwidthAllocation

instance Data.ToJSON PredictiveDialerConfig where
  toJSON PredictiveDialerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("bandwidthAllocation" Data..= bandwidthAllocation)
          ]
      )
