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
-- Module      : Amazonka.ConnectCampaigns.Types.ProgressiveDialerConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.ProgressiveDialerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Progressive Dialer config
--
-- /See:/ 'newProgressiveDialerConfig' smart constructor.
data ProgressiveDialerConfig = ProgressiveDialerConfig'
  { bandwidthAllocation :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProgressiveDialerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthAllocation', 'progressiveDialerConfig_bandwidthAllocation' - Undocumented member.
newProgressiveDialerConfig ::
  -- | 'bandwidthAllocation'
  Prelude.Double ->
  ProgressiveDialerConfig
newProgressiveDialerConfig pBandwidthAllocation_ =
  ProgressiveDialerConfig'
    { bandwidthAllocation =
        pBandwidthAllocation_
    }

-- | Undocumented member.
progressiveDialerConfig_bandwidthAllocation :: Lens.Lens' ProgressiveDialerConfig Prelude.Double
progressiveDialerConfig_bandwidthAllocation = Lens.lens (\ProgressiveDialerConfig' {bandwidthAllocation} -> bandwidthAllocation) (\s@ProgressiveDialerConfig' {} a -> s {bandwidthAllocation = a} :: ProgressiveDialerConfig)

instance Data.FromJSON ProgressiveDialerConfig where
  parseJSON =
    Data.withObject
      "ProgressiveDialerConfig"
      ( \x ->
          ProgressiveDialerConfig'
            Prelude.<$> (x Data..: "bandwidthAllocation")
      )

instance Prelude.Hashable ProgressiveDialerConfig where
  hashWithSalt _salt ProgressiveDialerConfig' {..} =
    _salt `Prelude.hashWithSalt` bandwidthAllocation

instance Prelude.NFData ProgressiveDialerConfig where
  rnf ProgressiveDialerConfig' {..} =
    Prelude.rnf bandwidthAllocation

instance Data.ToJSON ProgressiveDialerConfig where
  toJSON ProgressiveDialerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("bandwidthAllocation" Data..= bandwidthAllocation)
          ]
      )
