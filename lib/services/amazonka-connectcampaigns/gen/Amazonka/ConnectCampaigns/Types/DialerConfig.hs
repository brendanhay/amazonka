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
-- Module      : Amazonka.ConnectCampaigns.Types.DialerConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.DialerConfig where

import Amazonka.ConnectCampaigns.Types.PredictiveDialerConfig
import Amazonka.ConnectCampaigns.Types.ProgressiveDialerConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The possible types of dialer config parameters
--
-- /See:/ 'newDialerConfig' smart constructor.
data DialerConfig = DialerConfig'
  { predictiveDialerConfig :: Prelude.Maybe PredictiveDialerConfig,
    progressiveDialerConfig :: Prelude.Maybe ProgressiveDialerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DialerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictiveDialerConfig', 'dialerConfig_predictiveDialerConfig' - Undocumented member.
--
-- 'progressiveDialerConfig', 'dialerConfig_progressiveDialerConfig' - Undocumented member.
newDialerConfig ::
  DialerConfig
newDialerConfig =
  DialerConfig'
    { predictiveDialerConfig =
        Prelude.Nothing,
      progressiveDialerConfig = Prelude.Nothing
    }

-- | Undocumented member.
dialerConfig_predictiveDialerConfig :: Lens.Lens' DialerConfig (Prelude.Maybe PredictiveDialerConfig)
dialerConfig_predictiveDialerConfig = Lens.lens (\DialerConfig' {predictiveDialerConfig} -> predictiveDialerConfig) (\s@DialerConfig' {} a -> s {predictiveDialerConfig = a} :: DialerConfig)

-- | Undocumented member.
dialerConfig_progressiveDialerConfig :: Lens.Lens' DialerConfig (Prelude.Maybe ProgressiveDialerConfig)
dialerConfig_progressiveDialerConfig = Lens.lens (\DialerConfig' {progressiveDialerConfig} -> progressiveDialerConfig) (\s@DialerConfig' {} a -> s {progressiveDialerConfig = a} :: DialerConfig)

instance Data.FromJSON DialerConfig where
  parseJSON =
    Data.withObject
      "DialerConfig"
      ( \x ->
          DialerConfig'
            Prelude.<$> (x Data..:? "predictiveDialerConfig")
            Prelude.<*> (x Data..:? "progressiveDialerConfig")
      )

instance Prelude.Hashable DialerConfig where
  hashWithSalt _salt DialerConfig' {..} =
    _salt `Prelude.hashWithSalt` predictiveDialerConfig
      `Prelude.hashWithSalt` progressiveDialerConfig

instance Prelude.NFData DialerConfig where
  rnf DialerConfig' {..} =
    Prelude.rnf predictiveDialerConfig
      `Prelude.seq` Prelude.rnf progressiveDialerConfig

instance Data.ToJSON DialerConfig where
  toJSON DialerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("predictiveDialerConfig" Data..=)
              Prelude.<$> predictiveDialerConfig,
            ("progressiveDialerConfig" Data..=)
              Prelude.<$> progressiveDialerConfig
          ]
      )
