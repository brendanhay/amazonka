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
-- Module      : Amazonka.BillingConductor.Types.UpdateFreeTierConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.UpdateFreeTierConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The possible Amazon Web Services Free Tier configurations.
--
-- /See:/ 'newUpdateFreeTierConfig' smart constructor.
data UpdateFreeTierConfig = UpdateFreeTierConfig'
  { -- | Activate or deactivate application of Amazon Web Services Free Tier.
    activated :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFreeTierConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activated', 'updateFreeTierConfig_activated' - Activate or deactivate application of Amazon Web Services Free Tier.
newUpdateFreeTierConfig ::
  -- | 'activated'
  Prelude.Bool ->
  UpdateFreeTierConfig
newUpdateFreeTierConfig pActivated_ =
  UpdateFreeTierConfig' {activated = pActivated_}

-- | Activate or deactivate application of Amazon Web Services Free Tier.
updateFreeTierConfig_activated :: Lens.Lens' UpdateFreeTierConfig Prelude.Bool
updateFreeTierConfig_activated = Lens.lens (\UpdateFreeTierConfig' {activated} -> activated) (\s@UpdateFreeTierConfig' {} a -> s {activated = a} :: UpdateFreeTierConfig)

instance Data.FromJSON UpdateFreeTierConfig where
  parseJSON =
    Data.withObject
      "UpdateFreeTierConfig"
      ( \x ->
          UpdateFreeTierConfig'
            Prelude.<$> (x Data..: "Activated")
      )

instance Prelude.Hashable UpdateFreeTierConfig where
  hashWithSalt _salt UpdateFreeTierConfig' {..} =
    _salt `Prelude.hashWithSalt` activated

instance Prelude.NFData UpdateFreeTierConfig where
  rnf UpdateFreeTierConfig' {..} = Prelude.rnf activated

instance Data.ToJSON UpdateFreeTierConfig where
  toJSON UpdateFreeTierConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Activated" Data..= activated)]
      )
