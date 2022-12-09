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
-- Module      : Amazonka.BillingConductor.Types.UpdateTieringInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.UpdateTieringInput where

import Amazonka.BillingConductor.Types.UpdateFreeTierConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The set of tiering configurations for the pricing rule.
--
-- /See:/ 'newUpdateTieringInput' smart constructor.
data UpdateTieringInput = UpdateTieringInput'
  { -- | The possible Amazon Web Services Free Tier configurations.
    freeTier :: UpdateFreeTierConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTieringInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeTier', 'updateTieringInput_freeTier' - The possible Amazon Web Services Free Tier configurations.
newUpdateTieringInput ::
  -- | 'freeTier'
  UpdateFreeTierConfig ->
  UpdateTieringInput
newUpdateTieringInput pFreeTier_ =
  UpdateTieringInput' {freeTier = pFreeTier_}

-- | The possible Amazon Web Services Free Tier configurations.
updateTieringInput_freeTier :: Lens.Lens' UpdateTieringInput UpdateFreeTierConfig
updateTieringInput_freeTier = Lens.lens (\UpdateTieringInput' {freeTier} -> freeTier) (\s@UpdateTieringInput' {} a -> s {freeTier = a} :: UpdateTieringInput)

instance Data.FromJSON UpdateTieringInput where
  parseJSON =
    Data.withObject
      "UpdateTieringInput"
      ( \x ->
          UpdateTieringInput'
            Prelude.<$> (x Data..: "FreeTier")
      )

instance Prelude.Hashable UpdateTieringInput where
  hashWithSalt _salt UpdateTieringInput' {..} =
    _salt `Prelude.hashWithSalt` freeTier

instance Prelude.NFData UpdateTieringInput where
  rnf UpdateTieringInput' {..} = Prelude.rnf freeTier

instance Data.ToJSON UpdateTieringInput where
  toJSON UpdateTieringInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FreeTier" Data..= freeTier)]
      )
