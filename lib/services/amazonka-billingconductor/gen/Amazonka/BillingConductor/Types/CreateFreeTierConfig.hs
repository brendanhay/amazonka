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
-- Module      : Amazonka.BillingConductor.Types.CreateFreeTierConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CreateFreeTierConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The possible Amazon Web Services Free Tier configurations.
--
-- /See:/ 'newCreateFreeTierConfig' smart constructor.
data CreateFreeTierConfig = CreateFreeTierConfig'
  { -- | Activate or deactivate Amazon Web Services Free Tier.
    activated :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFreeTierConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activated', 'createFreeTierConfig_activated' - Activate or deactivate Amazon Web Services Free Tier.
newCreateFreeTierConfig ::
  -- | 'activated'
  Prelude.Bool ->
  CreateFreeTierConfig
newCreateFreeTierConfig pActivated_ =
  CreateFreeTierConfig' {activated = pActivated_}

-- | Activate or deactivate Amazon Web Services Free Tier.
createFreeTierConfig_activated :: Lens.Lens' CreateFreeTierConfig Prelude.Bool
createFreeTierConfig_activated = Lens.lens (\CreateFreeTierConfig' {activated} -> activated) (\s@CreateFreeTierConfig' {} a -> s {activated = a} :: CreateFreeTierConfig)

instance Prelude.Hashable CreateFreeTierConfig where
  hashWithSalt _salt CreateFreeTierConfig' {..} =
    _salt `Prelude.hashWithSalt` activated

instance Prelude.NFData CreateFreeTierConfig where
  rnf CreateFreeTierConfig' {..} = Prelude.rnf activated

instance Data.ToJSON CreateFreeTierConfig where
  toJSON CreateFreeTierConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Activated" Data..= activated)]
      )
