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
-- Module      : Amazonka.BillingConductor.Types.CreateTieringInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CreateTieringInput where

import Amazonka.BillingConductor.Types.CreateFreeTierConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The set of tiering configurations for the pricing rule.
--
-- /See:/ 'newCreateTieringInput' smart constructor.
data CreateTieringInput = CreateTieringInput'
  { -- | The possible Amazon Web Services Free Tier configurations.
    freeTier :: CreateFreeTierConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTieringInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeTier', 'createTieringInput_freeTier' - The possible Amazon Web Services Free Tier configurations.
newCreateTieringInput ::
  -- | 'freeTier'
  CreateFreeTierConfig ->
  CreateTieringInput
newCreateTieringInput pFreeTier_ =
  CreateTieringInput' {freeTier = pFreeTier_}

-- | The possible Amazon Web Services Free Tier configurations.
createTieringInput_freeTier :: Lens.Lens' CreateTieringInput CreateFreeTierConfig
createTieringInput_freeTier = Lens.lens (\CreateTieringInput' {freeTier} -> freeTier) (\s@CreateTieringInput' {} a -> s {freeTier = a} :: CreateTieringInput)

instance Prelude.Hashable CreateTieringInput where
  hashWithSalt _salt CreateTieringInput' {..} =
    _salt `Prelude.hashWithSalt` freeTier

instance Prelude.NFData CreateTieringInput where
  rnf CreateTieringInput' {..} = Prelude.rnf freeTier

instance Data.ToJSON CreateTieringInput where
  toJSON CreateTieringInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FreeTier" Data..= freeTier)]
      )
