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
-- Module      : Amazonka.BillingConductor.Types.Tiering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.Tiering where

import Amazonka.BillingConductor.Types.FreeTierConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The set of tiering configurations for the pricing rule.
--
-- /See:/ 'newTiering' smart constructor.
data Tiering = Tiering'
  { -- | The possible Amazon Web Services Free Tier configurations.
    freeTier :: FreeTierConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tiering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeTier', 'tiering_freeTier' - The possible Amazon Web Services Free Tier configurations.
newTiering ::
  -- | 'freeTier'
  FreeTierConfig ->
  Tiering
newTiering pFreeTier_ =
  Tiering' {freeTier = pFreeTier_}

-- | The possible Amazon Web Services Free Tier configurations.
tiering_freeTier :: Lens.Lens' Tiering FreeTierConfig
tiering_freeTier = Lens.lens (\Tiering' {freeTier} -> freeTier) (\s@Tiering' {} a -> s {freeTier = a} :: Tiering)

instance Data.FromJSON Tiering where
  parseJSON =
    Data.withObject
      "Tiering"
      (\x -> Tiering' Prelude.<$> (x Data..: "FreeTier"))

instance Prelude.Hashable Tiering where
  hashWithSalt _salt Tiering' {..} =
    _salt `Prelude.hashWithSalt` freeTier

instance Prelude.NFData Tiering where
  rnf Tiering' {..} = Prelude.rnf freeTier
