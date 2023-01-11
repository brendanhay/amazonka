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
-- Module      : Amazonka.Connect.Types.TelephonyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TelephonyConfig where

import Amazonka.Connect.Types.Distribution
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The distribution of traffic between the instance and its replicas.
--
-- /See:/ 'newTelephonyConfig' smart constructor.
data TelephonyConfig = TelephonyConfig'
  { -- | Information about traffic distributions.
    distributions :: [Distribution]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TelephonyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributions', 'telephonyConfig_distributions' - Information about traffic distributions.
newTelephonyConfig ::
  TelephonyConfig
newTelephonyConfig =
  TelephonyConfig' {distributions = Prelude.mempty}

-- | Information about traffic distributions.
telephonyConfig_distributions :: Lens.Lens' TelephonyConfig [Distribution]
telephonyConfig_distributions = Lens.lens (\TelephonyConfig' {distributions} -> distributions) (\s@TelephonyConfig' {} a -> s {distributions = a} :: TelephonyConfig) Prelude.. Lens.coerced

instance Data.FromJSON TelephonyConfig where
  parseJSON =
    Data.withObject
      "TelephonyConfig"
      ( \x ->
          TelephonyConfig'
            Prelude.<$> (x Data..:? "Distributions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TelephonyConfig where
  hashWithSalt _salt TelephonyConfig' {..} =
    _salt `Prelude.hashWithSalt` distributions

instance Prelude.NFData TelephonyConfig where
  rnf TelephonyConfig' {..} = Prelude.rnf distributions

instance Data.ToJSON TelephonyConfig where
  toJSON TelephonyConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("Distributions" Data..= distributions)
          ]
      )
