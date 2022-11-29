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
-- Module      : Amazonka.Personalize.Types.TunedHPOParams
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.TunedHPOParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | If hyperparameter optimization (HPO) was performed, contains the
-- hyperparameter values of the best performing model.
--
-- /See:/ 'newTunedHPOParams' smart constructor.
data TunedHPOParams = TunedHPOParams'
  { -- | A list of the hyperparameter values of the best performing model.
    algorithmHyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TunedHPOParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmHyperParameters', 'tunedHPOParams_algorithmHyperParameters' - A list of the hyperparameter values of the best performing model.
newTunedHPOParams ::
  TunedHPOParams
newTunedHPOParams =
  TunedHPOParams'
    { algorithmHyperParameters =
        Prelude.Nothing
    }

-- | A list of the hyperparameter values of the best performing model.
tunedHPOParams_algorithmHyperParameters :: Lens.Lens' TunedHPOParams (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
tunedHPOParams_algorithmHyperParameters = Lens.lens (\TunedHPOParams' {algorithmHyperParameters} -> algorithmHyperParameters) (\s@TunedHPOParams' {} a -> s {algorithmHyperParameters = a} :: TunedHPOParams) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TunedHPOParams where
  parseJSON =
    Core.withObject
      "TunedHPOParams"
      ( \x ->
          TunedHPOParams'
            Prelude.<$> ( x Core..:? "algorithmHyperParameters"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TunedHPOParams where
  hashWithSalt _salt TunedHPOParams' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmHyperParameters

instance Prelude.NFData TunedHPOParams where
  rnf TunedHPOParams' {..} =
    Prelude.rnf algorithmHyperParameters
