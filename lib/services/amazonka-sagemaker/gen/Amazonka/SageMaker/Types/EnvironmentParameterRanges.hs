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
-- Module      : Amazonka.SageMaker.Types.EnvironmentParameterRanges
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EnvironmentParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CategoricalParameter

-- | Specifies the range of environment parameters
--
-- /See:/ 'newEnvironmentParameterRanges' smart constructor.
data EnvironmentParameterRanges = EnvironmentParameterRanges'
  { -- | Specified a list of parameters for each category.
    categoricalParameterRanges :: Prelude.Maybe (Prelude.NonEmpty CategoricalParameter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentParameterRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalParameterRanges', 'environmentParameterRanges_categoricalParameterRanges' - Specified a list of parameters for each category.
newEnvironmentParameterRanges ::
  EnvironmentParameterRanges
newEnvironmentParameterRanges =
  EnvironmentParameterRanges'
    { categoricalParameterRanges =
        Prelude.Nothing
    }

-- | Specified a list of parameters for each category.
environmentParameterRanges_categoricalParameterRanges :: Lens.Lens' EnvironmentParameterRanges (Prelude.Maybe (Prelude.NonEmpty CategoricalParameter))
environmentParameterRanges_categoricalParameterRanges = Lens.lens (\EnvironmentParameterRanges' {categoricalParameterRanges} -> categoricalParameterRanges) (\s@EnvironmentParameterRanges' {} a -> s {categoricalParameterRanges = a} :: EnvironmentParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON EnvironmentParameterRanges where
  parseJSON =
    Core.withObject
      "EnvironmentParameterRanges"
      ( \x ->
          EnvironmentParameterRanges'
            Prelude.<$> (x Core..:? "CategoricalParameterRanges")
      )

instance Prelude.Hashable EnvironmentParameterRanges where
  hashWithSalt _salt EnvironmentParameterRanges' {..} =
    _salt
      `Prelude.hashWithSalt` categoricalParameterRanges

instance Prelude.NFData EnvironmentParameterRanges where
  rnf EnvironmentParameterRanges' {..} =
    Prelude.rnf categoricalParameterRanges

instance Core.ToJSON EnvironmentParameterRanges where
  toJSON EnvironmentParameterRanges' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CategoricalParameterRanges" Core..=)
              Prelude.<$> categoricalParameterRanges
          ]
      )
