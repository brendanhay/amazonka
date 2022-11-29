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
-- Module      : Amazonka.SageMaker.Types.ExplainerConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ExplainerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ClarifyExplainerConfig

-- | A parameter to activate explainers.
--
-- /See:/ 'newExplainerConfig' smart constructor.
data ExplainerConfig = ExplainerConfig'
  { -- | A member of @ExplainerConfig@ that contains configuration parameters for
    -- the SageMaker Clarify explainer.
    clarifyExplainerConfig :: Prelude.Maybe ClarifyExplainerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplainerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clarifyExplainerConfig', 'explainerConfig_clarifyExplainerConfig' - A member of @ExplainerConfig@ that contains configuration parameters for
-- the SageMaker Clarify explainer.
newExplainerConfig ::
  ExplainerConfig
newExplainerConfig =
  ExplainerConfig'
    { clarifyExplainerConfig =
        Prelude.Nothing
    }

-- | A member of @ExplainerConfig@ that contains configuration parameters for
-- the SageMaker Clarify explainer.
explainerConfig_clarifyExplainerConfig :: Lens.Lens' ExplainerConfig (Prelude.Maybe ClarifyExplainerConfig)
explainerConfig_clarifyExplainerConfig = Lens.lens (\ExplainerConfig' {clarifyExplainerConfig} -> clarifyExplainerConfig) (\s@ExplainerConfig' {} a -> s {clarifyExplainerConfig = a} :: ExplainerConfig)

instance Core.FromJSON ExplainerConfig where
  parseJSON =
    Core.withObject
      "ExplainerConfig"
      ( \x ->
          ExplainerConfig'
            Prelude.<$> (x Core..:? "ClarifyExplainerConfig")
      )

instance Prelude.Hashable ExplainerConfig where
  hashWithSalt _salt ExplainerConfig' {..} =
    _salt `Prelude.hashWithSalt` clarifyExplainerConfig

instance Prelude.NFData ExplainerConfig where
  rnf ExplainerConfig' {..} =
    Prelude.rnf clarifyExplainerConfig

instance Core.ToJSON ExplainerConfig where
  toJSON ExplainerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClarifyExplainerConfig" Core..=)
              Prelude.<$> clarifyExplainerConfig
          ]
      )
