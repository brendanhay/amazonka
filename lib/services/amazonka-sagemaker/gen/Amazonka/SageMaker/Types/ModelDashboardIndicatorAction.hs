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
-- Module      : Amazonka.SageMaker.Types.ModelDashboardIndicatorAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDashboardIndicatorAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An alert action taken to light up an icon on the Amazon SageMaker Model
-- Dashboard when an alert goes into @InAlert@ status.
--
-- /See:/ 'newModelDashboardIndicatorAction' smart constructor.
data ModelDashboardIndicatorAction = ModelDashboardIndicatorAction'
  { -- | Indicates whether the alert action is turned on.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDashboardIndicatorAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'modelDashboardIndicatorAction_enabled' - Indicates whether the alert action is turned on.
newModelDashboardIndicatorAction ::
  ModelDashboardIndicatorAction
newModelDashboardIndicatorAction =
  ModelDashboardIndicatorAction'
    { enabled =
        Prelude.Nothing
    }

-- | Indicates whether the alert action is turned on.
modelDashboardIndicatorAction_enabled :: Lens.Lens' ModelDashboardIndicatorAction (Prelude.Maybe Prelude.Bool)
modelDashboardIndicatorAction_enabled = Lens.lens (\ModelDashboardIndicatorAction' {enabled} -> enabled) (\s@ModelDashboardIndicatorAction' {} a -> s {enabled = a} :: ModelDashboardIndicatorAction)

instance Data.FromJSON ModelDashboardIndicatorAction where
  parseJSON =
    Data.withObject
      "ModelDashboardIndicatorAction"
      ( \x ->
          ModelDashboardIndicatorAction'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    ModelDashboardIndicatorAction
  where
  hashWithSalt _salt ModelDashboardIndicatorAction' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData ModelDashboardIndicatorAction where
  rnf ModelDashboardIndicatorAction' {..} =
    Prelude.rnf enabled
