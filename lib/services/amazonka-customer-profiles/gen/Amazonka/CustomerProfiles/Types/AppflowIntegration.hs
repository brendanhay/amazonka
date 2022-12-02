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
-- Module      : Amazonka.CustomerProfiles.Types.AppflowIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AppflowIntegration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Batch
import Amazonka.CustomerProfiles.Types.FlowDefinition
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for workflow of type @APPFLOW_INTEGRATION@.
--
-- /See:/ 'newAppflowIntegration' smart constructor.
data AppflowIntegration = AppflowIntegration'
  { -- | Batches in workflow of type @APPFLOW_INTEGRATION@.
    batches :: Prelude.Maybe [Batch],
    flowDefinition :: FlowDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppflowIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batches', 'appflowIntegration_batches' - Batches in workflow of type @APPFLOW_INTEGRATION@.
--
-- 'flowDefinition', 'appflowIntegration_flowDefinition' - Undocumented member.
newAppflowIntegration ::
  -- | 'flowDefinition'
  FlowDefinition ->
  AppflowIntegration
newAppflowIntegration pFlowDefinition_ =
  AppflowIntegration'
    { batches = Prelude.Nothing,
      flowDefinition = pFlowDefinition_
    }

-- | Batches in workflow of type @APPFLOW_INTEGRATION@.
appflowIntegration_batches :: Lens.Lens' AppflowIntegration (Prelude.Maybe [Batch])
appflowIntegration_batches = Lens.lens (\AppflowIntegration' {batches} -> batches) (\s@AppflowIntegration' {} a -> s {batches = a} :: AppflowIntegration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
appflowIntegration_flowDefinition :: Lens.Lens' AppflowIntegration FlowDefinition
appflowIntegration_flowDefinition = Lens.lens (\AppflowIntegration' {flowDefinition} -> flowDefinition) (\s@AppflowIntegration' {} a -> s {flowDefinition = a} :: AppflowIntegration)

instance Prelude.Hashable AppflowIntegration where
  hashWithSalt _salt AppflowIntegration' {..} =
    _salt `Prelude.hashWithSalt` batches
      `Prelude.hashWithSalt` flowDefinition

instance Prelude.NFData AppflowIntegration where
  rnf AppflowIntegration' {..} =
    Prelude.rnf batches
      `Prelude.seq` Prelude.rnf flowDefinition

instance Data.ToJSON AppflowIntegration where
  toJSON AppflowIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Batches" Data..=) Prelude.<$> batches,
            Prelude.Just
              ("FlowDefinition" Data..= flowDefinition)
          ]
      )
