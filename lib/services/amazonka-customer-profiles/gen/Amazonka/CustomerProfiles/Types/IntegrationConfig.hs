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
-- Module      : Amazonka.CustomerProfiles.Types.IntegrationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.IntegrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.AppflowIntegration
import qualified Amazonka.Prelude as Prelude

-- | Configuration data for integration workflow.
--
-- /See:/ 'newIntegrationConfig' smart constructor.
data IntegrationConfig = IntegrationConfig'
  { -- | Configuration data for @APPFLOW_INTEGRATION@ workflow type.
    appflowIntegration :: Prelude.Maybe AppflowIntegration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appflowIntegration', 'integrationConfig_appflowIntegration' - Configuration data for @APPFLOW_INTEGRATION@ workflow type.
newIntegrationConfig ::
  IntegrationConfig
newIntegrationConfig =
  IntegrationConfig'
    { appflowIntegration =
        Prelude.Nothing
    }

-- | Configuration data for @APPFLOW_INTEGRATION@ workflow type.
integrationConfig_appflowIntegration :: Lens.Lens' IntegrationConfig (Prelude.Maybe AppflowIntegration)
integrationConfig_appflowIntegration = Lens.lens (\IntegrationConfig' {appflowIntegration} -> appflowIntegration) (\s@IntegrationConfig' {} a -> s {appflowIntegration = a} :: IntegrationConfig)

instance Prelude.Hashable IntegrationConfig where
  hashWithSalt _salt IntegrationConfig' {..} =
    _salt `Prelude.hashWithSalt` appflowIntegration

instance Prelude.NFData IntegrationConfig where
  rnf IntegrationConfig' {..} =
    Prelude.rnf appflowIntegration

instance Core.ToJSON IntegrationConfig where
  toJSON IntegrationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AppflowIntegration" Core..=)
              Prelude.<$> appflowIntegration
          ]
      )
