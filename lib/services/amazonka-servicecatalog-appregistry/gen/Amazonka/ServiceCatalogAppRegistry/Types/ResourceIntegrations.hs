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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ResourceIntegrations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ResourceIntegrations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroup

-- | The service integration information about the resource.
--
-- /See:/ 'newResourceIntegrations' smart constructor.
data ResourceIntegrations = ResourceIntegrations'
  { -- | The information about the integration of Resource Groups.
    resourceGroup :: Prelude.Maybe ResourceGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceIntegrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroup', 'resourceIntegrations_resourceGroup' - The information about the integration of Resource Groups.
newResourceIntegrations ::
  ResourceIntegrations
newResourceIntegrations =
  ResourceIntegrations'
    { resourceGroup =
        Prelude.Nothing
    }

-- | The information about the integration of Resource Groups.
resourceIntegrations_resourceGroup :: Lens.Lens' ResourceIntegrations (Prelude.Maybe ResourceGroup)
resourceIntegrations_resourceGroup = Lens.lens (\ResourceIntegrations' {resourceGroup} -> resourceGroup) (\s@ResourceIntegrations' {} a -> s {resourceGroup = a} :: ResourceIntegrations)

instance Data.FromJSON ResourceIntegrations where
  parseJSON =
    Data.withObject
      "ResourceIntegrations"
      ( \x ->
          ResourceIntegrations'
            Prelude.<$> (x Data..:? "resourceGroup")
      )

instance Prelude.Hashable ResourceIntegrations where
  hashWithSalt _salt ResourceIntegrations' {..} =
    _salt `Prelude.hashWithSalt` resourceGroup

instance Prelude.NFData ResourceIntegrations where
  rnf ResourceIntegrations' {..} =
    Prelude.rnf resourceGroup
