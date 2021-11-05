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
-- Module      : Network.AWS.ServiceCatalogAppRegistry.Types.Integrations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalogAppRegistry.Types.Integrations where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceGroup

-- | The information about the service integration.
--
-- /See:/ 'newIntegrations' smart constructor.
data Integrations = Integrations'
  { -- | The information about the resource group integration.
    resourceGroup :: Prelude.Maybe ResourceGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Integrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroup', 'integrations_resourceGroup' - The information about the resource group integration.
newIntegrations ::
  Integrations
newIntegrations =
  Integrations' {resourceGroup = Prelude.Nothing}

-- | The information about the resource group integration.
integrations_resourceGroup :: Lens.Lens' Integrations (Prelude.Maybe ResourceGroup)
integrations_resourceGroup = Lens.lens (\Integrations' {resourceGroup} -> resourceGroup) (\s@Integrations' {} a -> s {resourceGroup = a} :: Integrations)

instance Core.FromJSON Integrations where
  parseJSON =
    Core.withObject
      "Integrations"
      ( \x ->
          Integrations'
            Prelude.<$> (x Core..:? "resourceGroup")
      )

instance Prelude.Hashable Integrations

instance Prelude.NFData Integrations
