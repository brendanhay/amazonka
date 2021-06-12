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
-- Module      : Network.AWS.MigrationHub.Types.DiscoveredResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.DiscoveredResource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Object representing the on-premises resource being migrated.
--
-- /See:/ 'newDiscoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { -- | A description that can be free-form text to record additional detail
    -- about the discovered resource for clarity or later reference.
    description :: Core.Maybe Core.Text,
    -- | The configurationId in Application Discovery Service that uniquely
    -- identifies the on-premise resource.
    configurationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiscoveredResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'discoveredResource_description' - A description that can be free-form text to record additional detail
-- about the discovered resource for clarity or later reference.
--
-- 'configurationId', 'discoveredResource_configurationId' - The configurationId in Application Discovery Service that uniquely
-- identifies the on-premise resource.
newDiscoveredResource ::
  -- | 'configurationId'
  Core.Text ->
  DiscoveredResource
newDiscoveredResource pConfigurationId_ =
  DiscoveredResource'
    { description = Core.Nothing,
      configurationId = pConfigurationId_
    }

-- | A description that can be free-form text to record additional detail
-- about the discovered resource for clarity or later reference.
discoveredResource_description :: Lens.Lens' DiscoveredResource (Core.Maybe Core.Text)
discoveredResource_description = Lens.lens (\DiscoveredResource' {description} -> description) (\s@DiscoveredResource' {} a -> s {description = a} :: DiscoveredResource)

-- | The configurationId in Application Discovery Service that uniquely
-- identifies the on-premise resource.
discoveredResource_configurationId :: Lens.Lens' DiscoveredResource Core.Text
discoveredResource_configurationId = Lens.lens (\DiscoveredResource' {configurationId} -> configurationId) (\s@DiscoveredResource' {} a -> s {configurationId = a} :: DiscoveredResource)

instance Core.FromJSON DiscoveredResource where
  parseJSON =
    Core.withObject
      "DiscoveredResource"
      ( \x ->
          DiscoveredResource'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..: "ConfigurationId")
      )

instance Core.Hashable DiscoveredResource

instance Core.NFData DiscoveredResource

instance Core.ToJSON DiscoveredResource where
  toJSON DiscoveredResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            Core.Just
              ("ConfigurationId" Core..= configurationId)
          ]
      )
