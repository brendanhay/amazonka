{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Object representing the on-premises resource being migrated.
--
-- /See:/ 'newDiscoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { -- | A description that can be free-form text to record additional detail
    -- about the discovered resource for clarity or later reference.
    description :: Prelude.Maybe Prelude.Text,
    -- | The configurationId in Application Discovery Service that uniquely
    -- identifies the on-premise resource.
    configurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DiscoveredResource
newDiscoveredResource pConfigurationId_ =
  DiscoveredResource'
    { description = Prelude.Nothing,
      configurationId = pConfigurationId_
    }

-- | A description that can be free-form text to record additional detail
-- about the discovered resource for clarity or later reference.
discoveredResource_description :: Lens.Lens' DiscoveredResource (Prelude.Maybe Prelude.Text)
discoveredResource_description = Lens.lens (\DiscoveredResource' {description} -> description) (\s@DiscoveredResource' {} a -> s {description = a} :: DiscoveredResource)

-- | The configurationId in Application Discovery Service that uniquely
-- identifies the on-premise resource.
discoveredResource_configurationId :: Lens.Lens' DiscoveredResource Prelude.Text
discoveredResource_configurationId = Lens.lens (\DiscoveredResource' {configurationId} -> configurationId) (\s@DiscoveredResource' {} a -> s {configurationId = a} :: DiscoveredResource)

instance Prelude.FromJSON DiscoveredResource where
  parseJSON =
    Prelude.withObject
      "DiscoveredResource"
      ( \x ->
          DiscoveredResource'
            Prelude.<$> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..: "ConfigurationId")
      )

instance Prelude.Hashable DiscoveredResource

instance Prelude.NFData DiscoveredResource

instance Prelude.ToJSON DiscoveredResource where
  toJSON DiscoveredResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just
              ("ConfigurationId" Prelude..= configurationId)
          ]
      )
