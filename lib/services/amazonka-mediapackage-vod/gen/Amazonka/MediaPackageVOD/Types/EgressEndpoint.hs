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
-- Module      : Amazonka.MediaPackageVOD.Types.EgressEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.EgressEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The endpoint URL used to access an Asset using one
-- PackagingConfiguration.
--
-- /See:/ 'newEgressEndpoint' smart constructor.
data EgressEndpoint = EgressEndpoint'
  { -- | The current processing status of the asset used for the packaging
    -- configuration. The status can be either QUEUED, PROCESSING, PLAYABLE, or
    -- FAILED. Status information won\'t be available for most assets ingested
    -- before 2021-09-30.
    status :: Prelude.Maybe Prelude.Text,
    -- | The URL of the parent manifest for the repackaged Asset.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ID of the PackagingConfiguration being applied to the Asset.
    packagingConfigurationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EgressEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'egressEndpoint_status' - The current processing status of the asset used for the packaging
-- configuration. The status can be either QUEUED, PROCESSING, PLAYABLE, or
-- FAILED. Status information won\'t be available for most assets ingested
-- before 2021-09-30.
--
-- 'url', 'egressEndpoint_url' - The URL of the parent manifest for the repackaged Asset.
--
-- 'packagingConfigurationId', 'egressEndpoint_packagingConfigurationId' - The ID of the PackagingConfiguration being applied to the Asset.
newEgressEndpoint ::
  EgressEndpoint
newEgressEndpoint =
  EgressEndpoint'
    { status = Prelude.Nothing,
      url = Prelude.Nothing,
      packagingConfigurationId = Prelude.Nothing
    }

-- | The current processing status of the asset used for the packaging
-- configuration. The status can be either QUEUED, PROCESSING, PLAYABLE, or
-- FAILED. Status information won\'t be available for most assets ingested
-- before 2021-09-30.
egressEndpoint_status :: Lens.Lens' EgressEndpoint (Prelude.Maybe Prelude.Text)
egressEndpoint_status = Lens.lens (\EgressEndpoint' {status} -> status) (\s@EgressEndpoint' {} a -> s {status = a} :: EgressEndpoint)

-- | The URL of the parent manifest for the repackaged Asset.
egressEndpoint_url :: Lens.Lens' EgressEndpoint (Prelude.Maybe Prelude.Text)
egressEndpoint_url = Lens.lens (\EgressEndpoint' {url} -> url) (\s@EgressEndpoint' {} a -> s {url = a} :: EgressEndpoint)

-- | The ID of the PackagingConfiguration being applied to the Asset.
egressEndpoint_packagingConfigurationId :: Lens.Lens' EgressEndpoint (Prelude.Maybe Prelude.Text)
egressEndpoint_packagingConfigurationId = Lens.lens (\EgressEndpoint' {packagingConfigurationId} -> packagingConfigurationId) (\s@EgressEndpoint' {} a -> s {packagingConfigurationId = a} :: EgressEndpoint)

instance Core.FromJSON EgressEndpoint where
  parseJSON =
    Core.withObject
      "EgressEndpoint"
      ( \x ->
          EgressEndpoint'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "packagingConfigurationId")
      )

instance Prelude.Hashable EgressEndpoint where
  hashWithSalt salt' EgressEndpoint' {..} =
    salt'
      `Prelude.hashWithSalt` packagingConfigurationId
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` status

instance Prelude.NFData EgressEndpoint where
  rnf EgressEndpoint' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf packagingConfigurationId
      `Prelude.seq` Prelude.rnf url
