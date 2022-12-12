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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.EgressEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The endpoint URL used to access an Asset using one
-- PackagingConfiguration.
--
-- /See:/ 'newEgressEndpoint' smart constructor.
data EgressEndpoint = EgressEndpoint'
  { -- | The ID of the PackagingConfiguration being applied to the Asset.
    packagingConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | The current processing status of the asset used for the packaging
    -- configuration. The status can be either QUEUED, PROCESSING, PLAYABLE, or
    -- FAILED. Status information won\'t be available for most assets ingested
    -- before 2021-09-30.
    status :: Prelude.Maybe Prelude.Text,
    -- | The URL of the parent manifest for the repackaged Asset.
    url :: Prelude.Maybe Prelude.Text
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
-- 'packagingConfigurationId', 'egressEndpoint_packagingConfigurationId' - The ID of the PackagingConfiguration being applied to the Asset.
--
-- 'status', 'egressEndpoint_status' - The current processing status of the asset used for the packaging
-- configuration. The status can be either QUEUED, PROCESSING, PLAYABLE, or
-- FAILED. Status information won\'t be available for most assets ingested
-- before 2021-09-30.
--
-- 'url', 'egressEndpoint_url' - The URL of the parent manifest for the repackaged Asset.
newEgressEndpoint ::
  EgressEndpoint
newEgressEndpoint =
  EgressEndpoint'
    { packagingConfigurationId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The ID of the PackagingConfiguration being applied to the Asset.
egressEndpoint_packagingConfigurationId :: Lens.Lens' EgressEndpoint (Prelude.Maybe Prelude.Text)
egressEndpoint_packagingConfigurationId = Lens.lens (\EgressEndpoint' {packagingConfigurationId} -> packagingConfigurationId) (\s@EgressEndpoint' {} a -> s {packagingConfigurationId = a} :: EgressEndpoint)

-- | The current processing status of the asset used for the packaging
-- configuration. The status can be either QUEUED, PROCESSING, PLAYABLE, or
-- FAILED. Status information won\'t be available for most assets ingested
-- before 2021-09-30.
egressEndpoint_status :: Lens.Lens' EgressEndpoint (Prelude.Maybe Prelude.Text)
egressEndpoint_status = Lens.lens (\EgressEndpoint' {status} -> status) (\s@EgressEndpoint' {} a -> s {status = a} :: EgressEndpoint)

-- | The URL of the parent manifest for the repackaged Asset.
egressEndpoint_url :: Lens.Lens' EgressEndpoint (Prelude.Maybe Prelude.Text)
egressEndpoint_url = Lens.lens (\EgressEndpoint' {url} -> url) (\s@EgressEndpoint' {} a -> s {url = a} :: EgressEndpoint)

instance Data.FromJSON EgressEndpoint where
  parseJSON =
    Data.withObject
      "EgressEndpoint"
      ( \x ->
          EgressEndpoint'
            Prelude.<$> (x Data..:? "packagingConfigurationId")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable EgressEndpoint where
  hashWithSalt _salt EgressEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` packagingConfigurationId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` url

instance Prelude.NFData EgressEndpoint where
  rnf EgressEndpoint' {..} =
    Prelude.rnf packagingConfigurationId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf url
