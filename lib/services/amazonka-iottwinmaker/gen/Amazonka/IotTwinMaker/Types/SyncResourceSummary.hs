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
-- Module      : Amazonka.IotTwinMaker.Types.SyncResourceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncResourceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.SyncResourceStatus
import Amazonka.IotTwinMaker.Types.SyncResourceType
import qualified Amazonka.Prelude as Prelude

-- | The sync resource summary.
--
-- /See:/ 'newSyncResourceSummary' smart constructor.
data SyncResourceSummary = SyncResourceSummary'
  { -- | The external Id.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The resource Id.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe SyncResourceType,
    -- | The sync resource summary status.
    status :: Prelude.Maybe SyncResourceStatus,
    -- | The update date and time.
    updateDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'syncResourceSummary_externalId' - The external Id.
--
-- 'resourceId', 'syncResourceSummary_resourceId' - The resource Id.
--
-- 'resourceType', 'syncResourceSummary_resourceType' - The resource type.
--
-- 'status', 'syncResourceSummary_status' - The sync resource summary status.
--
-- 'updateDateTime', 'syncResourceSummary_updateDateTime' - The update date and time.
newSyncResourceSummary ::
  SyncResourceSummary
newSyncResourceSummary =
  SyncResourceSummary'
    { externalId = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      updateDateTime = Prelude.Nothing
    }

-- | The external Id.
syncResourceSummary_externalId :: Lens.Lens' SyncResourceSummary (Prelude.Maybe Prelude.Text)
syncResourceSummary_externalId = Lens.lens (\SyncResourceSummary' {externalId} -> externalId) (\s@SyncResourceSummary' {} a -> s {externalId = a} :: SyncResourceSummary)

-- | The resource Id.
syncResourceSummary_resourceId :: Lens.Lens' SyncResourceSummary (Prelude.Maybe Prelude.Text)
syncResourceSummary_resourceId = Lens.lens (\SyncResourceSummary' {resourceId} -> resourceId) (\s@SyncResourceSummary' {} a -> s {resourceId = a} :: SyncResourceSummary)

-- | The resource type.
syncResourceSummary_resourceType :: Lens.Lens' SyncResourceSummary (Prelude.Maybe SyncResourceType)
syncResourceSummary_resourceType = Lens.lens (\SyncResourceSummary' {resourceType} -> resourceType) (\s@SyncResourceSummary' {} a -> s {resourceType = a} :: SyncResourceSummary)

-- | The sync resource summary status.
syncResourceSummary_status :: Lens.Lens' SyncResourceSummary (Prelude.Maybe SyncResourceStatus)
syncResourceSummary_status = Lens.lens (\SyncResourceSummary' {status} -> status) (\s@SyncResourceSummary' {} a -> s {status = a} :: SyncResourceSummary)

-- | The update date and time.
syncResourceSummary_updateDateTime :: Lens.Lens' SyncResourceSummary (Prelude.Maybe Prelude.UTCTime)
syncResourceSummary_updateDateTime = Lens.lens (\SyncResourceSummary' {updateDateTime} -> updateDateTime) (\s@SyncResourceSummary' {} a -> s {updateDateTime = a} :: SyncResourceSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SyncResourceSummary where
  parseJSON =
    Data.withObject
      "SyncResourceSummary"
      ( \x ->
          SyncResourceSummary'
            Prelude.<$> (x Data..:? "externalId")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updateDateTime")
      )

instance Prelude.Hashable SyncResourceSummary where
  hashWithSalt _salt SyncResourceSummary' {..} =
    _salt `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateDateTime

instance Prelude.NFData SyncResourceSummary where
  rnf SyncResourceSummary' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateDateTime
