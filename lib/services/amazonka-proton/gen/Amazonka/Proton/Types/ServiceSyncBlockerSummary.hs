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
-- Module      : Amazonka.Proton.Types.ServiceSyncBlockerSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceSyncBlockerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.SyncBlocker

-- | If a service instance is manually updated, Proton wants to prevent
-- accidentally overriding a manual change.
--
-- A blocker is created because of the manual update or deletion of a
-- service instance. The summary describes the blocker as being active or
-- resolved.
--
-- /See:/ 'newServiceSyncBlockerSummary' smart constructor.
data ServiceSyncBlockerSummary = ServiceSyncBlockerSummary'
  { -- | The latest active blockers for the synced service.
    latestBlockers :: Prelude.Maybe [SyncBlocker],
    -- | The name of the service instance that you want sync your service
    -- configuration with.
    serviceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the service that you want to get the sync blocker summary
    -- for. If given a service instance name and a service name, it will return
    -- the blockers only applying to the instance that is blocked.
    --
    -- If given only a service name, it will return the blockers that apply to
    -- all of the instances. In order to get the blockers for a single
    -- instance, you will need to make two distinct calls, one to get the sync
    -- blocker summary for the service and the other to get the sync blocker
    -- for the service instance.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSyncBlockerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestBlockers', 'serviceSyncBlockerSummary_latestBlockers' - The latest active blockers for the synced service.
--
-- 'serviceInstanceName', 'serviceSyncBlockerSummary_serviceInstanceName' - The name of the service instance that you want sync your service
-- configuration with.
--
-- 'serviceName', 'serviceSyncBlockerSummary_serviceName' - The name of the service that you want to get the sync blocker summary
-- for. If given a service instance name and a service name, it will return
-- the blockers only applying to the instance that is blocked.
--
-- If given only a service name, it will return the blockers that apply to
-- all of the instances. In order to get the blockers for a single
-- instance, you will need to make two distinct calls, one to get the sync
-- blocker summary for the service and the other to get the sync blocker
-- for the service instance.
newServiceSyncBlockerSummary ::
  -- | 'serviceName'
  Prelude.Text ->
  ServiceSyncBlockerSummary
newServiceSyncBlockerSummary pServiceName_ =
  ServiceSyncBlockerSummary'
    { latestBlockers =
        Prelude.Nothing,
      serviceInstanceName = Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | The latest active blockers for the synced service.
serviceSyncBlockerSummary_latestBlockers :: Lens.Lens' ServiceSyncBlockerSummary (Prelude.Maybe [SyncBlocker])
serviceSyncBlockerSummary_latestBlockers = Lens.lens (\ServiceSyncBlockerSummary' {latestBlockers} -> latestBlockers) (\s@ServiceSyncBlockerSummary' {} a -> s {latestBlockers = a} :: ServiceSyncBlockerSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service instance that you want sync your service
-- configuration with.
serviceSyncBlockerSummary_serviceInstanceName :: Lens.Lens' ServiceSyncBlockerSummary (Prelude.Maybe Prelude.Text)
serviceSyncBlockerSummary_serviceInstanceName = Lens.lens (\ServiceSyncBlockerSummary' {serviceInstanceName} -> serviceInstanceName) (\s@ServiceSyncBlockerSummary' {} a -> s {serviceInstanceName = a} :: ServiceSyncBlockerSummary)

-- | The name of the service that you want to get the sync blocker summary
-- for. If given a service instance name and a service name, it will return
-- the blockers only applying to the instance that is blocked.
--
-- If given only a service name, it will return the blockers that apply to
-- all of the instances. In order to get the blockers for a single
-- instance, you will need to make two distinct calls, one to get the sync
-- blocker summary for the service and the other to get the sync blocker
-- for the service instance.
serviceSyncBlockerSummary_serviceName :: Lens.Lens' ServiceSyncBlockerSummary Prelude.Text
serviceSyncBlockerSummary_serviceName = Lens.lens (\ServiceSyncBlockerSummary' {serviceName} -> serviceName) (\s@ServiceSyncBlockerSummary' {} a -> s {serviceName = a} :: ServiceSyncBlockerSummary)

instance Data.FromJSON ServiceSyncBlockerSummary where
  parseJSON =
    Data.withObject
      "ServiceSyncBlockerSummary"
      ( \x ->
          ServiceSyncBlockerSummary'
            Prelude.<$> (x Data..:? "latestBlockers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "serviceInstanceName")
            Prelude.<*> (x Data..: "serviceName")
      )

instance Prelude.Hashable ServiceSyncBlockerSummary where
  hashWithSalt _salt ServiceSyncBlockerSummary' {..} =
    _salt
      `Prelude.hashWithSalt` latestBlockers
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ServiceSyncBlockerSummary where
  rnf ServiceSyncBlockerSummary' {..} =
    Prelude.rnf latestBlockers
      `Prelude.seq` Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf serviceName
