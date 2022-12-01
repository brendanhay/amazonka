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
-- Module      : Amazonka.MGN.Types.SourceServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SourceServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.DataReplicationInfo
import Amazonka.MGN.Types.LaunchedInstance
import Amazonka.MGN.Types.LifeCycle
import Amazonka.MGN.Types.ReplicationType
import Amazonka.MGN.Types.SourceProperties
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newSourceServer' smart constructor.
data SourceServer = SourceServer'
  { -- | Source server Tags.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Source server vCenter client id.
    vcenterClientID :: Prelude.Maybe Prelude.Text,
    -- | Source server lifecycle state.
    lifeCycle :: Prelude.Maybe LifeCycle,
    -- | Source server replication type.
    replicationType :: Prelude.Maybe ReplicationType,
    -- | Source server launched instance.
    launchedInstance :: Prelude.Maybe LaunchedInstance,
    -- | Source server ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Source server data replication info.
    dataReplicationInfo :: Prelude.Maybe DataReplicationInfo,
    -- | Source server archived status.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | Source server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Source server properties.
    sourceProperties :: Prelude.Maybe SourceProperties
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'sourceServer_tags' - Source server Tags.
--
-- 'vcenterClientID', 'sourceServer_vcenterClientID' - Source server vCenter client id.
--
-- 'lifeCycle', 'sourceServer_lifeCycle' - Source server lifecycle state.
--
-- 'replicationType', 'sourceServer_replicationType' - Source server replication type.
--
-- 'launchedInstance', 'sourceServer_launchedInstance' - Source server launched instance.
--
-- 'arn', 'sourceServer_arn' - Source server ARN.
--
-- 'dataReplicationInfo', 'sourceServer_dataReplicationInfo' - Source server data replication info.
--
-- 'isArchived', 'sourceServer_isArchived' - Source server archived status.
--
-- 'sourceServerID', 'sourceServer_sourceServerID' - Source server ID.
--
-- 'sourceProperties', 'sourceServer_sourceProperties' - Source server properties.
newSourceServer ::
  SourceServer
newSourceServer =
  SourceServer'
    { tags = Prelude.Nothing,
      vcenterClientID = Prelude.Nothing,
      lifeCycle = Prelude.Nothing,
      replicationType = Prelude.Nothing,
      launchedInstance = Prelude.Nothing,
      arn = Prelude.Nothing,
      dataReplicationInfo = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      sourceProperties = Prelude.Nothing
    }

-- | Source server Tags.
sourceServer_tags :: Lens.Lens' SourceServer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sourceServer_tags = Lens.lens (\SourceServer' {tags} -> tags) (\s@SourceServer' {} a -> s {tags = a} :: SourceServer) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Source server vCenter client id.
sourceServer_vcenterClientID :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Text)
sourceServer_vcenterClientID = Lens.lens (\SourceServer' {vcenterClientID} -> vcenterClientID) (\s@SourceServer' {} a -> s {vcenterClientID = a} :: SourceServer)

-- | Source server lifecycle state.
sourceServer_lifeCycle :: Lens.Lens' SourceServer (Prelude.Maybe LifeCycle)
sourceServer_lifeCycle = Lens.lens (\SourceServer' {lifeCycle} -> lifeCycle) (\s@SourceServer' {} a -> s {lifeCycle = a} :: SourceServer)

-- | Source server replication type.
sourceServer_replicationType :: Lens.Lens' SourceServer (Prelude.Maybe ReplicationType)
sourceServer_replicationType = Lens.lens (\SourceServer' {replicationType} -> replicationType) (\s@SourceServer' {} a -> s {replicationType = a} :: SourceServer)

-- | Source server launched instance.
sourceServer_launchedInstance :: Lens.Lens' SourceServer (Prelude.Maybe LaunchedInstance)
sourceServer_launchedInstance = Lens.lens (\SourceServer' {launchedInstance} -> launchedInstance) (\s@SourceServer' {} a -> s {launchedInstance = a} :: SourceServer)

-- | Source server ARN.
sourceServer_arn :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Text)
sourceServer_arn = Lens.lens (\SourceServer' {arn} -> arn) (\s@SourceServer' {} a -> s {arn = a} :: SourceServer)

-- | Source server data replication info.
sourceServer_dataReplicationInfo :: Lens.Lens' SourceServer (Prelude.Maybe DataReplicationInfo)
sourceServer_dataReplicationInfo = Lens.lens (\SourceServer' {dataReplicationInfo} -> dataReplicationInfo) (\s@SourceServer' {} a -> s {dataReplicationInfo = a} :: SourceServer)

-- | Source server archived status.
sourceServer_isArchived :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Bool)
sourceServer_isArchived = Lens.lens (\SourceServer' {isArchived} -> isArchived) (\s@SourceServer' {} a -> s {isArchived = a} :: SourceServer)

-- | Source server ID.
sourceServer_sourceServerID :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Text)
sourceServer_sourceServerID = Lens.lens (\SourceServer' {sourceServerID} -> sourceServerID) (\s@SourceServer' {} a -> s {sourceServerID = a} :: SourceServer)

-- | Source server properties.
sourceServer_sourceProperties :: Lens.Lens' SourceServer (Prelude.Maybe SourceProperties)
sourceServer_sourceProperties = Lens.lens (\SourceServer' {sourceProperties} -> sourceProperties) (\s@SourceServer' {} a -> s {sourceProperties = a} :: SourceServer)

instance Core.FromJSON SourceServer where
  parseJSON =
    Core.withObject
      "SourceServer"
      ( \x ->
          SourceServer'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "vcenterClientID")
            Prelude.<*> (x Core..:? "lifeCycle")
            Prelude.<*> (x Core..:? "replicationType")
            Prelude.<*> (x Core..:? "launchedInstance")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "dataReplicationInfo")
            Prelude.<*> (x Core..:? "isArchived")
            Prelude.<*> (x Core..:? "sourceServerID")
            Prelude.<*> (x Core..:? "sourceProperties")
      )

instance Prelude.Hashable SourceServer where
  hashWithSalt _salt SourceServer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vcenterClientID
      `Prelude.hashWithSalt` lifeCycle
      `Prelude.hashWithSalt` replicationType
      `Prelude.hashWithSalt` launchedInstance
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` dataReplicationInfo
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` sourceProperties

instance Prelude.NFData SourceServer where
  rnf SourceServer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vcenterClientID
      `Prelude.seq` Prelude.rnf lifeCycle
      `Prelude.seq` Prelude.rnf replicationType
      `Prelude.seq` Prelude.rnf launchedInstance
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dataReplicationInfo
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf sourceProperties
