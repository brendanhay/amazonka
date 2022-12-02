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
-- Module      : Amazonka.DrS.Types.SourceServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.SourceServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.DataReplicationInfo
import Amazonka.DrS.Types.LastLaunchResult
import Amazonka.DrS.Types.LifeCycle
import Amazonka.DrS.Types.SourceProperties
import Amazonka.DrS.Types.StagingArea
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newSourceServer' smart constructor.
data SourceServer = SourceServer'
  { -- | The tags associated with the Source Server.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The lifecycle information of this Source Server.
    lifeCycle :: Prelude.Maybe LifeCycle,
    -- | The ID of the Recovery Instance associated with this Source Server.
    recoveryInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Source Server.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the last recovery launch of this Source Server.
    lastLaunchResult :: Prelude.Maybe LastLaunchResult,
    -- | The Data Replication Info of the Source Server.
    dataReplicationInfo :: Prelude.Maybe DataReplicationInfo,
    -- | The staging area of the source server.
    stagingArea :: Prelude.Maybe StagingArea,
    -- | The ID of the Source Server.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | The source properties of the Source Server.
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
-- 'tags', 'sourceServer_tags' - The tags associated with the Source Server.
--
-- 'lifeCycle', 'sourceServer_lifeCycle' - The lifecycle information of this Source Server.
--
-- 'recoveryInstanceId', 'sourceServer_recoveryInstanceId' - The ID of the Recovery Instance associated with this Source Server.
--
-- 'arn', 'sourceServer_arn' - The ARN of the Source Server.
--
-- 'lastLaunchResult', 'sourceServer_lastLaunchResult' - The status of the last recovery launch of this Source Server.
--
-- 'dataReplicationInfo', 'sourceServer_dataReplicationInfo' - The Data Replication Info of the Source Server.
--
-- 'stagingArea', 'sourceServer_stagingArea' - The staging area of the source server.
--
-- 'sourceServerID', 'sourceServer_sourceServerID' - The ID of the Source Server.
--
-- 'sourceProperties', 'sourceServer_sourceProperties' - The source properties of the Source Server.
newSourceServer ::
  SourceServer
newSourceServer =
  SourceServer'
    { tags = Prelude.Nothing,
      lifeCycle = Prelude.Nothing,
      recoveryInstanceId = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastLaunchResult = Prelude.Nothing,
      dataReplicationInfo = Prelude.Nothing,
      stagingArea = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      sourceProperties = Prelude.Nothing
    }

-- | The tags associated with the Source Server.
sourceServer_tags :: Lens.Lens' SourceServer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sourceServer_tags = Lens.lens (\SourceServer' {tags} -> tags) (\s@SourceServer' {} a -> s {tags = a} :: SourceServer) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The lifecycle information of this Source Server.
sourceServer_lifeCycle :: Lens.Lens' SourceServer (Prelude.Maybe LifeCycle)
sourceServer_lifeCycle = Lens.lens (\SourceServer' {lifeCycle} -> lifeCycle) (\s@SourceServer' {} a -> s {lifeCycle = a} :: SourceServer)

-- | The ID of the Recovery Instance associated with this Source Server.
sourceServer_recoveryInstanceId :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Text)
sourceServer_recoveryInstanceId = Lens.lens (\SourceServer' {recoveryInstanceId} -> recoveryInstanceId) (\s@SourceServer' {} a -> s {recoveryInstanceId = a} :: SourceServer)

-- | The ARN of the Source Server.
sourceServer_arn :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Text)
sourceServer_arn = Lens.lens (\SourceServer' {arn} -> arn) (\s@SourceServer' {} a -> s {arn = a} :: SourceServer)

-- | The status of the last recovery launch of this Source Server.
sourceServer_lastLaunchResult :: Lens.Lens' SourceServer (Prelude.Maybe LastLaunchResult)
sourceServer_lastLaunchResult = Lens.lens (\SourceServer' {lastLaunchResult} -> lastLaunchResult) (\s@SourceServer' {} a -> s {lastLaunchResult = a} :: SourceServer)

-- | The Data Replication Info of the Source Server.
sourceServer_dataReplicationInfo :: Lens.Lens' SourceServer (Prelude.Maybe DataReplicationInfo)
sourceServer_dataReplicationInfo = Lens.lens (\SourceServer' {dataReplicationInfo} -> dataReplicationInfo) (\s@SourceServer' {} a -> s {dataReplicationInfo = a} :: SourceServer)

-- | The staging area of the source server.
sourceServer_stagingArea :: Lens.Lens' SourceServer (Prelude.Maybe StagingArea)
sourceServer_stagingArea = Lens.lens (\SourceServer' {stagingArea} -> stagingArea) (\s@SourceServer' {} a -> s {stagingArea = a} :: SourceServer)

-- | The ID of the Source Server.
sourceServer_sourceServerID :: Lens.Lens' SourceServer (Prelude.Maybe Prelude.Text)
sourceServer_sourceServerID = Lens.lens (\SourceServer' {sourceServerID} -> sourceServerID) (\s@SourceServer' {} a -> s {sourceServerID = a} :: SourceServer)

-- | The source properties of the Source Server.
sourceServer_sourceProperties :: Lens.Lens' SourceServer (Prelude.Maybe SourceProperties)
sourceServer_sourceProperties = Lens.lens (\SourceServer' {sourceProperties} -> sourceProperties) (\s@SourceServer' {} a -> s {sourceProperties = a} :: SourceServer)

instance Data.FromJSON SourceServer where
  parseJSON =
    Data.withObject
      "SourceServer"
      ( \x ->
          SourceServer'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "lifeCycle")
            Prelude.<*> (x Data..:? "recoveryInstanceId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "lastLaunchResult")
            Prelude.<*> (x Data..:? "dataReplicationInfo")
            Prelude.<*> (x Data..:? "stagingArea")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "sourceProperties")
      )

instance Prelude.Hashable SourceServer where
  hashWithSalt _salt SourceServer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lifeCycle
      `Prelude.hashWithSalt` recoveryInstanceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastLaunchResult
      `Prelude.hashWithSalt` dataReplicationInfo
      `Prelude.hashWithSalt` stagingArea
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` sourceProperties

instance Prelude.NFData SourceServer where
  rnf SourceServer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lifeCycle
      `Prelude.seq` Prelude.rnf recoveryInstanceId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastLaunchResult
      `Prelude.seq` Prelude.rnf dataReplicationInfo
      `Prelude.seq` Prelude.rnf stagingArea
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf sourceProperties
