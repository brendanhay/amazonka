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
-- Module      : Amazonka.DrS.Types.SourceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.SourceNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.RecoveryLifeCycle
import Amazonka.DrS.Types.ReplicationStatus
import qualified Amazonka.Prelude as Prelude

-- | The ARN of the Source Network.
--
-- /See:/ 'newSourceNetwork' smart constructor.
data SourceNetwork = SourceNetwork'
  { -- | The ARN of the Source Network.
    arn :: Prelude.Maybe Prelude.Text,
    -- | CloudFormation stack name that was deployed for recovering the Source
    -- Network.
    cfnStackName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An object containing information regarding the last recovery of the
    -- Source Network.
    lastRecovery :: Prelude.Maybe RecoveryLifeCycle,
    -- | ID of the recovered VPC following Source Network recovery.
    launchedVpcID :: Prelude.Maybe Prelude.Text,
    -- | Status of Source Network Replication. Possible values: (a) STOPPED -
    -- Source Network is not replicating. (b) IN_PROGRESS - Source Network is
    -- being replicated. (c) PROTECTED - Source Network was replicated
    -- successfully and is being synchronized for changes. (d) ERROR - Source
    -- Network replication has failed
    replicationStatus :: Prelude.Maybe ReplicationStatus,
    -- | Error details in case Source Network replication status is ERROR.
    replicationStatusDetails :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Account ID containing the VPC protected by the Source Network.
    sourceAccountID :: Prelude.Maybe Prelude.Text,
    -- | Source Network ID.
    sourceNetworkID :: Prelude.Maybe Prelude.Text,
    -- | Region containing the VPC protected by the Source Network.
    sourceRegion :: Prelude.Maybe Prelude.Text,
    -- | VPC ID protected by the Source Network.
    sourceVpcID :: Prelude.Maybe Prelude.Text,
    -- | A list of tags associated with the Source Network.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'sourceNetwork_arn' - The ARN of the Source Network.
--
-- 'cfnStackName', 'sourceNetwork_cfnStackName' - CloudFormation stack name that was deployed for recovering the Source
-- Network.
--
-- 'lastRecovery', 'sourceNetwork_lastRecovery' - An object containing information regarding the last recovery of the
-- Source Network.
--
-- 'launchedVpcID', 'sourceNetwork_launchedVpcID' - ID of the recovered VPC following Source Network recovery.
--
-- 'replicationStatus', 'sourceNetwork_replicationStatus' - Status of Source Network Replication. Possible values: (a) STOPPED -
-- Source Network is not replicating. (b) IN_PROGRESS - Source Network is
-- being replicated. (c) PROTECTED - Source Network was replicated
-- successfully and is being synchronized for changes. (d) ERROR - Source
-- Network replication has failed
--
-- 'replicationStatusDetails', 'sourceNetwork_replicationStatusDetails' - Error details in case Source Network replication status is ERROR.
--
-- 'sourceAccountID', 'sourceNetwork_sourceAccountID' - Account ID containing the VPC protected by the Source Network.
--
-- 'sourceNetworkID', 'sourceNetwork_sourceNetworkID' - Source Network ID.
--
-- 'sourceRegion', 'sourceNetwork_sourceRegion' - Region containing the VPC protected by the Source Network.
--
-- 'sourceVpcID', 'sourceNetwork_sourceVpcID' - VPC ID protected by the Source Network.
--
-- 'tags', 'sourceNetwork_tags' - A list of tags associated with the Source Network.
newSourceNetwork ::
  SourceNetwork
newSourceNetwork =
  SourceNetwork'
    { arn = Prelude.Nothing,
      cfnStackName = Prelude.Nothing,
      lastRecovery = Prelude.Nothing,
      launchedVpcID = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      replicationStatusDetails = Prelude.Nothing,
      sourceAccountID = Prelude.Nothing,
      sourceNetworkID = Prelude.Nothing,
      sourceRegion = Prelude.Nothing,
      sourceVpcID = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the Source Network.
sourceNetwork_arn :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_arn = Lens.lens (\SourceNetwork' {arn} -> arn) (\s@SourceNetwork' {} a -> s {arn = a} :: SourceNetwork)

-- | CloudFormation stack name that was deployed for recovering the Source
-- Network.
sourceNetwork_cfnStackName :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_cfnStackName = Lens.lens (\SourceNetwork' {cfnStackName} -> cfnStackName) (\s@SourceNetwork' {} a -> s {cfnStackName = a} :: SourceNetwork) Prelude.. Lens.mapping Data._Sensitive

-- | An object containing information regarding the last recovery of the
-- Source Network.
sourceNetwork_lastRecovery :: Lens.Lens' SourceNetwork (Prelude.Maybe RecoveryLifeCycle)
sourceNetwork_lastRecovery = Lens.lens (\SourceNetwork' {lastRecovery} -> lastRecovery) (\s@SourceNetwork' {} a -> s {lastRecovery = a} :: SourceNetwork)

-- | ID of the recovered VPC following Source Network recovery.
sourceNetwork_launchedVpcID :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_launchedVpcID = Lens.lens (\SourceNetwork' {launchedVpcID} -> launchedVpcID) (\s@SourceNetwork' {} a -> s {launchedVpcID = a} :: SourceNetwork)

-- | Status of Source Network Replication. Possible values: (a) STOPPED -
-- Source Network is not replicating. (b) IN_PROGRESS - Source Network is
-- being replicated. (c) PROTECTED - Source Network was replicated
-- successfully and is being synchronized for changes. (d) ERROR - Source
-- Network replication has failed
sourceNetwork_replicationStatus :: Lens.Lens' SourceNetwork (Prelude.Maybe ReplicationStatus)
sourceNetwork_replicationStatus = Lens.lens (\SourceNetwork' {replicationStatus} -> replicationStatus) (\s@SourceNetwork' {} a -> s {replicationStatus = a} :: SourceNetwork)

-- | Error details in case Source Network replication status is ERROR.
sourceNetwork_replicationStatusDetails :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_replicationStatusDetails = Lens.lens (\SourceNetwork' {replicationStatusDetails} -> replicationStatusDetails) (\s@SourceNetwork' {} a -> s {replicationStatusDetails = a} :: SourceNetwork) Prelude.. Lens.mapping Data._Sensitive

-- | Account ID containing the VPC protected by the Source Network.
sourceNetwork_sourceAccountID :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_sourceAccountID = Lens.lens (\SourceNetwork' {sourceAccountID} -> sourceAccountID) (\s@SourceNetwork' {} a -> s {sourceAccountID = a} :: SourceNetwork)

-- | Source Network ID.
sourceNetwork_sourceNetworkID :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_sourceNetworkID = Lens.lens (\SourceNetwork' {sourceNetworkID} -> sourceNetworkID) (\s@SourceNetwork' {} a -> s {sourceNetworkID = a} :: SourceNetwork)

-- | Region containing the VPC protected by the Source Network.
sourceNetwork_sourceRegion :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_sourceRegion = Lens.lens (\SourceNetwork' {sourceRegion} -> sourceRegion) (\s@SourceNetwork' {} a -> s {sourceRegion = a} :: SourceNetwork)

-- | VPC ID protected by the Source Network.
sourceNetwork_sourceVpcID :: Lens.Lens' SourceNetwork (Prelude.Maybe Prelude.Text)
sourceNetwork_sourceVpcID = Lens.lens (\SourceNetwork' {sourceVpcID} -> sourceVpcID) (\s@SourceNetwork' {} a -> s {sourceVpcID = a} :: SourceNetwork)

-- | A list of tags associated with the Source Network.
sourceNetwork_tags :: Lens.Lens' SourceNetwork (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sourceNetwork_tags = Lens.lens (\SourceNetwork' {tags} -> tags) (\s@SourceNetwork' {} a -> s {tags = a} :: SourceNetwork) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON SourceNetwork where
  parseJSON =
    Data.withObject
      "SourceNetwork"
      ( \x ->
          SourceNetwork'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "cfnStackName")
            Prelude.<*> (x Data..:? "lastRecovery")
            Prelude.<*> (x Data..:? "launchedVpcID")
            Prelude.<*> (x Data..:? "replicationStatus")
            Prelude.<*> (x Data..:? "replicationStatusDetails")
            Prelude.<*> (x Data..:? "sourceAccountID")
            Prelude.<*> (x Data..:? "sourceNetworkID")
            Prelude.<*> (x Data..:? "sourceRegion")
            Prelude.<*> (x Data..:? "sourceVpcID")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SourceNetwork where
  hashWithSalt _salt SourceNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` cfnStackName
      `Prelude.hashWithSalt` lastRecovery
      `Prelude.hashWithSalt` launchedVpcID
      `Prelude.hashWithSalt` replicationStatus
      `Prelude.hashWithSalt` replicationStatusDetails
      `Prelude.hashWithSalt` sourceAccountID
      `Prelude.hashWithSalt` sourceNetworkID
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` sourceVpcID
      `Prelude.hashWithSalt` tags

instance Prelude.NFData SourceNetwork where
  rnf SourceNetwork' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf cfnStackName
      `Prelude.seq` Prelude.rnf lastRecovery
      `Prelude.seq` Prelude.rnf launchedVpcID
      `Prelude.seq` Prelude.rnf replicationStatus
      `Prelude.seq` Prelude.rnf replicationStatusDetails
      `Prelude.seq` Prelude.rnf sourceAccountID
      `Prelude.seq` Prelude.rnf sourceNetworkID
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf sourceVpcID
      `Prelude.seq` Prelude.rnf tags
