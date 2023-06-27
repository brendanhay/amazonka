{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.DescribeDomainHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about domain and node health, the standby
-- Availability Zone, number of nodes per Availability Zone, and shard
-- count per node.
module Amazonka.OpenSearch.DescribeDomainHealth
  ( -- * Creating a Request
    DescribeDomainHealth (..),
    newDescribeDomainHealth,

    -- * Request Lenses
    describeDomainHealth_domainName,

    -- * Destructuring the Response
    DescribeDomainHealthResponse (..),
    newDescribeDomainHealthResponse,

    -- * Response Lenses
    describeDomainHealthResponse_activeAvailabilityZoneCount,
    describeDomainHealthResponse_availabilityZoneCount,
    describeDomainHealthResponse_clusterHealth,
    describeDomainHealthResponse_dataNodeCount,
    describeDomainHealthResponse_dedicatedMaster,
    describeDomainHealthResponse_domainState,
    describeDomainHealthResponse_environmentInformation,
    describeDomainHealthResponse_masterEligibleNodeCount,
    describeDomainHealthResponse_masterNode,
    describeDomainHealthResponse_standByAvailabilityZoneCount,
    describeDomainHealthResponse_totalShards,
    describeDomainHealthResponse_totalUnAssignedShards,
    describeDomainHealthResponse_warmNodeCount,
    describeDomainHealthResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeDomainHealth@ operation.
--
-- /See:/ 'newDescribeDomainHealth' smart constructor.
data DescribeDomainHealth = DescribeDomainHealth'
  { -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeDomainHealth_domainName' - The name of the domain.
newDescribeDomainHealth ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeDomainHealth
newDescribeDomainHealth pDomainName_ =
  DescribeDomainHealth' {domainName = pDomainName_}

-- | The name of the domain.
describeDomainHealth_domainName :: Lens.Lens' DescribeDomainHealth Prelude.Text
describeDomainHealth_domainName = Lens.lens (\DescribeDomainHealth' {domainName} -> domainName) (\s@DescribeDomainHealth' {} a -> s {domainName = a} :: DescribeDomainHealth)

instance Core.AWSRequest DescribeDomainHealth where
  type
    AWSResponse DescribeDomainHealth =
      DescribeDomainHealthResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainHealthResponse'
            Prelude.<$> (x Data..?> "ActiveAvailabilityZoneCount")
            Prelude.<*> (x Data..?> "AvailabilityZoneCount")
            Prelude.<*> (x Data..?> "ClusterHealth")
            Prelude.<*> (x Data..?> "DataNodeCount")
            Prelude.<*> (x Data..?> "DedicatedMaster")
            Prelude.<*> (x Data..?> "DomainState")
            Prelude.<*> ( x
                            Data..?> "EnvironmentInformation"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "MasterEligibleNodeCount")
            Prelude.<*> (x Data..?> "MasterNode")
            Prelude.<*> (x Data..?> "StandByAvailabilityZoneCount")
            Prelude.<*> (x Data..?> "TotalShards")
            Prelude.<*> (x Data..?> "TotalUnAssignedShards")
            Prelude.<*> (x Data..?> "WarmNodeCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDomainHealth where
  hashWithSalt _salt DescribeDomainHealth' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeDomainHealth where
  rnf DescribeDomainHealth' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders DescribeDomainHealth where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDomainHealth where
  toPath DescribeDomainHealth' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/health"
      ]

instance Data.ToQuery DescribeDomainHealth where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeDomainHealth@ request. Contains health
-- information for the requested domain.
--
-- /See:/ 'newDescribeDomainHealthResponse' smart constructor.
data DescribeDomainHealthResponse = DescribeDomainHealthResponse'
  { -- | The number of active Availability Zones configured for the domain. If
    -- the service is unable to fetch this information, it will return
    -- @NotAvailable@.
    activeAvailabilityZoneCount :: Prelude.Maybe Prelude.Text,
    -- | The number of Availability Zones configured for the domain. If the
    -- service is unable to fetch this information, it will return
    -- @NotAvailable@.
    availabilityZoneCount :: Prelude.Maybe Prelude.Text,
    -- | The current health status of your cluster.
    --
    -- -   @Red@ - At least one primary shard is not allocated to any node.
    --
    -- -   @Yellow@ - All primary shards are allocated to nodes, but some
    --     replicas aren’t.
    --
    -- -   @Green@ - All primary shards and their replicas are allocated to
    --     nodes.
    --
    -- -   @NotAvailable@ - Unable to retrieve cluster health.
    clusterHealth :: Prelude.Maybe DomainHealth,
    -- | The number of data nodes configured for the domain. If the service is
    -- unable to fetch this information, it will return @NotAvailable@.
    dataNodeCount :: Prelude.Maybe Prelude.Text,
    -- | A boolean that indicates if dedicated master nodes are activated for the
    -- domain.
    dedicatedMaster :: Prelude.Maybe Prelude.Bool,
    -- | The current state of the domain.
    --
    -- -   @Processing@ - The domain has updates in progress.
    --
    -- -   @Active@ - Requested changes have been processed and deployed to the
    --     domain.
    domainState :: Prelude.Maybe DomainState,
    -- | A list of @EnvironmentInfo@ for the domain.
    environmentInformation :: Prelude.Maybe [EnvironmentInfo],
    -- | The number of nodes that can be elected as a master node. If dedicated
    -- master nodes is turned on, this value is the number of dedicated master
    -- nodes configured for the domain. If the service is unable to fetch this
    -- information, it will return @NotAvailable@.
    masterEligibleNodeCount :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the domain has an elected master node.
    --
    -- -   __Available__ - The domain has an elected master node.
    --
    -- -   __UnAvailable__ - The master node hasn\'t yet been elected, and a
    --     quorum to elect a new master node hasn\'t been reached.
    masterNode :: Prelude.Maybe MasterNodeStatus,
    -- | The number of standby Availability Zones configured for the domain. If
    -- the service is unable to fetch this information, it will return
    -- @NotAvailable@.
    standByAvailabilityZoneCount :: Prelude.Maybe Prelude.Text,
    -- | The total number of primary and replica shards for the domain.
    totalShards :: Prelude.Maybe Prelude.Text,
    -- | The total number of primary and replica shards not allocated to any of
    -- the nodes for the cluster.
    totalUnAssignedShards :: Prelude.Maybe Prelude.Text,
    -- | The number of warm nodes configured for the domain.
    warmNodeCount :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeAvailabilityZoneCount', 'describeDomainHealthResponse_activeAvailabilityZoneCount' - The number of active Availability Zones configured for the domain. If
-- the service is unable to fetch this information, it will return
-- @NotAvailable@.
--
-- 'availabilityZoneCount', 'describeDomainHealthResponse_availabilityZoneCount' - The number of Availability Zones configured for the domain. If the
-- service is unable to fetch this information, it will return
-- @NotAvailable@.
--
-- 'clusterHealth', 'describeDomainHealthResponse_clusterHealth' - The current health status of your cluster.
--
-- -   @Red@ - At least one primary shard is not allocated to any node.
--
-- -   @Yellow@ - All primary shards are allocated to nodes, but some
--     replicas aren’t.
--
-- -   @Green@ - All primary shards and their replicas are allocated to
--     nodes.
--
-- -   @NotAvailable@ - Unable to retrieve cluster health.
--
-- 'dataNodeCount', 'describeDomainHealthResponse_dataNodeCount' - The number of data nodes configured for the domain. If the service is
-- unable to fetch this information, it will return @NotAvailable@.
--
-- 'dedicatedMaster', 'describeDomainHealthResponse_dedicatedMaster' - A boolean that indicates if dedicated master nodes are activated for the
-- domain.
--
-- 'domainState', 'describeDomainHealthResponse_domainState' - The current state of the domain.
--
-- -   @Processing@ - The domain has updates in progress.
--
-- -   @Active@ - Requested changes have been processed and deployed to the
--     domain.
--
-- 'environmentInformation', 'describeDomainHealthResponse_environmentInformation' - A list of @EnvironmentInfo@ for the domain.
--
-- 'masterEligibleNodeCount', 'describeDomainHealthResponse_masterEligibleNodeCount' - The number of nodes that can be elected as a master node. If dedicated
-- master nodes is turned on, this value is the number of dedicated master
-- nodes configured for the domain. If the service is unable to fetch this
-- information, it will return @NotAvailable@.
--
-- 'masterNode', 'describeDomainHealthResponse_masterNode' - Indicates whether the domain has an elected master node.
--
-- -   __Available__ - The domain has an elected master node.
--
-- -   __UnAvailable__ - The master node hasn\'t yet been elected, and a
--     quorum to elect a new master node hasn\'t been reached.
--
-- 'standByAvailabilityZoneCount', 'describeDomainHealthResponse_standByAvailabilityZoneCount' - The number of standby Availability Zones configured for the domain. If
-- the service is unable to fetch this information, it will return
-- @NotAvailable@.
--
-- 'totalShards', 'describeDomainHealthResponse_totalShards' - The total number of primary and replica shards for the domain.
--
-- 'totalUnAssignedShards', 'describeDomainHealthResponse_totalUnAssignedShards' - The total number of primary and replica shards not allocated to any of
-- the nodes for the cluster.
--
-- 'warmNodeCount', 'describeDomainHealthResponse_warmNodeCount' - The number of warm nodes configured for the domain.
--
-- 'httpStatus', 'describeDomainHealthResponse_httpStatus' - The response's http status code.
newDescribeDomainHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainHealthResponse
newDescribeDomainHealthResponse pHttpStatus_ =
  DescribeDomainHealthResponse'
    { activeAvailabilityZoneCount =
        Prelude.Nothing,
      availabilityZoneCount = Prelude.Nothing,
      clusterHealth = Prelude.Nothing,
      dataNodeCount = Prelude.Nothing,
      dedicatedMaster = Prelude.Nothing,
      domainState = Prelude.Nothing,
      environmentInformation = Prelude.Nothing,
      masterEligibleNodeCount = Prelude.Nothing,
      masterNode = Prelude.Nothing,
      standByAvailabilityZoneCount =
        Prelude.Nothing,
      totalShards = Prelude.Nothing,
      totalUnAssignedShards = Prelude.Nothing,
      warmNodeCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of active Availability Zones configured for the domain. If
-- the service is unable to fetch this information, it will return
-- @NotAvailable@.
describeDomainHealthResponse_activeAvailabilityZoneCount :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_activeAvailabilityZoneCount = Lens.lens (\DescribeDomainHealthResponse' {activeAvailabilityZoneCount} -> activeAvailabilityZoneCount) (\s@DescribeDomainHealthResponse' {} a -> s {activeAvailabilityZoneCount = a} :: DescribeDomainHealthResponse)

-- | The number of Availability Zones configured for the domain. If the
-- service is unable to fetch this information, it will return
-- @NotAvailable@.
describeDomainHealthResponse_availabilityZoneCount :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_availabilityZoneCount = Lens.lens (\DescribeDomainHealthResponse' {availabilityZoneCount} -> availabilityZoneCount) (\s@DescribeDomainHealthResponse' {} a -> s {availabilityZoneCount = a} :: DescribeDomainHealthResponse)

-- | The current health status of your cluster.
--
-- -   @Red@ - At least one primary shard is not allocated to any node.
--
-- -   @Yellow@ - All primary shards are allocated to nodes, but some
--     replicas aren’t.
--
-- -   @Green@ - All primary shards and their replicas are allocated to
--     nodes.
--
-- -   @NotAvailable@ - Unable to retrieve cluster health.
describeDomainHealthResponse_clusterHealth :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe DomainHealth)
describeDomainHealthResponse_clusterHealth = Lens.lens (\DescribeDomainHealthResponse' {clusterHealth} -> clusterHealth) (\s@DescribeDomainHealthResponse' {} a -> s {clusterHealth = a} :: DescribeDomainHealthResponse)

-- | The number of data nodes configured for the domain. If the service is
-- unable to fetch this information, it will return @NotAvailable@.
describeDomainHealthResponse_dataNodeCount :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_dataNodeCount = Lens.lens (\DescribeDomainHealthResponse' {dataNodeCount} -> dataNodeCount) (\s@DescribeDomainHealthResponse' {} a -> s {dataNodeCount = a} :: DescribeDomainHealthResponse)

-- | A boolean that indicates if dedicated master nodes are activated for the
-- domain.
describeDomainHealthResponse_dedicatedMaster :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Bool)
describeDomainHealthResponse_dedicatedMaster = Lens.lens (\DescribeDomainHealthResponse' {dedicatedMaster} -> dedicatedMaster) (\s@DescribeDomainHealthResponse' {} a -> s {dedicatedMaster = a} :: DescribeDomainHealthResponse)

-- | The current state of the domain.
--
-- -   @Processing@ - The domain has updates in progress.
--
-- -   @Active@ - Requested changes have been processed and deployed to the
--     domain.
describeDomainHealthResponse_domainState :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe DomainState)
describeDomainHealthResponse_domainState = Lens.lens (\DescribeDomainHealthResponse' {domainState} -> domainState) (\s@DescribeDomainHealthResponse' {} a -> s {domainState = a} :: DescribeDomainHealthResponse)

-- | A list of @EnvironmentInfo@ for the domain.
describeDomainHealthResponse_environmentInformation :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe [EnvironmentInfo])
describeDomainHealthResponse_environmentInformation = Lens.lens (\DescribeDomainHealthResponse' {environmentInformation} -> environmentInformation) (\s@DescribeDomainHealthResponse' {} a -> s {environmentInformation = a} :: DescribeDomainHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of nodes that can be elected as a master node. If dedicated
-- master nodes is turned on, this value is the number of dedicated master
-- nodes configured for the domain. If the service is unable to fetch this
-- information, it will return @NotAvailable@.
describeDomainHealthResponse_masterEligibleNodeCount :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_masterEligibleNodeCount = Lens.lens (\DescribeDomainHealthResponse' {masterEligibleNodeCount} -> masterEligibleNodeCount) (\s@DescribeDomainHealthResponse' {} a -> s {masterEligibleNodeCount = a} :: DescribeDomainHealthResponse)

-- | Indicates whether the domain has an elected master node.
--
-- -   __Available__ - The domain has an elected master node.
--
-- -   __UnAvailable__ - The master node hasn\'t yet been elected, and a
--     quorum to elect a new master node hasn\'t been reached.
describeDomainHealthResponse_masterNode :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe MasterNodeStatus)
describeDomainHealthResponse_masterNode = Lens.lens (\DescribeDomainHealthResponse' {masterNode} -> masterNode) (\s@DescribeDomainHealthResponse' {} a -> s {masterNode = a} :: DescribeDomainHealthResponse)

-- | The number of standby Availability Zones configured for the domain. If
-- the service is unable to fetch this information, it will return
-- @NotAvailable@.
describeDomainHealthResponse_standByAvailabilityZoneCount :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_standByAvailabilityZoneCount = Lens.lens (\DescribeDomainHealthResponse' {standByAvailabilityZoneCount} -> standByAvailabilityZoneCount) (\s@DescribeDomainHealthResponse' {} a -> s {standByAvailabilityZoneCount = a} :: DescribeDomainHealthResponse)

-- | The total number of primary and replica shards for the domain.
describeDomainHealthResponse_totalShards :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_totalShards = Lens.lens (\DescribeDomainHealthResponse' {totalShards} -> totalShards) (\s@DescribeDomainHealthResponse' {} a -> s {totalShards = a} :: DescribeDomainHealthResponse)

-- | The total number of primary and replica shards not allocated to any of
-- the nodes for the cluster.
describeDomainHealthResponse_totalUnAssignedShards :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_totalUnAssignedShards = Lens.lens (\DescribeDomainHealthResponse' {totalUnAssignedShards} -> totalUnAssignedShards) (\s@DescribeDomainHealthResponse' {} a -> s {totalUnAssignedShards = a} :: DescribeDomainHealthResponse)

-- | The number of warm nodes configured for the domain.
describeDomainHealthResponse_warmNodeCount :: Lens.Lens' DescribeDomainHealthResponse (Prelude.Maybe Prelude.Text)
describeDomainHealthResponse_warmNodeCount = Lens.lens (\DescribeDomainHealthResponse' {warmNodeCount} -> warmNodeCount) (\s@DescribeDomainHealthResponse' {} a -> s {warmNodeCount = a} :: DescribeDomainHealthResponse)

-- | The response's http status code.
describeDomainHealthResponse_httpStatus :: Lens.Lens' DescribeDomainHealthResponse Prelude.Int
describeDomainHealthResponse_httpStatus = Lens.lens (\DescribeDomainHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainHealthResponse' {} a -> s {httpStatus = a} :: DescribeDomainHealthResponse)

instance Prelude.NFData DescribeDomainHealthResponse where
  rnf DescribeDomainHealthResponse' {..} =
    Prelude.rnf activeAvailabilityZoneCount
      `Prelude.seq` Prelude.rnf availabilityZoneCount
      `Prelude.seq` Prelude.rnf clusterHealth
      `Prelude.seq` Prelude.rnf dataNodeCount
      `Prelude.seq` Prelude.rnf dedicatedMaster
      `Prelude.seq` Prelude.rnf domainState
      `Prelude.seq` Prelude.rnf environmentInformation
      `Prelude.seq` Prelude.rnf masterEligibleNodeCount
      `Prelude.seq` Prelude.rnf masterNode
      `Prelude.seq` Prelude.rnf standByAvailabilityZoneCount
      `Prelude.seq` Prelude.rnf totalShards
      `Prelude.seq` Prelude.rnf totalUnAssignedShards
      `Prelude.seq` Prelude.rnf warmNodeCount
      `Prelude.seq` Prelude.rnf httpStatus
