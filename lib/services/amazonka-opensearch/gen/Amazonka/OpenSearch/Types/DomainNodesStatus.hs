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
-- Module      : Amazonka.OpenSearch.Types.DomainNodesStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainNodesStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.NodeStatus
import Amazonka.OpenSearch.Types.NodeType
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Container for information about nodes on the domain.
--
-- /See:/ 'newDomainNodesStatus' smart constructor.
data DomainNodesStatus = DomainNodesStatus'
  { -- | The Availability Zone of the node.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type information of the node.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | The ID of the node.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the node is active or in standby.
    nodeStatus :: Prelude.Maybe NodeStatus,
    -- | Indicates whether the nodes is a data, master, or ultrawarm node.
    nodeType :: Prelude.Maybe NodeType,
    -- | The storage size of the node, in GiB.
    storageSize :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the node has EBS or instance storage.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | If the nodes has EBS storage, indicates if the volume type is GP2 or
    -- GP3. Only applicable for data nodes.
    storageVolumeType :: Prelude.Maybe VolumeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainNodesStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'domainNodesStatus_availabilityZone' - The Availability Zone of the node.
--
-- 'instanceType', 'domainNodesStatus_instanceType' - The instance type information of the node.
--
-- 'nodeId', 'domainNodesStatus_nodeId' - The ID of the node.
--
-- 'nodeStatus', 'domainNodesStatus_nodeStatus' - Indicates if the node is active or in standby.
--
-- 'nodeType', 'domainNodesStatus_nodeType' - Indicates whether the nodes is a data, master, or ultrawarm node.
--
-- 'storageSize', 'domainNodesStatus_storageSize' - The storage size of the node, in GiB.
--
-- 'storageType', 'domainNodesStatus_storageType' - Indicates if the node has EBS or instance storage.
--
-- 'storageVolumeType', 'domainNodesStatus_storageVolumeType' - If the nodes has EBS storage, indicates if the volume type is GP2 or
-- GP3. Only applicable for data nodes.
newDomainNodesStatus ::
  DomainNodesStatus
newDomainNodesStatus =
  DomainNodesStatus'
    { availabilityZone =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      nodeId = Prelude.Nothing,
      nodeStatus = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      storageSize = Prelude.Nothing,
      storageType = Prelude.Nothing,
      storageVolumeType = Prelude.Nothing
    }

-- | The Availability Zone of the node.
domainNodesStatus_availabilityZone :: Lens.Lens' DomainNodesStatus (Prelude.Maybe Prelude.Text)
domainNodesStatus_availabilityZone = Lens.lens (\DomainNodesStatus' {availabilityZone} -> availabilityZone) (\s@DomainNodesStatus' {} a -> s {availabilityZone = a} :: DomainNodesStatus)

-- | The instance type information of the node.
domainNodesStatus_instanceType :: Lens.Lens' DomainNodesStatus (Prelude.Maybe OpenSearchPartitionInstanceType)
domainNodesStatus_instanceType = Lens.lens (\DomainNodesStatus' {instanceType} -> instanceType) (\s@DomainNodesStatus' {} a -> s {instanceType = a} :: DomainNodesStatus)

-- | The ID of the node.
domainNodesStatus_nodeId :: Lens.Lens' DomainNodesStatus (Prelude.Maybe Prelude.Text)
domainNodesStatus_nodeId = Lens.lens (\DomainNodesStatus' {nodeId} -> nodeId) (\s@DomainNodesStatus' {} a -> s {nodeId = a} :: DomainNodesStatus)

-- | Indicates if the node is active or in standby.
domainNodesStatus_nodeStatus :: Lens.Lens' DomainNodesStatus (Prelude.Maybe NodeStatus)
domainNodesStatus_nodeStatus = Lens.lens (\DomainNodesStatus' {nodeStatus} -> nodeStatus) (\s@DomainNodesStatus' {} a -> s {nodeStatus = a} :: DomainNodesStatus)

-- | Indicates whether the nodes is a data, master, or ultrawarm node.
domainNodesStatus_nodeType :: Lens.Lens' DomainNodesStatus (Prelude.Maybe NodeType)
domainNodesStatus_nodeType = Lens.lens (\DomainNodesStatus' {nodeType} -> nodeType) (\s@DomainNodesStatus' {} a -> s {nodeType = a} :: DomainNodesStatus)

-- | The storage size of the node, in GiB.
domainNodesStatus_storageSize :: Lens.Lens' DomainNodesStatus (Prelude.Maybe Prelude.Text)
domainNodesStatus_storageSize = Lens.lens (\DomainNodesStatus' {storageSize} -> storageSize) (\s@DomainNodesStatus' {} a -> s {storageSize = a} :: DomainNodesStatus)

-- | Indicates if the node has EBS or instance storage.
domainNodesStatus_storageType :: Lens.Lens' DomainNodesStatus (Prelude.Maybe Prelude.Text)
domainNodesStatus_storageType = Lens.lens (\DomainNodesStatus' {storageType} -> storageType) (\s@DomainNodesStatus' {} a -> s {storageType = a} :: DomainNodesStatus)

-- | If the nodes has EBS storage, indicates if the volume type is GP2 or
-- GP3. Only applicable for data nodes.
domainNodesStatus_storageVolumeType :: Lens.Lens' DomainNodesStatus (Prelude.Maybe VolumeType)
domainNodesStatus_storageVolumeType = Lens.lens (\DomainNodesStatus' {storageVolumeType} -> storageVolumeType) (\s@DomainNodesStatus' {} a -> s {storageVolumeType = a} :: DomainNodesStatus)

instance Data.FromJSON DomainNodesStatus where
  parseJSON =
    Data.withObject
      "DomainNodesStatus"
      ( \x ->
          DomainNodesStatus'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "NodeId")
            Prelude.<*> (x Data..:? "NodeStatus")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "StorageSize")
            Prelude.<*> (x Data..:? "StorageType")
            Prelude.<*> (x Data..:? "StorageVolumeType")
      )

instance Prelude.Hashable DomainNodesStatus where
  hashWithSalt _salt DomainNodesStatus' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` nodeId
      `Prelude.hashWithSalt` nodeStatus
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` storageSize
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` storageVolumeType

instance Prelude.NFData DomainNodesStatus where
  rnf DomainNodesStatus' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf nodeStatus
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf storageSize
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf storageVolumeType
