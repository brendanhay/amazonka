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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ManagedBlockChain.Types.NodeStatus
import qualified Amazonka.Prelude as Prelude

-- | A summary of configuration properties for a node.
--
-- /See:/ 'newNodeSummary' smart constructor.
data NodeSummary = NodeSummary'
  { -- | The Amazon Resource Name (ARN) of the node. For more information about
    -- ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the node was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The status of the node.
    status :: Prelude.Maybe NodeStatus,
    -- | The Availability Zone in which the node exists.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the node.
    id :: Prelude.Maybe Prelude.Text,
    -- | The EC2 instance type for the node.
    instanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'nodeSummary_arn' - The Amazon Resource Name (ARN) of the node. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'creationDate', 'nodeSummary_creationDate' - The date and time that the node was created.
--
-- 'status', 'nodeSummary_status' - The status of the node.
--
-- 'availabilityZone', 'nodeSummary_availabilityZone' - The Availability Zone in which the node exists.
--
-- 'id', 'nodeSummary_id' - The unique identifier of the node.
--
-- 'instanceType', 'nodeSummary_instanceType' - The EC2 instance type for the node.
newNodeSummary ::
  NodeSummary
newNodeSummary =
  NodeSummary'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      id = Prelude.Nothing,
      instanceType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the node. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
nodeSummary_arn :: Lens.Lens' NodeSummary (Prelude.Maybe Prelude.Text)
nodeSummary_arn = Lens.lens (\NodeSummary' {arn} -> arn) (\s@NodeSummary' {} a -> s {arn = a} :: NodeSummary)

-- | The date and time that the node was created.
nodeSummary_creationDate :: Lens.Lens' NodeSummary (Prelude.Maybe Prelude.UTCTime)
nodeSummary_creationDate = Lens.lens (\NodeSummary' {creationDate} -> creationDate) (\s@NodeSummary' {} a -> s {creationDate = a} :: NodeSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the node.
nodeSummary_status :: Lens.Lens' NodeSummary (Prelude.Maybe NodeStatus)
nodeSummary_status = Lens.lens (\NodeSummary' {status} -> status) (\s@NodeSummary' {} a -> s {status = a} :: NodeSummary)

-- | The Availability Zone in which the node exists.
nodeSummary_availabilityZone :: Lens.Lens' NodeSummary (Prelude.Maybe Prelude.Text)
nodeSummary_availabilityZone = Lens.lens (\NodeSummary' {availabilityZone} -> availabilityZone) (\s@NodeSummary' {} a -> s {availabilityZone = a} :: NodeSummary)

-- | The unique identifier of the node.
nodeSummary_id :: Lens.Lens' NodeSummary (Prelude.Maybe Prelude.Text)
nodeSummary_id = Lens.lens (\NodeSummary' {id} -> id) (\s@NodeSummary' {} a -> s {id = a} :: NodeSummary)

-- | The EC2 instance type for the node.
nodeSummary_instanceType :: Lens.Lens' NodeSummary (Prelude.Maybe Prelude.Text)
nodeSummary_instanceType = Lens.lens (\NodeSummary' {instanceType} -> instanceType) (\s@NodeSummary' {} a -> s {instanceType = a} :: NodeSummary)

instance Core.FromJSON NodeSummary where
  parseJSON =
    Core.withObject
      "NodeSummary"
      ( \x ->
          NodeSummary'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "InstanceType")
      )

instance Prelude.Hashable NodeSummary where
  hashWithSalt _salt NodeSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData NodeSummary where
  rnf NodeSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceType
