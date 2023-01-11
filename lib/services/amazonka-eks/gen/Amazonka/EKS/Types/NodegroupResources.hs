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
-- Module      : Amazonka.EKS.Types.NodegroupResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.NodegroupResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.AutoScalingGroup
import qualified Amazonka.Prelude as Prelude

-- | An object representing the resources associated with the node group,
-- such as Auto Scaling groups and security groups for remote access.
--
-- /See:/ 'newNodegroupResources' smart constructor.
data NodegroupResources = NodegroupResources'
  { -- | The Auto Scaling groups associated with the node group.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup],
    -- | The remote access security group associated with the node group. This
    -- security group controls SSH access to the nodes.
    remoteAccessSecurityGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodegroupResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroups', 'nodegroupResources_autoScalingGroups' - The Auto Scaling groups associated with the node group.
--
-- 'remoteAccessSecurityGroup', 'nodegroupResources_remoteAccessSecurityGroup' - The remote access security group associated with the node group. This
-- security group controls SSH access to the nodes.
newNodegroupResources ::
  NodegroupResources
newNodegroupResources =
  NodegroupResources'
    { autoScalingGroups =
        Prelude.Nothing,
      remoteAccessSecurityGroup = Prelude.Nothing
    }

-- | The Auto Scaling groups associated with the node group.
nodegroupResources_autoScalingGroups :: Lens.Lens' NodegroupResources (Prelude.Maybe [AutoScalingGroup])
nodegroupResources_autoScalingGroups = Lens.lens (\NodegroupResources' {autoScalingGroups} -> autoScalingGroups) (\s@NodegroupResources' {} a -> s {autoScalingGroups = a} :: NodegroupResources) Prelude.. Lens.mapping Lens.coerced

-- | The remote access security group associated with the node group. This
-- security group controls SSH access to the nodes.
nodegroupResources_remoteAccessSecurityGroup :: Lens.Lens' NodegroupResources (Prelude.Maybe Prelude.Text)
nodegroupResources_remoteAccessSecurityGroup = Lens.lens (\NodegroupResources' {remoteAccessSecurityGroup} -> remoteAccessSecurityGroup) (\s@NodegroupResources' {} a -> s {remoteAccessSecurityGroup = a} :: NodegroupResources)

instance Data.FromJSON NodegroupResources where
  parseJSON =
    Data.withObject
      "NodegroupResources"
      ( \x ->
          NodegroupResources'
            Prelude.<$> ( x Data..:? "autoScalingGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "remoteAccessSecurityGroup")
      )

instance Prelude.Hashable NodegroupResources where
  hashWithSalt _salt NodegroupResources' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroups
      `Prelude.hashWithSalt` remoteAccessSecurityGroup

instance Prelude.NFData NodegroupResources where
  rnf NodegroupResources' {..} =
    Prelude.rnf autoScalingGroups
      `Prelude.seq` Prelude.rnf remoteAccessSecurityGroup
