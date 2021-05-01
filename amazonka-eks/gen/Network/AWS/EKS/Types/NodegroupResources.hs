{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EKS.Types.NodegroupResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.NodegroupResources where

import Network.AWS.EKS.Types.AutoScalingGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the resources associated with the node group,
-- such as Auto Scaling groups and security groups for remote access.
--
-- /See:/ 'newNodegroupResources' smart constructor.
data NodegroupResources = NodegroupResources'
  { -- | The remote access security group associated with the node group. This
    -- security group controls SSH access to the nodes.
    remoteAccessSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | The Auto Scaling groups associated with the node group.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NodegroupResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessSecurityGroup', 'nodegroupResources_remoteAccessSecurityGroup' - The remote access security group associated with the node group. This
-- security group controls SSH access to the nodes.
--
-- 'autoScalingGroups', 'nodegroupResources_autoScalingGroups' - The Auto Scaling groups associated with the node group.
newNodegroupResources ::
  NodegroupResources
newNodegroupResources =
  NodegroupResources'
    { remoteAccessSecurityGroup =
        Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing
    }

-- | The remote access security group associated with the node group. This
-- security group controls SSH access to the nodes.
nodegroupResources_remoteAccessSecurityGroup :: Lens.Lens' NodegroupResources (Prelude.Maybe Prelude.Text)
nodegroupResources_remoteAccessSecurityGroup = Lens.lens (\NodegroupResources' {remoteAccessSecurityGroup} -> remoteAccessSecurityGroup) (\s@NodegroupResources' {} a -> s {remoteAccessSecurityGroup = a} :: NodegroupResources)

-- | The Auto Scaling groups associated with the node group.
nodegroupResources_autoScalingGroups :: Lens.Lens' NodegroupResources (Prelude.Maybe [AutoScalingGroup])
nodegroupResources_autoScalingGroups = Lens.lens (\NodegroupResources' {autoScalingGroups} -> autoScalingGroups) (\s@NodegroupResources' {} a -> s {autoScalingGroups = a} :: NodegroupResources) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON NodegroupResources where
  parseJSON =
    Prelude.withObject
      "NodegroupResources"
      ( \x ->
          NodegroupResources'
            Prelude.<$> (x Prelude..:? "remoteAccessSecurityGroup")
            Prelude.<*> ( x Prelude..:? "autoScalingGroups"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable NodegroupResources

instance Prelude.NFData NodegroupResources
