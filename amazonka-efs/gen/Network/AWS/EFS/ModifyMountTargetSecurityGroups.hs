{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EFS.ModifyMountTargetSecurityGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the set of security groups in effect for a mount target.
--
-- When you create a mount target, Amazon EFS also creates a new network
-- interface. For more information, see CreateMountTarget. This operation
-- replaces the security groups in effect for the network interface
-- associated with a mount target, with the @SecurityGroups@ provided in
-- the request. This operation requires that the network interface of the
-- mount target has been created and the lifecycle state of the mount
-- target is not @deleted@.
--
-- The operation requires permissions for the following actions:
--
-- -   @elasticfilesystem:ModifyMountTargetSecurityGroups@ action on the
--     mount target\'s file system.
--
-- -   @ec2:ModifyNetworkInterfaceAttribute@ action on the mount target\'s
--     network interface.
module Network.AWS.EFS.ModifyMountTargetSecurityGroups
  ( -- * Creating a Request
    ModifyMountTargetSecurityGroups (..),
    newModifyMountTargetSecurityGroups,

    -- * Request Lenses
    modifyMountTargetSecurityGroups_securityGroups,
    modifyMountTargetSecurityGroups_mountTargetId,

    -- * Destructuring the Response
    ModifyMountTargetSecurityGroupsResponse (..),
    newModifyMountTargetSecurityGroupsResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyMountTargetSecurityGroups' smart constructor.
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'
  { -- | An array of up to five VPC security group IDs.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the mount target whose security groups you want to modify.
    mountTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyMountTargetSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'modifyMountTargetSecurityGroups_securityGroups' - An array of up to five VPC security group IDs.
--
-- 'mountTargetId', 'modifyMountTargetSecurityGroups_mountTargetId' - The ID of the mount target whose security groups you want to modify.
newModifyMountTargetSecurityGroups ::
  -- | 'mountTargetId'
  Prelude.Text ->
  ModifyMountTargetSecurityGroups
newModifyMountTargetSecurityGroups pMountTargetId_ =
  ModifyMountTargetSecurityGroups'
    { securityGroups =
        Prelude.Nothing,
      mountTargetId = pMountTargetId_
    }

-- | An array of up to five VPC security group IDs.
modifyMountTargetSecurityGroups_securityGroups :: Lens.Lens' ModifyMountTargetSecurityGroups (Prelude.Maybe [Prelude.Text])
modifyMountTargetSecurityGroups_securityGroups = Lens.lens (\ModifyMountTargetSecurityGroups' {securityGroups} -> securityGroups) (\s@ModifyMountTargetSecurityGroups' {} a -> s {securityGroups = a} :: ModifyMountTargetSecurityGroups) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the mount target whose security groups you want to modify.
modifyMountTargetSecurityGroups_mountTargetId :: Lens.Lens' ModifyMountTargetSecurityGroups Prelude.Text
modifyMountTargetSecurityGroups_mountTargetId = Lens.lens (\ModifyMountTargetSecurityGroups' {mountTargetId} -> mountTargetId) (\s@ModifyMountTargetSecurityGroups' {} a -> s {mountTargetId = a} :: ModifyMountTargetSecurityGroups)

instance
  Prelude.AWSRequest
    ModifyMountTargetSecurityGroups
  where
  type
    Rs ModifyMountTargetSecurityGroups =
      ModifyMountTargetSecurityGroupsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull
      ModifyMountTargetSecurityGroupsResponse'

instance
  Prelude.Hashable
    ModifyMountTargetSecurityGroups

instance
  Prelude.NFData
    ModifyMountTargetSecurityGroups

instance
  Prelude.ToHeaders
    ModifyMountTargetSecurityGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToJSON
    ModifyMountTargetSecurityGroups
  where
  toJSON ModifyMountTargetSecurityGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityGroups" Prelude..=)
              Prelude.<$> securityGroups
          ]
      )

instance
  Prelude.ToPath
    ModifyMountTargetSecurityGroups
  where
  toPath ModifyMountTargetSecurityGroups' {..} =
    Prelude.mconcat
      [ "/2015-02-01/mount-targets/",
        Prelude.toBS mountTargetId,
        "/security-groups"
      ]

instance
  Prelude.ToQuery
    ModifyMountTargetSecurityGroups
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse = ModifyMountTargetSecurityGroupsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyMountTargetSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyMountTargetSecurityGroupsResponse ::
  ModifyMountTargetSecurityGroupsResponse
newModifyMountTargetSecurityGroupsResponse =
  ModifyMountTargetSecurityGroupsResponse'

instance
  Prelude.NFData
    ModifyMountTargetSecurityGroupsResponse
