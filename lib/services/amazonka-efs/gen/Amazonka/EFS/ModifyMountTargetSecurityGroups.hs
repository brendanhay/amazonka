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
-- Module      : Amazonka.EFS.ModifyMountTargetSecurityGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EFS.ModifyMountTargetSecurityGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyMountTargetSecurityGroups' smart constructor.
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'
  { -- | An array of up to five VPC security group IDs.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the mount target whose security groups you want to modify.
    mountTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
modifyMountTargetSecurityGroups_securityGroups = Lens.lens (\ModifyMountTargetSecurityGroups' {securityGroups} -> securityGroups) (\s@ModifyMountTargetSecurityGroups' {} a -> s {securityGroups = a} :: ModifyMountTargetSecurityGroups) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the mount target whose security groups you want to modify.
modifyMountTargetSecurityGroups_mountTargetId :: Lens.Lens' ModifyMountTargetSecurityGroups Prelude.Text
modifyMountTargetSecurityGroups_mountTargetId = Lens.lens (\ModifyMountTargetSecurityGroups' {mountTargetId} -> mountTargetId) (\s@ModifyMountTargetSecurityGroups' {} a -> s {mountTargetId = a} :: ModifyMountTargetSecurityGroups)

instance
  Core.AWSRequest
    ModifyMountTargetSecurityGroups
  where
  type
    AWSResponse ModifyMountTargetSecurityGroups =
      ModifyMountTargetSecurityGroupsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      ModifyMountTargetSecurityGroupsResponse'

instance
  Prelude.Hashable
    ModifyMountTargetSecurityGroups
  where
  hashWithSalt
    _salt
    ModifyMountTargetSecurityGroups' {..} =
      _salt
        `Prelude.hashWithSalt` securityGroups
        `Prelude.hashWithSalt` mountTargetId

instance
  Prelude.NFData
    ModifyMountTargetSecurityGroups
  where
  rnf ModifyMountTargetSecurityGroups' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf mountTargetId

instance
  Data.ToHeaders
    ModifyMountTargetSecurityGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ModifyMountTargetSecurityGroups where
  toJSON ModifyMountTargetSecurityGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups
          ]
      )

instance Data.ToPath ModifyMountTargetSecurityGroups where
  toPath ModifyMountTargetSecurityGroups' {..} =
    Prelude.mconcat
      [ "/2015-02-01/mount-targets/",
        Data.toBS mountTargetId,
        "/security-groups"
      ]

instance Data.ToQuery ModifyMountTargetSecurityGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse = ModifyMountTargetSecurityGroupsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
