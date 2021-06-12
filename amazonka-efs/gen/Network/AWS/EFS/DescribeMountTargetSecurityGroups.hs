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
-- Module      : Network.AWS.EFS.DescribeMountTargetSecurityGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the security groups currently in effect for a mount target. This
-- operation requires that the network interface of the mount target has
-- been created and the lifecycle state of the mount target is not
-- @deleted@.
--
-- This operation requires permissions for the following actions:
--
-- -   @elasticfilesystem:DescribeMountTargetSecurityGroups@ action on the
--     mount target\'s file system.
--
-- -   @ec2:DescribeNetworkInterfaceAttribute@ action on the mount
--     target\'s network interface.
module Network.AWS.EFS.DescribeMountTargetSecurityGroups
  ( -- * Creating a Request
    DescribeMountTargetSecurityGroups (..),
    newDescribeMountTargetSecurityGroups,

    -- * Request Lenses
    describeMountTargetSecurityGroups_mountTargetId,

    -- * Destructuring the Response
    DescribeMountTargetSecurityGroupsResponse (..),
    newDescribeMountTargetSecurityGroupsResponse,

    -- * Response Lenses
    describeMountTargetSecurityGroupsResponse_httpStatus,
    describeMountTargetSecurityGroupsResponse_securityGroups,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeMountTargetSecurityGroups' smart constructor.
data DescribeMountTargetSecurityGroups = DescribeMountTargetSecurityGroups'
  { -- | The ID of the mount target whose security groups you want to retrieve.
    mountTargetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMountTargetSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountTargetId', 'describeMountTargetSecurityGroups_mountTargetId' - The ID of the mount target whose security groups you want to retrieve.
newDescribeMountTargetSecurityGroups ::
  -- | 'mountTargetId'
  Core.Text ->
  DescribeMountTargetSecurityGroups
newDescribeMountTargetSecurityGroups pMountTargetId_ =
  DescribeMountTargetSecurityGroups'
    { mountTargetId =
        pMountTargetId_
    }

-- | The ID of the mount target whose security groups you want to retrieve.
describeMountTargetSecurityGroups_mountTargetId :: Lens.Lens' DescribeMountTargetSecurityGroups Core.Text
describeMountTargetSecurityGroups_mountTargetId = Lens.lens (\DescribeMountTargetSecurityGroups' {mountTargetId} -> mountTargetId) (\s@DescribeMountTargetSecurityGroups' {} a -> s {mountTargetId = a} :: DescribeMountTargetSecurityGroups)

instance
  Core.AWSRequest
    DescribeMountTargetSecurityGroups
  where
  type
    AWSResponse DescribeMountTargetSecurityGroups =
      DescribeMountTargetSecurityGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMountTargetSecurityGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "SecurityGroups" Core..!@ Core.mempty)
      )

instance
  Core.Hashable
    DescribeMountTargetSecurityGroups

instance
  Core.NFData
    DescribeMountTargetSecurityGroups

instance
  Core.ToHeaders
    DescribeMountTargetSecurityGroups
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeMountTargetSecurityGroups
  where
  toPath DescribeMountTargetSecurityGroups' {..} =
    Core.mconcat
      [ "/2015-02-01/mount-targets/",
        Core.toBS mountTargetId,
        "/security-groups"
      ]

instance
  Core.ToQuery
    DescribeMountTargetSecurityGroups
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMountTargetSecurityGroupsResponse' smart constructor.
data DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of security groups.
    securityGroups :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMountTargetSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeMountTargetSecurityGroupsResponse_httpStatus' - The response's http status code.
--
-- 'securityGroups', 'describeMountTargetSecurityGroupsResponse_securityGroups' - An array of security groups.
newDescribeMountTargetSecurityGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMountTargetSecurityGroupsResponse
newDescribeMountTargetSecurityGroupsResponse
  pHttpStatus_ =
    DescribeMountTargetSecurityGroupsResponse'
      { httpStatus =
          pHttpStatus_,
        securityGroups = Core.mempty
      }

-- | The response's http status code.
describeMountTargetSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse Core.Int
describeMountTargetSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeMountTargetSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeMountTargetSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeMountTargetSecurityGroupsResponse)

-- | An array of security groups.
describeMountTargetSecurityGroupsResponse_securityGroups :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse [Core.Text]
describeMountTargetSecurityGroupsResponse_securityGroups = Lens.lens (\DescribeMountTargetSecurityGroupsResponse' {securityGroups} -> securityGroups) (\s@DescribeMountTargetSecurityGroupsResponse' {} a -> s {securityGroups = a} :: DescribeMountTargetSecurityGroupsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    DescribeMountTargetSecurityGroupsResponse
