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
-- Module      : Amazonka.EFS.DescribeMountTargetSecurityGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EFS.DescribeMountTargetSecurityGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeMountTargetSecurityGroups' smart constructor.
data DescribeMountTargetSecurityGroups = DescribeMountTargetSecurityGroups'
  { -- | The ID of the mount target whose security groups you want to retrieve.
    mountTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeMountTargetSecurityGroups
newDescribeMountTargetSecurityGroups pMountTargetId_ =
  DescribeMountTargetSecurityGroups'
    { mountTargetId =
        pMountTargetId_
    }

-- | The ID of the mount target whose security groups you want to retrieve.
describeMountTargetSecurityGroups_mountTargetId :: Lens.Lens' DescribeMountTargetSecurityGroups Prelude.Text
describeMountTargetSecurityGroups_mountTargetId = Lens.lens (\DescribeMountTargetSecurityGroups' {mountTargetId} -> mountTargetId) (\s@DescribeMountTargetSecurityGroups' {} a -> s {mountTargetId = a} :: DescribeMountTargetSecurityGroups)

instance
  Core.AWSRequest
    DescribeMountTargetSecurityGroups
  where
  type
    AWSResponse DescribeMountTargetSecurityGroups =
      DescribeMountTargetSecurityGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMountTargetSecurityGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..?> "SecurityGroups"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    DescribeMountTargetSecurityGroups
  where
  hashWithSalt
    _salt
    DescribeMountTargetSecurityGroups' {..} =
      _salt `Prelude.hashWithSalt` mountTargetId

instance
  Prelude.NFData
    DescribeMountTargetSecurityGroups
  where
  rnf DescribeMountTargetSecurityGroups' {..} =
    Prelude.rnf mountTargetId

instance
  Data.ToHeaders
    DescribeMountTargetSecurityGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeMountTargetSecurityGroups
  where
  toPath DescribeMountTargetSecurityGroups' {..} =
    Prelude.mconcat
      [ "/2015-02-01/mount-targets/",
        Data.toBS mountTargetId,
        "/security-groups"
      ]

instance
  Data.ToQuery
    DescribeMountTargetSecurityGroups
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMountTargetSecurityGroupsResponse' smart constructor.
data DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of security groups.
    securityGroups :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMountTargetSecurityGroupsResponse
newDescribeMountTargetSecurityGroupsResponse
  pHttpStatus_ =
    DescribeMountTargetSecurityGroupsResponse'
      { httpStatus =
          pHttpStatus_,
        securityGroups = Prelude.mempty
      }

-- | The response's http status code.
describeMountTargetSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse Prelude.Int
describeMountTargetSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeMountTargetSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeMountTargetSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeMountTargetSecurityGroupsResponse)

-- | An array of security groups.
describeMountTargetSecurityGroupsResponse_securityGroups :: Lens.Lens' DescribeMountTargetSecurityGroupsResponse [Prelude.Text]
describeMountTargetSecurityGroupsResponse_securityGroups = Lens.lens (\DescribeMountTargetSecurityGroupsResponse' {securityGroups} -> securityGroups) (\s@DescribeMountTargetSecurityGroupsResponse' {} a -> s {securityGroups = a} :: DescribeMountTargetSecurityGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeMountTargetSecurityGroupsResponse
  where
  rnf DescribeMountTargetSecurityGroupsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf securityGroups
