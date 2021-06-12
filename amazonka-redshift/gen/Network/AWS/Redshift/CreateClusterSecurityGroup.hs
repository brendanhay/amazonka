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
-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters.
--
-- For information about managing security groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.CreateClusterSecurityGroup
  ( -- * Creating a Request
    CreateClusterSecurityGroup (..),
    newCreateClusterSecurityGroup,

    -- * Request Lenses
    createClusterSecurityGroup_tags,
    createClusterSecurityGroup_clusterSecurityGroupName,
    createClusterSecurityGroup_description,

    -- * Destructuring the Response
    CreateClusterSecurityGroupResponse (..),
    newCreateClusterSecurityGroupResponse,

    -- * Response Lenses
    createClusterSecurityGroupResponse_clusterSecurityGroup,
    createClusterSecurityGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateClusterSecurityGroup' smart constructor.
data CreateClusterSecurityGroup = CreateClusterSecurityGroup'
  { -- | A list of tag instances.
    tags :: Core.Maybe [Tag],
    -- | The name for the security group. Amazon Redshift stores the value as a
    -- lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain no more than 255 alphanumeric characters or hyphens.
    --
    -- -   Must not be \"Default\".
    --
    -- -   Must be unique for all security groups that are created by your AWS
    --     account.
    --
    -- Example: @examplesecuritygroup@
    clusterSecurityGroupName :: Core.Text,
    -- | A description for the security group.
    description :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClusterSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createClusterSecurityGroup_tags' - A list of tag instances.
--
-- 'clusterSecurityGroupName', 'createClusterSecurityGroup_clusterSecurityGroupName' - The name for the security group. Amazon Redshift stores the value as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain no more than 255 alphanumeric characters or hyphens.
--
-- -   Must not be \"Default\".
--
-- -   Must be unique for all security groups that are created by your AWS
--     account.
--
-- Example: @examplesecuritygroup@
--
-- 'description', 'createClusterSecurityGroup_description' - A description for the security group.
newCreateClusterSecurityGroup ::
  -- | 'clusterSecurityGroupName'
  Core.Text ->
  -- | 'description'
  Core.Text ->
  CreateClusterSecurityGroup
newCreateClusterSecurityGroup
  pClusterSecurityGroupName_
  pDescription_ =
    CreateClusterSecurityGroup'
      { tags = Core.Nothing,
        clusterSecurityGroupName =
          pClusterSecurityGroupName_,
        description = pDescription_
      }

-- | A list of tag instances.
createClusterSecurityGroup_tags :: Lens.Lens' CreateClusterSecurityGroup (Core.Maybe [Tag])
createClusterSecurityGroup_tags = Lens.lens (\CreateClusterSecurityGroup' {tags} -> tags) (\s@CreateClusterSecurityGroup' {} a -> s {tags = a} :: CreateClusterSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | The name for the security group. Amazon Redshift stores the value as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain no more than 255 alphanumeric characters or hyphens.
--
-- -   Must not be \"Default\".
--
-- -   Must be unique for all security groups that are created by your AWS
--     account.
--
-- Example: @examplesecuritygroup@
createClusterSecurityGroup_clusterSecurityGroupName :: Lens.Lens' CreateClusterSecurityGroup Core.Text
createClusterSecurityGroup_clusterSecurityGroupName = Lens.lens (\CreateClusterSecurityGroup' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@CreateClusterSecurityGroup' {} a -> s {clusterSecurityGroupName = a} :: CreateClusterSecurityGroup)

-- | A description for the security group.
createClusterSecurityGroup_description :: Lens.Lens' CreateClusterSecurityGroup Core.Text
createClusterSecurityGroup_description = Lens.lens (\CreateClusterSecurityGroup' {description} -> description) (\s@CreateClusterSecurityGroup' {} a -> s {description = a} :: CreateClusterSecurityGroup)

instance Core.AWSRequest CreateClusterSecurityGroup where
  type
    AWSResponse CreateClusterSecurityGroup =
      CreateClusterSecurityGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateClusterSecurityGroupResult"
      ( \s h x ->
          CreateClusterSecurityGroupResponse'
            Core.<$> (x Core..@? "ClusterSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateClusterSecurityGroup

instance Core.NFData CreateClusterSecurityGroup

instance Core.ToHeaders CreateClusterSecurityGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateClusterSecurityGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateClusterSecurityGroup where
  toQuery CreateClusterSecurityGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateClusterSecurityGroup" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "ClusterSecurityGroupName"
          Core.=: clusterSecurityGroupName,
        "Description" Core.=: description
      ]

-- | /See:/ 'newCreateClusterSecurityGroupResponse' smart constructor.
data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse'
  { clusterSecurityGroup :: Core.Maybe ClusterSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClusterSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSecurityGroup', 'createClusterSecurityGroupResponse_clusterSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'createClusterSecurityGroupResponse_httpStatus' - The response's http status code.
newCreateClusterSecurityGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateClusterSecurityGroupResponse
newCreateClusterSecurityGroupResponse pHttpStatus_ =
  CreateClusterSecurityGroupResponse'
    { clusterSecurityGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createClusterSecurityGroupResponse_clusterSecurityGroup :: Lens.Lens' CreateClusterSecurityGroupResponse (Core.Maybe ClusterSecurityGroup)
createClusterSecurityGroupResponse_clusterSecurityGroup = Lens.lens (\CreateClusterSecurityGroupResponse' {clusterSecurityGroup} -> clusterSecurityGroup) (\s@CreateClusterSecurityGroupResponse' {} a -> s {clusterSecurityGroup = a} :: CreateClusterSecurityGroupResponse)

-- | The response's http status code.
createClusterSecurityGroupResponse_httpStatus :: Lens.Lens' CreateClusterSecurityGroupResponse Core.Int
createClusterSecurityGroupResponse_httpStatus = Lens.lens (\CreateClusterSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@CreateClusterSecurityGroupResponse' {} a -> s {httpStatus = a} :: CreateClusterSecurityGroupResponse)

instance
  Core.NFData
    CreateClusterSecurityGroupResponse
