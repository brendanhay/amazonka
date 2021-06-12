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
-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift subnet group. You must provide a list of
-- one or more subnets in your existing Amazon Virtual Private Cloud
-- (Amazon VPC) when creating Amazon Redshift subnet group.
--
-- For information about subnet groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html Amazon Redshift Cluster Subnet Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.CreateClusterSubnetGroup
  ( -- * Creating a Request
    CreateClusterSubnetGroup (..),
    newCreateClusterSubnetGroup,

    -- * Request Lenses
    createClusterSubnetGroup_tags,
    createClusterSubnetGroup_clusterSubnetGroupName,
    createClusterSubnetGroup_description,
    createClusterSubnetGroup_subnetIds,

    -- * Destructuring the Response
    CreateClusterSubnetGroupResponse (..),
    newCreateClusterSubnetGroupResponse,

    -- * Response Lenses
    createClusterSubnetGroupResponse_clusterSubnetGroup,
    createClusterSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateClusterSubnetGroup' smart constructor.
data CreateClusterSubnetGroup = CreateClusterSubnetGroup'
  { -- | A list of tag instances.
    tags :: Core.Maybe [Tag],
    -- | The name for the subnet group. Amazon Redshift stores the value as a
    -- lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain no more than 255 alphanumeric characters or hyphens.
    --
    -- -   Must not be \"Default\".
    --
    -- -   Must be unique for all subnet groups that are created by your AWS
    --     account.
    --
    -- Example: @examplesubnetgroup@
    clusterSubnetGroupName :: Core.Text,
    -- | A description for the subnet group.
    description :: Core.Text,
    -- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
    -- single request.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClusterSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createClusterSubnetGroup_tags' - A list of tag instances.
--
-- 'clusterSubnetGroupName', 'createClusterSubnetGroup_clusterSubnetGroupName' - The name for the subnet group. Amazon Redshift stores the value as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain no more than 255 alphanumeric characters or hyphens.
--
-- -   Must not be \"Default\".
--
-- -   Must be unique for all subnet groups that are created by your AWS
--     account.
--
-- Example: @examplesubnetgroup@
--
-- 'description', 'createClusterSubnetGroup_description' - A description for the subnet group.
--
-- 'subnetIds', 'createClusterSubnetGroup_subnetIds' - An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
newCreateClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Core.Text ->
  -- | 'description'
  Core.Text ->
  CreateClusterSubnetGroup
newCreateClusterSubnetGroup
  pClusterSubnetGroupName_
  pDescription_ =
    CreateClusterSubnetGroup'
      { tags = Core.Nothing,
        clusterSubnetGroupName = pClusterSubnetGroupName_,
        description = pDescription_,
        subnetIds = Core.mempty
      }

-- | A list of tag instances.
createClusterSubnetGroup_tags :: Lens.Lens' CreateClusterSubnetGroup (Core.Maybe [Tag])
createClusterSubnetGroup_tags = Lens.lens (\CreateClusterSubnetGroup' {tags} -> tags) (\s@CreateClusterSubnetGroup' {} a -> s {tags = a} :: CreateClusterSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | The name for the subnet group. Amazon Redshift stores the value as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain no more than 255 alphanumeric characters or hyphens.
--
-- -   Must not be \"Default\".
--
-- -   Must be unique for all subnet groups that are created by your AWS
--     account.
--
-- Example: @examplesubnetgroup@
createClusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' CreateClusterSubnetGroup Core.Text
createClusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\CreateClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@CreateClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: CreateClusterSubnetGroup)

-- | A description for the subnet group.
createClusterSubnetGroup_description :: Lens.Lens' CreateClusterSubnetGroup Core.Text
createClusterSubnetGroup_description = Lens.lens (\CreateClusterSubnetGroup' {description} -> description) (\s@CreateClusterSubnetGroup' {} a -> s {description = a} :: CreateClusterSubnetGroup)

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
createClusterSubnetGroup_subnetIds :: Lens.Lens' CreateClusterSubnetGroup [Core.Text]
createClusterSubnetGroup_subnetIds = Lens.lens (\CreateClusterSubnetGroup' {subnetIds} -> subnetIds) (\s@CreateClusterSubnetGroup' {} a -> s {subnetIds = a} :: CreateClusterSubnetGroup) Core.. Lens._Coerce

instance Core.AWSRequest CreateClusterSubnetGroup where
  type
    AWSResponse CreateClusterSubnetGroup =
      CreateClusterSubnetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateClusterSubnetGroupResult"
      ( \s h x ->
          CreateClusterSubnetGroupResponse'
            Core.<$> (x Core..@? "ClusterSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateClusterSubnetGroup

instance Core.NFData CreateClusterSubnetGroup

instance Core.ToHeaders CreateClusterSubnetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateClusterSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateClusterSubnetGroup where
  toQuery CreateClusterSubnetGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateClusterSubnetGroup" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "ClusterSubnetGroupName"
          Core.=: clusterSubnetGroupName,
        "Description" Core.=: description,
        "SubnetIds"
          Core.=: Core.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'newCreateClusterSubnetGroupResponse' smart constructor.
data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Core.Maybe ClusterSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClusterSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroup', 'createClusterSubnetGroupResponse_clusterSubnetGroup' - Undocumented member.
--
-- 'httpStatus', 'createClusterSubnetGroupResponse_httpStatus' - The response's http status code.
newCreateClusterSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateClusterSubnetGroupResponse
newCreateClusterSubnetGroupResponse pHttpStatus_ =
  CreateClusterSubnetGroupResponse'
    { clusterSubnetGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createClusterSubnetGroupResponse_clusterSubnetGroup :: Lens.Lens' CreateClusterSubnetGroupResponse (Core.Maybe ClusterSubnetGroup)
createClusterSubnetGroupResponse_clusterSubnetGroup = Lens.lens (\CreateClusterSubnetGroupResponse' {clusterSubnetGroup} -> clusterSubnetGroup) (\s@CreateClusterSubnetGroupResponse' {} a -> s {clusterSubnetGroup = a} :: CreateClusterSubnetGroupResponse)

-- | The response's http status code.
createClusterSubnetGroupResponse_httpStatus :: Lens.Lens' CreateClusterSubnetGroupResponse Core.Int
createClusterSubnetGroupResponse_httpStatus = Lens.lens (\CreateClusterSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateClusterSubnetGroupResponse' {} a -> s {httpStatus = a} :: CreateClusterSubnetGroupResponse)

instance Core.NFData CreateClusterSubnetGroupResponse
