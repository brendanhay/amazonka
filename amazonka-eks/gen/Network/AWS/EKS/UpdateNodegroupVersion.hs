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
-- Module      : Network.AWS.EKS.UpdateNodegroupVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Kubernetes version or AMI version of an Amazon EKS managed
-- node group.
--
-- You can update a node group using a launch template only if the node
-- group was originally deployed with a launch template. If you need to
-- update a custom AMI in a node group that was deployed with a launch
-- template, then update your custom AMI, specify the new ID in a new
-- version of the launch template, and then update the node group to the
-- new version of the launch template.
--
-- If you update without a launch template, then you can update to the
-- latest available AMI version of a node group\'s current Kubernetes
-- version by not specifying a Kubernetes version in the request. You can
-- update to the latest AMI version of your cluster\'s current Kubernetes
-- version by specifying your cluster\'s Kubernetes version in the request.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
-- in the /Amazon EKS User Guide/.
--
-- You cannot roll back a node group to an earlier Kubernetes version or
-- AMI version.
--
-- When a node in a managed node group is terminated due to a scaling
-- action or update, the pods in that node are drained first. Amazon EKS
-- attempts to drain the nodes gracefully and will fail if it is unable to
-- do so. You can @force@ the update if Amazon EKS is unable to drain the
-- nodes as a result of a pod disruption budget issue.
module Network.AWS.EKS.UpdateNodegroupVersion
  ( -- * Creating a Request
    UpdateNodegroupVersion (..),
    newUpdateNodegroupVersion,

    -- * Request Lenses
    updateNodegroupVersion_releaseVersion,
    updateNodegroupVersion_force,
    updateNodegroupVersion_launchTemplate,
    updateNodegroupVersion_version,
    updateNodegroupVersion_clientRequestToken,
    updateNodegroupVersion_clusterName,
    updateNodegroupVersion_nodegroupName,

    -- * Destructuring the Response
    UpdateNodegroupVersionResponse (..),
    newUpdateNodegroupVersionResponse,

    -- * Response Lenses
    updateNodegroupVersionResponse_update,
    updateNodegroupVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateNodegroupVersion' smart constructor.
data UpdateNodegroupVersion = UpdateNodegroupVersion'
  { -- | The AMI version of the Amazon EKS optimized AMI to use for the update.
    -- By default, the latest available AMI version for the node group\'s
    -- Kubernetes version is used. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
    -- in the /Amazon EKS User Guide/. If you specify @launchTemplate@, and
    -- your launch template uses a custom AMI, then don\'t specify
    -- @releaseVersion@, or the node group update will fail. For more
    -- information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    releaseVersion :: Core.Maybe Core.Text,
    -- | Force the update if the existing node group\'s pods are unable to be
    -- drained due to a pod disruption budget issue. If an update fails because
    -- pods could not be drained, you can force the update after it fails to
    -- terminate the old node whether or not any pods are running on the node.
    force :: Core.Maybe Core.Bool,
    -- | An object representing a node group\'s launch template specification.
    -- You can only update a node group using a launch template if the node
    -- group was originally deployed with a launch template.
    launchTemplate :: Core.Maybe LaunchTemplateSpecification,
    -- | The Kubernetes version to update to. If no version is specified, then
    -- the Kubernetes version of the node group does not change. You can
    -- specify the Kubernetes version of the cluster to update the node group
    -- to the latest AMI version of the cluster\'s Kubernetes version. If you
    -- specify @launchTemplate@, and your launch template uses a custom AMI,
    -- then don\'t specify @version@, or the node group update will fail. For
    -- more information about using launch templates with Amazon EKS, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
    -- in the Amazon EKS User Guide.
    version :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name of the Amazon EKS cluster that is associated with the managed
    -- node group to update.
    clusterName :: Core.Text,
    -- | The name of the managed node group to update.
    nodegroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNodegroupVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseVersion', 'updateNodegroupVersion_releaseVersion' - The AMI version of the Amazon EKS optimized AMI to use for the update.
-- By default, the latest available AMI version for the node group\'s
-- Kubernetes version is used. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
-- in the /Amazon EKS User Guide/. If you specify @launchTemplate@, and
-- your launch template uses a custom AMI, then don\'t specify
-- @releaseVersion@, or the node group update will fail. For more
-- information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'force', 'updateNodegroupVersion_force' - Force the update if the existing node group\'s pods are unable to be
-- drained due to a pod disruption budget issue. If an update fails because
-- pods could not be drained, you can force the update after it fails to
-- terminate the old node whether or not any pods are running on the node.
--
-- 'launchTemplate', 'updateNodegroupVersion_launchTemplate' - An object representing a node group\'s launch template specification.
-- You can only update a node group using a launch template if the node
-- group was originally deployed with a launch template.
--
-- 'version', 'updateNodegroupVersion_version' - The Kubernetes version to update to. If no version is specified, then
-- the Kubernetes version of the node group does not change. You can
-- specify the Kubernetes version of the cluster to update the node group
-- to the latest AMI version of the cluster\'s Kubernetes version. If you
-- specify @launchTemplate@, and your launch template uses a custom AMI,
-- then don\'t specify @version@, or the node group update will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
--
-- 'clientRequestToken', 'updateNodegroupVersion_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'clusterName', 'updateNodegroupVersion_clusterName' - The name of the Amazon EKS cluster that is associated with the managed
-- node group to update.
--
-- 'nodegroupName', 'updateNodegroupVersion_nodegroupName' - The name of the managed node group to update.
newUpdateNodegroupVersion ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'nodegroupName'
  Core.Text ->
  UpdateNodegroupVersion
newUpdateNodegroupVersion
  pClusterName_
  pNodegroupName_ =
    UpdateNodegroupVersion'
      { releaseVersion =
          Core.Nothing,
        force = Core.Nothing,
        launchTemplate = Core.Nothing,
        version = Core.Nothing,
        clientRequestToken = Core.Nothing,
        clusterName = pClusterName_,
        nodegroupName = pNodegroupName_
      }

-- | The AMI version of the Amazon EKS optimized AMI to use for the update.
-- By default, the latest available AMI version for the node group\'s
-- Kubernetes version is used. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-linux-ami-versions.html Amazon EKS optimized Amazon Linux 2 AMI versions>
-- in the /Amazon EKS User Guide/. If you specify @launchTemplate@, and
-- your launch template uses a custom AMI, then don\'t specify
-- @releaseVersion@, or the node group update will fail. For more
-- information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
updateNodegroupVersion_releaseVersion :: Lens.Lens' UpdateNodegroupVersion (Core.Maybe Core.Text)
updateNodegroupVersion_releaseVersion = Lens.lens (\UpdateNodegroupVersion' {releaseVersion} -> releaseVersion) (\s@UpdateNodegroupVersion' {} a -> s {releaseVersion = a} :: UpdateNodegroupVersion)

-- | Force the update if the existing node group\'s pods are unable to be
-- drained due to a pod disruption budget issue. If an update fails because
-- pods could not be drained, you can force the update after it fails to
-- terminate the old node whether or not any pods are running on the node.
updateNodegroupVersion_force :: Lens.Lens' UpdateNodegroupVersion (Core.Maybe Core.Bool)
updateNodegroupVersion_force = Lens.lens (\UpdateNodegroupVersion' {force} -> force) (\s@UpdateNodegroupVersion' {} a -> s {force = a} :: UpdateNodegroupVersion)

-- | An object representing a node group\'s launch template specification.
-- You can only update a node group using a launch template if the node
-- group was originally deployed with a launch template.
updateNodegroupVersion_launchTemplate :: Lens.Lens' UpdateNodegroupVersion (Core.Maybe LaunchTemplateSpecification)
updateNodegroupVersion_launchTemplate = Lens.lens (\UpdateNodegroupVersion' {launchTemplate} -> launchTemplate) (\s@UpdateNodegroupVersion' {} a -> s {launchTemplate = a} :: UpdateNodegroupVersion)

-- | The Kubernetes version to update to. If no version is specified, then
-- the Kubernetes version of the node group does not change. You can
-- specify the Kubernetes version of the cluster to update the node group
-- to the latest AMI version of the cluster\'s Kubernetes version. If you
-- specify @launchTemplate@, and your launch template uses a custom AMI,
-- then don\'t specify @version@, or the node group update will fail. For
-- more information about using launch templates with Amazon EKS, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/launch-templates.html Launch template support>
-- in the Amazon EKS User Guide.
updateNodegroupVersion_version :: Lens.Lens' UpdateNodegroupVersion (Core.Maybe Core.Text)
updateNodegroupVersion_version = Lens.lens (\UpdateNodegroupVersion' {version} -> version) (\s@UpdateNodegroupVersion' {} a -> s {version = a} :: UpdateNodegroupVersion)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateNodegroupVersion_clientRequestToken :: Lens.Lens' UpdateNodegroupVersion (Core.Maybe Core.Text)
updateNodegroupVersion_clientRequestToken = Lens.lens (\UpdateNodegroupVersion' {clientRequestToken} -> clientRequestToken) (\s@UpdateNodegroupVersion' {} a -> s {clientRequestToken = a} :: UpdateNodegroupVersion)

-- | The name of the Amazon EKS cluster that is associated with the managed
-- node group to update.
updateNodegroupVersion_clusterName :: Lens.Lens' UpdateNodegroupVersion Core.Text
updateNodegroupVersion_clusterName = Lens.lens (\UpdateNodegroupVersion' {clusterName} -> clusterName) (\s@UpdateNodegroupVersion' {} a -> s {clusterName = a} :: UpdateNodegroupVersion)

-- | The name of the managed node group to update.
updateNodegroupVersion_nodegroupName :: Lens.Lens' UpdateNodegroupVersion Core.Text
updateNodegroupVersion_nodegroupName = Lens.lens (\UpdateNodegroupVersion' {nodegroupName} -> nodegroupName) (\s@UpdateNodegroupVersion' {} a -> s {nodegroupName = a} :: UpdateNodegroupVersion)

instance Core.AWSRequest UpdateNodegroupVersion where
  type
    AWSResponse UpdateNodegroupVersion =
      UpdateNodegroupVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNodegroupVersionResponse'
            Core.<$> (x Core..?> "update")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateNodegroupVersion

instance Core.NFData UpdateNodegroupVersion

instance Core.ToHeaders UpdateNodegroupVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateNodegroupVersion where
  toJSON UpdateNodegroupVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("releaseVersion" Core..=) Core.<$> releaseVersion,
            ("force" Core..=) Core.<$> force,
            ("launchTemplate" Core..=) Core.<$> launchTemplate,
            ("version" Core..=) Core.<$> version,
            ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken
          ]
      )

instance Core.ToPath UpdateNodegroupVersion where
  toPath UpdateNodegroupVersion' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/node-groups/",
        Core.toBS nodegroupName,
        "/update-version"
      ]

instance Core.ToQuery UpdateNodegroupVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateNodegroupVersionResponse' smart constructor.
data UpdateNodegroupVersionResponse = UpdateNodegroupVersionResponse'
  { update :: Core.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNodegroupVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'updateNodegroupVersionResponse_update' - Undocumented member.
--
-- 'httpStatus', 'updateNodegroupVersionResponse_httpStatus' - The response's http status code.
newUpdateNodegroupVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateNodegroupVersionResponse
newUpdateNodegroupVersionResponse pHttpStatus_ =
  UpdateNodegroupVersionResponse'
    { update =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateNodegroupVersionResponse_update :: Lens.Lens' UpdateNodegroupVersionResponse (Core.Maybe Update)
updateNodegroupVersionResponse_update = Lens.lens (\UpdateNodegroupVersionResponse' {update} -> update) (\s@UpdateNodegroupVersionResponse' {} a -> s {update = a} :: UpdateNodegroupVersionResponse)

-- | The response's http status code.
updateNodegroupVersionResponse_httpStatus :: Lens.Lens' UpdateNodegroupVersionResponse Core.Int
updateNodegroupVersionResponse_httpStatus = Lens.lens (\UpdateNodegroupVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateNodegroupVersionResponse' {} a -> s {httpStatus = a} :: UpdateNodegroupVersionResponse)

instance Core.NFData UpdateNodegroupVersionResponse
