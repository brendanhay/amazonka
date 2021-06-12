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
-- Module      : Network.AWS.EKS.CreateFargateProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Fargate profile for your Amazon EKS cluster. You must
-- have at least one Fargate profile in a cluster to be able to run pods on
-- Fargate.
--
-- The Fargate profile allows an administrator to declare which pods run on
-- Fargate and specify which pods run on which Fargate profile. This
-- declaration is done through the profile’s selectors. Each profile can
-- have up to five selectors that contain a namespace and labels. A
-- namespace is required for every selector. The label field consists of
-- multiple optional key-value pairs. Pods that match the selectors are
-- scheduled on Fargate. If a to-be-scheduled pod matches any of the
-- selectors in the Fargate profile, then that pod is run on Fargate.
--
-- When you create a Fargate profile, you must specify a pod execution role
-- to use with the pods that are scheduled with the profile. This role is
-- added to the cluster\'s Kubernetes
-- <https://kubernetes.io/docs/admin/authorization/rbac/ Role Based Access Control>
-- (RBAC) for authorization so that the @kubelet@ that is running on the
-- Fargate infrastructure can register with your Amazon EKS cluster so that
-- it can appear in your cluster as a node. The pod execution role also
-- provides IAM permissions to the Fargate infrastructure to allow read
-- access to Amazon ECR image repositories. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
--
-- Fargate profiles are immutable. However, you can create a new updated
-- profile to replace an existing profile and then delete the original
-- after the updated profile has finished creating.
--
-- If any Fargate profiles in a cluster are in the @DELETING@ status, you
-- must wait for that Fargate profile to finish deleting before you can
-- create any other profiles in that cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/fargate-profile.html AWS Fargate Profile>
-- in the /Amazon EKS User Guide/.
module Network.AWS.EKS.CreateFargateProfile
  ( -- * Creating a Request
    CreateFargateProfile (..),
    newCreateFargateProfile,

    -- * Request Lenses
    createFargateProfile_tags,
    createFargateProfile_selectors,
    createFargateProfile_clientRequestToken,
    createFargateProfile_subnets,
    createFargateProfile_fargateProfileName,
    createFargateProfile_clusterName,
    createFargateProfile_podExecutionRoleArn,

    -- * Destructuring the Response
    CreateFargateProfileResponse (..),
    newCreateFargateProfileResponse,

    -- * Response Lenses
    createFargateProfileResponse_fargateProfile,
    createFargateProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFargateProfile' smart constructor.
data CreateFargateProfile = CreateFargateProfile'
  { -- | The metadata to apply to the Fargate profile to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Fargate profile tags do not
    -- propagate to any other resources associated with the Fargate profile,
    -- such as the pods that are scheduled with it.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The selectors to match for pods to use this Fargate profile. Each
    -- selector must have an associated namespace. Optionally, you can also
    -- specify labels for a namespace. You may specify up to five selectors in
    -- a Fargate profile.
    selectors :: Core.Maybe [FargateProfileSelector],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The IDs of subnets to launch your pods into. At this time, pods running
    -- on Fargate are not assigned public IP addresses, so only private subnets
    -- (with no direct route to an Internet Gateway) are accepted for this
    -- parameter.
    subnets :: Core.Maybe [Core.Text],
    -- | The name of the Fargate profile.
    fargateProfileName :: Core.Text,
    -- | The name of the Amazon EKS cluster to apply the Fargate profile to.
    clusterName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
    -- that match the selectors in the Fargate profile. The pod execution role
    -- allows Fargate infrastructure to register with your cluster as a node,
    -- and it provides read access to Amazon ECR image repositories. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
    -- in the /Amazon EKS User Guide/.
    podExecutionRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFargateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFargateProfile_tags' - The metadata to apply to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Fargate profile tags do not
-- propagate to any other resources associated with the Fargate profile,
-- such as the pods that are scheduled with it.
--
-- 'selectors', 'createFargateProfile_selectors' - The selectors to match for pods to use this Fargate profile. Each
-- selector must have an associated namespace. Optionally, you can also
-- specify labels for a namespace. You may specify up to five selectors in
-- a Fargate profile.
--
-- 'clientRequestToken', 'createFargateProfile_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'subnets', 'createFargateProfile_subnets' - The IDs of subnets to launch your pods into. At this time, pods running
-- on Fargate are not assigned public IP addresses, so only private subnets
-- (with no direct route to an Internet Gateway) are accepted for this
-- parameter.
--
-- 'fargateProfileName', 'createFargateProfile_fargateProfileName' - The name of the Fargate profile.
--
-- 'clusterName', 'createFargateProfile_clusterName' - The name of the Amazon EKS cluster to apply the Fargate profile to.
--
-- 'podExecutionRoleArn', 'createFargateProfile_podExecutionRoleArn' - The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. The pod execution role
-- allows Fargate infrastructure to register with your cluster as a node,
-- and it provides read access to Amazon ECR image repositories. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
newCreateFargateProfile ::
  -- | 'fargateProfileName'
  Core.Text ->
  -- | 'clusterName'
  Core.Text ->
  -- | 'podExecutionRoleArn'
  Core.Text ->
  CreateFargateProfile
newCreateFargateProfile
  pFargateProfileName_
  pClusterName_
  pPodExecutionRoleArn_ =
    CreateFargateProfile'
      { tags = Core.Nothing,
        selectors = Core.Nothing,
        clientRequestToken = Core.Nothing,
        subnets = Core.Nothing,
        fargateProfileName = pFargateProfileName_,
        clusterName = pClusterName_,
        podExecutionRoleArn = pPodExecutionRoleArn_
      }

-- | The metadata to apply to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Fargate profile tags do not
-- propagate to any other resources associated with the Fargate profile,
-- such as the pods that are scheduled with it.
createFargateProfile_tags :: Lens.Lens' CreateFargateProfile (Core.Maybe (Core.HashMap Core.Text Core.Text))
createFargateProfile_tags = Lens.lens (\CreateFargateProfile' {tags} -> tags) (\s@CreateFargateProfile' {} a -> s {tags = a} :: CreateFargateProfile) Core.. Lens.mapping Lens._Coerce

-- | The selectors to match for pods to use this Fargate profile. Each
-- selector must have an associated namespace. Optionally, you can also
-- specify labels for a namespace. You may specify up to five selectors in
-- a Fargate profile.
createFargateProfile_selectors :: Lens.Lens' CreateFargateProfile (Core.Maybe [FargateProfileSelector])
createFargateProfile_selectors = Lens.lens (\CreateFargateProfile' {selectors} -> selectors) (\s@CreateFargateProfile' {} a -> s {selectors = a} :: CreateFargateProfile) Core.. Lens.mapping Lens._Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createFargateProfile_clientRequestToken :: Lens.Lens' CreateFargateProfile (Core.Maybe Core.Text)
createFargateProfile_clientRequestToken = Lens.lens (\CreateFargateProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateFargateProfile' {} a -> s {clientRequestToken = a} :: CreateFargateProfile)

-- | The IDs of subnets to launch your pods into. At this time, pods running
-- on Fargate are not assigned public IP addresses, so only private subnets
-- (with no direct route to an Internet Gateway) are accepted for this
-- parameter.
createFargateProfile_subnets :: Lens.Lens' CreateFargateProfile (Core.Maybe [Core.Text])
createFargateProfile_subnets = Lens.lens (\CreateFargateProfile' {subnets} -> subnets) (\s@CreateFargateProfile' {} a -> s {subnets = a} :: CreateFargateProfile) Core.. Lens.mapping Lens._Coerce

-- | The name of the Fargate profile.
createFargateProfile_fargateProfileName :: Lens.Lens' CreateFargateProfile Core.Text
createFargateProfile_fargateProfileName = Lens.lens (\CreateFargateProfile' {fargateProfileName} -> fargateProfileName) (\s@CreateFargateProfile' {} a -> s {fargateProfileName = a} :: CreateFargateProfile)

-- | The name of the Amazon EKS cluster to apply the Fargate profile to.
createFargateProfile_clusterName :: Lens.Lens' CreateFargateProfile Core.Text
createFargateProfile_clusterName = Lens.lens (\CreateFargateProfile' {clusterName} -> clusterName) (\s@CreateFargateProfile' {} a -> s {clusterName = a} :: CreateFargateProfile)

-- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. The pod execution role
-- allows Fargate infrastructure to register with your cluster as a node,
-- and it provides read access to Amazon ECR image repositories. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
createFargateProfile_podExecutionRoleArn :: Lens.Lens' CreateFargateProfile Core.Text
createFargateProfile_podExecutionRoleArn = Lens.lens (\CreateFargateProfile' {podExecutionRoleArn} -> podExecutionRoleArn) (\s@CreateFargateProfile' {} a -> s {podExecutionRoleArn = a} :: CreateFargateProfile)

instance Core.AWSRequest CreateFargateProfile where
  type
    AWSResponse CreateFargateProfile =
      CreateFargateProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFargateProfileResponse'
            Core.<$> (x Core..?> "fargateProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateFargateProfile

instance Core.NFData CreateFargateProfile

instance Core.ToHeaders CreateFargateProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateFargateProfile where
  toJSON CreateFargateProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("selectors" Core..=) Core.<$> selectors,
            ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("subnets" Core..=) Core.<$> subnets,
            Core.Just
              ("fargateProfileName" Core..= fargateProfileName),
            Core.Just
              ("podExecutionRoleArn" Core..= podExecutionRoleArn)
          ]
      )

instance Core.ToPath CreateFargateProfile where
  toPath CreateFargateProfile' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/fargate-profiles"
      ]

instance Core.ToQuery CreateFargateProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateFargateProfileResponse' smart constructor.
data CreateFargateProfileResponse = CreateFargateProfileResponse'
  { -- | The full description of your new Fargate profile.
    fargateProfile :: Core.Maybe FargateProfile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFargateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fargateProfile', 'createFargateProfileResponse_fargateProfile' - The full description of your new Fargate profile.
--
-- 'httpStatus', 'createFargateProfileResponse_httpStatus' - The response's http status code.
newCreateFargateProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateFargateProfileResponse
newCreateFargateProfileResponse pHttpStatus_ =
  CreateFargateProfileResponse'
    { fargateProfile =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new Fargate profile.
createFargateProfileResponse_fargateProfile :: Lens.Lens' CreateFargateProfileResponse (Core.Maybe FargateProfile)
createFargateProfileResponse_fargateProfile = Lens.lens (\CreateFargateProfileResponse' {fargateProfile} -> fargateProfile) (\s@CreateFargateProfileResponse' {} a -> s {fargateProfile = a} :: CreateFargateProfileResponse)

-- | The response's http status code.
createFargateProfileResponse_httpStatus :: Lens.Lens' CreateFargateProfileResponse Core.Int
createFargateProfileResponse_httpStatus = Lens.lens (\CreateFargateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateFargateProfileResponse' {} a -> s {httpStatus = a} :: CreateFargateProfileResponse)

instance Core.NFData CreateFargateProfileResponse
