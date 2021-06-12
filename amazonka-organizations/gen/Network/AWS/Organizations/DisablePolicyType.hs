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
-- Module      : Network.AWS.Organizations.DisablePolicyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an organizational policy type in a root. A policy of a certain
-- type can be attached to entities in a root only if that type is enabled
-- in the root. After you perform this operation, you no longer can attach
-- policies of the specified type to that root or to any organizational
-- unit (OU) or account in that root. You can undo this by using the
-- EnablePolicyType operation.
--
-- This is an asynchronous request that AWS performs in the background. If
-- you disable a policy type for a root, it still appears enabled for the
-- organization if
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features>
-- are enabled for the organization. AWS recommends that you first use
-- ListRoots to see the status of policy types for a specified root, and
-- then use this operation.
--
-- This operation can be called only from the organization\'s management
-- account.
--
-- To view the status of available policy types in the organization, use
-- DescribeOrganization.
module Network.AWS.Organizations.DisablePolicyType
  ( -- * Creating a Request
    DisablePolicyType (..),
    newDisablePolicyType,

    -- * Request Lenses
    disablePolicyType_rootId,
    disablePolicyType_policyType,

    -- * Destructuring the Response
    DisablePolicyTypeResponse (..),
    newDisablePolicyTypeResponse,

    -- * Response Lenses
    disablePolicyTypeResponse_root,
    disablePolicyTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisablePolicyType' smart constructor.
data DisablePolicyType = DisablePolicyType'
  { -- | The unique identifier (ID) of the root in which you want to disable a
    -- policy type. You can get the ID from the ListRoots operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string
    -- requires \"r-\" followed by from 4 to 32 lowercase letters or digits.
    rootId :: Core.Text,
    -- | The policy type that you want to disable in this root. You can specify
    -- one of the following values:
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    policyType :: PolicyType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisablePolicyType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootId', 'disablePolicyType_rootId' - The unique identifier (ID) of the root in which you want to disable a
-- policy type. You can get the ID from the ListRoots operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string
-- requires \"r-\" followed by from 4 to 32 lowercase letters or digits.
--
-- 'policyType', 'disablePolicyType_policyType' - The policy type that you want to disable in this root. You can specify
-- one of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
newDisablePolicyType ::
  -- | 'rootId'
  Core.Text ->
  -- | 'policyType'
  PolicyType ->
  DisablePolicyType
newDisablePolicyType pRootId_ pPolicyType_ =
  DisablePolicyType'
    { rootId = pRootId_,
      policyType = pPolicyType_
    }

-- | The unique identifier (ID) of the root in which you want to disable a
-- policy type. You can get the ID from the ListRoots operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string
-- requires \"r-\" followed by from 4 to 32 lowercase letters or digits.
disablePolicyType_rootId :: Lens.Lens' DisablePolicyType Core.Text
disablePolicyType_rootId = Lens.lens (\DisablePolicyType' {rootId} -> rootId) (\s@DisablePolicyType' {} a -> s {rootId = a} :: DisablePolicyType)

-- | The policy type that you want to disable in this root. You can specify
-- one of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
disablePolicyType_policyType :: Lens.Lens' DisablePolicyType PolicyType
disablePolicyType_policyType = Lens.lens (\DisablePolicyType' {policyType} -> policyType) (\s@DisablePolicyType' {} a -> s {policyType = a} :: DisablePolicyType)

instance Core.AWSRequest DisablePolicyType where
  type
    AWSResponse DisablePolicyType =
      DisablePolicyTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisablePolicyTypeResponse'
            Core.<$> (x Core..?> "Root")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisablePolicyType

instance Core.NFData DisablePolicyType

instance Core.ToHeaders DisablePolicyType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DisablePolicyType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisablePolicyType where
  toJSON DisablePolicyType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RootId" Core..= rootId),
            Core.Just ("PolicyType" Core..= policyType)
          ]
      )

instance Core.ToPath DisablePolicyType where
  toPath = Core.const "/"

instance Core.ToQuery DisablePolicyType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisablePolicyTypeResponse' smart constructor.
data DisablePolicyTypeResponse = DisablePolicyTypeResponse'
  { -- | A structure that shows the root with the updated list of enabled policy
    -- types.
    root :: Core.Maybe Root,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisablePolicyTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'root', 'disablePolicyTypeResponse_root' - A structure that shows the root with the updated list of enabled policy
-- types.
--
-- 'httpStatus', 'disablePolicyTypeResponse_httpStatus' - The response's http status code.
newDisablePolicyTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisablePolicyTypeResponse
newDisablePolicyTypeResponse pHttpStatus_ =
  DisablePolicyTypeResponse'
    { root = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that shows the root with the updated list of enabled policy
-- types.
disablePolicyTypeResponse_root :: Lens.Lens' DisablePolicyTypeResponse (Core.Maybe Root)
disablePolicyTypeResponse_root = Lens.lens (\DisablePolicyTypeResponse' {root} -> root) (\s@DisablePolicyTypeResponse' {} a -> s {root = a} :: DisablePolicyTypeResponse)

-- | The response's http status code.
disablePolicyTypeResponse_httpStatus :: Lens.Lens' DisablePolicyTypeResponse Core.Int
disablePolicyTypeResponse_httpStatus = Lens.lens (\DisablePolicyTypeResponse' {httpStatus} -> httpStatus) (\s@DisablePolicyTypeResponse' {} a -> s {httpStatus = a} :: DisablePolicyTypeResponse)

instance Core.NFData DisablePolicyTypeResponse
