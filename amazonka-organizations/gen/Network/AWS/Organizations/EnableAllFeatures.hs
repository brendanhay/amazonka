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
-- Module      : Network.AWS.Organizations.EnableAllFeatures
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables all features in an organization. This enables the use of
-- organization policies that can restrict the services and actions that
-- can be called in each account. Until you enable all features, you have
-- access only to consolidated billing, and you can\'t use any of the
-- advanced account administration features that AWS Organizations
-- supports. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /AWS Organizations User Guide./
--
-- This operation is required only for organizations that were created
-- explicitly with only the consolidated billing features enabled. Calling
-- this operation sends a handshake to every invited account in the
-- organization. The feature set change can be finalized and the additional
-- features enabled only after all administrators in the invited accounts
-- approve the change by accepting the handshake.
--
-- After you enable all features, you can separately enable or disable
-- individual policy types in a root using EnablePolicyType and
-- DisablePolicyType. To see the status of policy types in a root, use
-- ListRoots.
--
-- After all invited member accounts accept the handshake, you finalize the
-- feature set change by accepting the handshake that contains
-- @\"Action\": \"ENABLE_ALL_FEATURES\"@. This completes the change.
--
-- After you enable all features in your organization, the management
-- account in the organization can apply policies on all member accounts.
-- These policies can restrict what users and even administrators in those
-- accounts can do. The management account can apply policies that prevent
-- accounts from leaving the organization. Ensure that your account
-- administrators are aware of this.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.EnableAllFeatures
  ( -- * Creating a Request
    EnableAllFeatures (..),
    newEnableAllFeatures,

    -- * Destructuring the Response
    EnableAllFeaturesResponse (..),
    newEnableAllFeaturesResponse,

    -- * Response Lenses
    enableAllFeaturesResponse_handshake,
    enableAllFeaturesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableAllFeatures' smart constructor.
data EnableAllFeatures = EnableAllFeatures'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAllFeatures' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAllFeatures ::
  EnableAllFeatures
newEnableAllFeatures = EnableAllFeatures'

instance Core.AWSRequest EnableAllFeatures where
  type
    AWSResponse EnableAllFeatures =
      EnableAllFeaturesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableAllFeaturesResponse'
            Core.<$> (x Core..?> "Handshake")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableAllFeatures

instance Core.NFData EnableAllFeatures

instance Core.ToHeaders EnableAllFeatures where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.EnableAllFeatures" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableAllFeatures where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath EnableAllFeatures where
  toPath = Core.const "/"

instance Core.ToQuery EnableAllFeatures where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableAllFeaturesResponse' smart constructor.
data EnableAllFeaturesResponse = EnableAllFeaturesResponse'
  { -- | A structure that contains details about the handshake created to support
    -- this request to enable all features in the organization.
    handshake :: Core.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAllFeaturesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshake', 'enableAllFeaturesResponse_handshake' - A structure that contains details about the handshake created to support
-- this request to enable all features in the organization.
--
-- 'httpStatus', 'enableAllFeaturesResponse_httpStatus' - The response's http status code.
newEnableAllFeaturesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableAllFeaturesResponse
newEnableAllFeaturesResponse pHttpStatus_ =
  EnableAllFeaturesResponse'
    { handshake =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the handshake created to support
-- this request to enable all features in the organization.
enableAllFeaturesResponse_handshake :: Lens.Lens' EnableAllFeaturesResponse (Core.Maybe Handshake)
enableAllFeaturesResponse_handshake = Lens.lens (\EnableAllFeaturesResponse' {handshake} -> handshake) (\s@EnableAllFeaturesResponse' {} a -> s {handshake = a} :: EnableAllFeaturesResponse)

-- | The response's http status code.
enableAllFeaturesResponse_httpStatus :: Lens.Lens' EnableAllFeaturesResponse Core.Int
enableAllFeaturesResponse_httpStatus = Lens.lens (\EnableAllFeaturesResponse' {httpStatus} -> httpStatus) (\s@EnableAllFeaturesResponse' {} a -> s {httpStatus = a} :: EnableAllFeaturesResponse)

instance Core.NFData EnableAllFeaturesResponse
