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
-- Module      : Amazonka.Organizations.EnableAllFeatures
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables all features in an organization. This enables the use of
-- organization policies that can restrict the services and actions that
-- can be called in each account. Until you enable all features, you have
-- access only to consolidated billing, and you can\'t use any of the
-- advanced account administration features that Organizations supports.
-- For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /Organizations User Guide./
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
module Amazonka.Organizations.EnableAllFeatures
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableAllFeatures' smart constructor.
data EnableAllFeatures = EnableAllFeatures'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableAllFeaturesResponse'
            Prelude.<$> (x Data..?> "Handshake")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableAllFeatures where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData EnableAllFeatures where
  rnf _ = ()

instance Data.ToHeaders EnableAllFeatures where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.EnableAllFeatures" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableAllFeatures where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath EnableAllFeatures where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableAllFeatures where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableAllFeaturesResponse' smart constructor.
data EnableAllFeaturesResponse = EnableAllFeaturesResponse'
  { -- | A structure that contains details about the handshake created to support
    -- this request to enable all features in the organization.
    handshake :: Prelude.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  EnableAllFeaturesResponse
newEnableAllFeaturesResponse pHttpStatus_ =
  EnableAllFeaturesResponse'
    { handshake =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the handshake created to support
-- this request to enable all features in the organization.
enableAllFeaturesResponse_handshake :: Lens.Lens' EnableAllFeaturesResponse (Prelude.Maybe Handshake)
enableAllFeaturesResponse_handshake = Lens.lens (\EnableAllFeaturesResponse' {handshake} -> handshake) (\s@EnableAllFeaturesResponse' {} a -> s {handshake = a} :: EnableAllFeaturesResponse)

-- | The response's http status code.
enableAllFeaturesResponse_httpStatus :: Lens.Lens' EnableAllFeaturesResponse Prelude.Int
enableAllFeaturesResponse_httpStatus = Lens.lens (\EnableAllFeaturesResponse' {httpStatus} -> httpStatus) (\s@EnableAllFeaturesResponse' {} a -> s {httpStatus = a} :: EnableAllFeaturesResponse)

instance Prelude.NFData EnableAllFeaturesResponse where
  rnf EnableAllFeaturesResponse' {..} =
    Prelude.rnf handshake
      `Prelude.seq` Prelude.rnf httpStatus
