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
-- Module      : Network.AWS.CloudWatchLogs.DeleteResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource policy from this account. This revokes the access of
-- the identities in that policy to put log events to this account.
module Network.AWS.CloudWatchLogs.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_policyName,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The name of the policy to be revoked. This parameter is required.
    policyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'deleteResourcePolicy_policyName' - The name of the policy to be revoked. This parameter is required.
newDeleteResourcePolicy ::
  DeleteResourcePolicy
newDeleteResourcePolicy =
  DeleteResourcePolicy' {policyName = Core.Nothing}

-- | The name of the policy to be revoked. This parameter is required.
deleteResourcePolicy_policyName :: Lens.Lens' DeleteResourcePolicy (Core.Maybe Core.Text)
deleteResourcePolicy_policyName = Lens.lens (\DeleteResourcePolicy' {policyName} -> policyName) (\s@DeleteResourcePolicy' {} a -> s {policyName = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteResourcePolicyResponse'

instance Core.Hashable DeleteResourcePolicy

instance Core.NFData DeleteResourcePolicy

instance Core.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DeleteResourcePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [("policyName" Core..=) Core.<$> policyName]
      )

instance Core.ToPath DeleteResourcePolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteResourcePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourcePolicyResponse ::
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse =
  DeleteResourcePolicyResponse'

instance Core.NFData DeleteResourcePolicyResponse
