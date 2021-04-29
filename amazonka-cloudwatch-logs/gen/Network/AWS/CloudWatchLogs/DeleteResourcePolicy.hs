{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The name of the policy to be revoked. This parameter is required.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  DeleteResourcePolicy' {policyName = Prelude.Nothing}

-- | The name of the policy to be revoked. This parameter is required.
deleteResourcePolicy_policyName :: Lens.Lens' DeleteResourcePolicy (Prelude.Maybe Prelude.Text)
deleteResourcePolicy_policyName = Lens.lens (\DeleteResourcePolicy' {policyName} -> policyName) (\s@DeleteResourcePolicy' {} a -> s {policyName = a} :: DeleteResourcePolicy)

instance Prelude.AWSRequest DeleteResourcePolicy where
  type
    Rs DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteResourcePolicyResponse'

instance Prelude.Hashable DeleteResourcePolicy

instance Prelude.NFData DeleteResourcePolicy

instance Prelude.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.DeleteResourcePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("policyName" Prelude..=) Prelude.<$> policyName]
      )

instance Prelude.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourcePolicyResponse ::
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse =
  DeleteResourcePolicyResponse'

instance Prelude.NFData DeleteResourcePolicyResponse
