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
-- Module      : Network.AWS.Glue.DeleteResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified policy.
module Network.AWS.Glue.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicy_policyHashCondition,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,

    -- * Response Lenses
    deleteResourcePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The ARN of the AWS Glue resource for the resource policy to be deleted.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The hash value returned when this policy was set.
    policyHashCondition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteResourcePolicy_resourceArn' - The ARN of the AWS Glue resource for the resource policy to be deleted.
--
-- 'policyHashCondition', 'deleteResourcePolicy_policyHashCondition' - The hash value returned when this policy was set.
newDeleteResourcePolicy ::
  DeleteResourcePolicy
newDeleteResourcePolicy =
  DeleteResourcePolicy'
    { resourceArn =
        Prelude.Nothing,
      policyHashCondition = Prelude.Nothing
    }

-- | The ARN of the AWS Glue resource for the resource policy to be deleted.
deleteResourcePolicy_resourceArn :: Lens.Lens' DeleteResourcePolicy (Prelude.Maybe Prelude.Text)
deleteResourcePolicy_resourceArn = Lens.lens (\DeleteResourcePolicy' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicy' {} a -> s {resourceArn = a} :: DeleteResourcePolicy)

-- | The hash value returned when this policy was set.
deleteResourcePolicy_policyHashCondition :: Lens.Lens' DeleteResourcePolicy (Prelude.Maybe Prelude.Text)
deleteResourcePolicy_policyHashCondition = Lens.lens (\DeleteResourcePolicy' {policyHashCondition} -> policyHashCondition) (\s@DeleteResourcePolicy' {} a -> s {policyHashCondition = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcePolicy

instance Prelude.NFData DeleteResourcePolicy

instance Core.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.DeleteResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceArn" Core..=) Prelude.<$> resourceArn,
            ("PolicyHashCondition" Core..=)
              Prelude.<$> policyHashCondition
          ]
      )

instance Core.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourcePolicyResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse pHttpStatus_ =
  DeleteResourcePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourcePolicyResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyResponse Prelude.Int
deleteResourcePolicyResponse_httpStatus = Lens.lens (\DeleteResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyResponse)

instance Prelude.NFData DeleteResourcePolicyResponse
