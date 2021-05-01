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
-- Module      : Network.AWS.WAFRegional.DeletePermissionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Permanently deletes an IAM policy from the specified RuleGroup.
--
-- The user making the request must be the owner of the RuleGroup.
module Network.AWS.WAFRegional.DeletePermissionPolicy
  ( -- * Creating a Request
    DeletePermissionPolicy (..),
    newDeletePermissionPolicy,

    -- * Request Lenses
    deletePermissionPolicy_resourceArn,

    -- * Destructuring the Response
    DeletePermissionPolicyResponse (..),
    newDeletePermissionPolicyResponse,

    -- * Response Lenses
    deletePermissionPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newDeletePermissionPolicy' smart constructor.
data DeletePermissionPolicy = DeletePermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup from which you want to
    -- delete the policy.
    --
    -- The user making the request must be the owner of the RuleGroup.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deletePermissionPolicy_resourceArn' - The Amazon Resource Name (ARN) of the RuleGroup from which you want to
-- delete the policy.
--
-- The user making the request must be the owner of the RuleGroup.
newDeletePermissionPolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeletePermissionPolicy
newDeletePermissionPolicy pResourceArn_ =
  DeletePermissionPolicy'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the RuleGroup from which you want to
-- delete the policy.
--
-- The user making the request must be the owner of the RuleGroup.
deletePermissionPolicy_resourceArn :: Lens.Lens' DeletePermissionPolicy Prelude.Text
deletePermissionPolicy_resourceArn = Lens.lens (\DeletePermissionPolicy' {resourceArn} -> resourceArn) (\s@DeletePermissionPolicy' {} a -> s {resourceArn = a} :: DeletePermissionPolicy)

instance Prelude.AWSRequest DeletePermissionPolicy where
  type
    Rs DeletePermissionPolicy =
      DeletePermissionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePermissionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePermissionPolicy

instance Prelude.NFData DeletePermissionPolicy

instance Prelude.ToHeaders DeletePermissionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_Regional_20161128.DeletePermissionPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeletePermissionPolicy where
  toJSON DeletePermissionPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceArn" Prelude..= resourceArn)
          ]
      )

instance Prelude.ToPath DeletePermissionPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePermissionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePermissionPolicyResponse' smart constructor.
data DeletePermissionPolicyResponse = DeletePermissionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePermissionPolicyResponse_httpStatus' - The response's http status code.
newDeletePermissionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionPolicyResponse
newDeletePermissionPolicyResponse pHttpStatus_ =
  DeletePermissionPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePermissionPolicyResponse_httpStatus :: Lens.Lens' DeletePermissionPolicyResponse Prelude.Int
deletePermissionPolicyResponse_httpStatus = Lens.lens (\DeletePermissionPolicyResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionPolicyResponse' {} a -> s {httpStatus = a} :: DeletePermissionPolicyResponse)

instance
  Prelude.NFData
    DeletePermissionPolicyResponse
