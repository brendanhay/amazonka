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
-- Module      : Network.AWS.CloudDirectory.DetachPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from an object.
module Network.AWS.CloudDirectory.DetachPolicy
  ( -- * Creating a Request
    DetachPolicy (..),
    newDetachPolicy,

    -- * Request Lenses
    detachPolicy_directoryArn,
    detachPolicy_policyReference,
    detachPolicy_objectReference,

    -- * Destructuring the Response
    DetachPolicyResponse (..),
    newDetachPolicyResponse,

    -- * Response Lenses
    detachPolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where both objects reside. For more information, see arns.
    directoryArn :: Core.Text,
    -- | Reference that identifies the policy object.
    policyReference :: ObjectReference,
    -- | Reference that identifies the object whose policy object will be
    -- detached.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'detachPolicy_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where both objects reside. For more information, see arns.
--
-- 'policyReference', 'detachPolicy_policyReference' - Reference that identifies the policy object.
--
-- 'objectReference', 'detachPolicy_objectReference' - Reference that identifies the object whose policy object will be
-- detached.
newDetachPolicy ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  DetachPolicy
newDetachPolicy
  pDirectoryArn_
  pPolicyReference_
  pObjectReference_ =
    DetachPolicy'
      { directoryArn = pDirectoryArn_,
        policyReference = pPolicyReference_,
        objectReference = pObjectReference_
      }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where both objects reside. For more information, see arns.
detachPolicy_directoryArn :: Lens.Lens' DetachPolicy Core.Text
detachPolicy_directoryArn = Lens.lens (\DetachPolicy' {directoryArn} -> directoryArn) (\s@DetachPolicy' {} a -> s {directoryArn = a} :: DetachPolicy)

-- | Reference that identifies the policy object.
detachPolicy_policyReference :: Lens.Lens' DetachPolicy ObjectReference
detachPolicy_policyReference = Lens.lens (\DetachPolicy' {policyReference} -> policyReference) (\s@DetachPolicy' {} a -> s {policyReference = a} :: DetachPolicy)

-- | Reference that identifies the object whose policy object will be
-- detached.
detachPolicy_objectReference :: Lens.Lens' DetachPolicy ObjectReference
detachPolicy_objectReference = Lens.lens (\DetachPolicy' {objectReference} -> objectReference) (\s@DetachPolicy' {} a -> s {objectReference = a} :: DetachPolicy)

instance Core.AWSRequest DetachPolicy where
  type AWSResponse DetachPolicy = DetachPolicyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachPolicy

instance Core.NFData DetachPolicy

instance Core.ToHeaders DetachPolicy where
  toHeaders DetachPolicy' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("PolicyReference" Core..= policyReference),
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath DetachPolicy where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/policy/detach"

instance Core.ToQuery DetachPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachPolicyResponse_httpStatus' - The response's http status code.
newDetachPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachPolicyResponse
newDetachPolicyResponse pHttpStatus_ =
  DetachPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
detachPolicyResponse_httpStatus :: Lens.Lens' DetachPolicyResponse Core.Int
detachPolicyResponse_httpStatus = Lens.lens (\DetachPolicyResponse' {httpStatus} -> httpStatus) (\s@DetachPolicyResponse' {} a -> s {httpStatus = a} :: DetachPolicyResponse)

instance Core.NFData DetachPolicyResponse
