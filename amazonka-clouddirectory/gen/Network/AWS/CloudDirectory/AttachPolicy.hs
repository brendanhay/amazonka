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
-- Module      : Network.AWS.CloudDirectory.AttachPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
module Network.AWS.CloudDirectory.AttachPolicy
  ( -- * Creating a Request
    AttachPolicy (..),
    newAttachPolicy,

    -- * Request Lenses
    attachPolicy_directoryArn,
    attachPolicy_policyReference,
    attachPolicy_objectReference,

    -- * Destructuring the Response
    AttachPolicyResponse (..),
    newAttachPolicyResponse,

    -- * Response Lenses
    attachPolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where both objects reside. For more information, see arns.
    directoryArn :: Core.Text,
    -- | The reference that is associated with the policy object.
    policyReference :: ObjectReference,
    -- | The reference that identifies the object to which the policy will be
    -- attached.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'attachPolicy_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where both objects reside. For more information, see arns.
--
-- 'policyReference', 'attachPolicy_policyReference' - The reference that is associated with the policy object.
--
-- 'objectReference', 'attachPolicy_objectReference' - The reference that identifies the object to which the policy will be
-- attached.
newAttachPolicy ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  AttachPolicy
newAttachPolicy
  pDirectoryArn_
  pPolicyReference_
  pObjectReference_ =
    AttachPolicy'
      { directoryArn = pDirectoryArn_,
        policyReference = pPolicyReference_,
        objectReference = pObjectReference_
      }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where both objects reside. For more information, see arns.
attachPolicy_directoryArn :: Lens.Lens' AttachPolicy Core.Text
attachPolicy_directoryArn = Lens.lens (\AttachPolicy' {directoryArn} -> directoryArn) (\s@AttachPolicy' {} a -> s {directoryArn = a} :: AttachPolicy)

-- | The reference that is associated with the policy object.
attachPolicy_policyReference :: Lens.Lens' AttachPolicy ObjectReference
attachPolicy_policyReference = Lens.lens (\AttachPolicy' {policyReference} -> policyReference) (\s@AttachPolicy' {} a -> s {policyReference = a} :: AttachPolicy)

-- | The reference that identifies the object to which the policy will be
-- attached.
attachPolicy_objectReference :: Lens.Lens' AttachPolicy ObjectReference
attachPolicy_objectReference = Lens.lens (\AttachPolicy' {objectReference} -> objectReference) (\s@AttachPolicy' {} a -> s {objectReference = a} :: AttachPolicy)

instance Core.AWSRequest AttachPolicy where
  type AWSResponse AttachPolicy = AttachPolicyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachPolicy

instance Core.NFData AttachPolicy

instance Core.ToHeaders AttachPolicy where
  toHeaders AttachPolicy' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("PolicyReference" Core..= policyReference),
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath AttachPolicy where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/policy/attach"

instance Core.ToQuery AttachPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachPolicyResponse_httpStatus' - The response's http status code.
newAttachPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AttachPolicyResponse
newAttachPolicyResponse pHttpStatus_ =
  AttachPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
attachPolicyResponse_httpStatus :: Lens.Lens' AttachPolicyResponse Core.Int
attachPolicyResponse_httpStatus = Lens.lens (\AttachPolicyResponse' {httpStatus} -> httpStatus) (\s@AttachPolicyResponse' {} a -> s {httpStatus = a} :: AttachPolicyResponse)

instance Core.NFData AttachPolicyResponse
