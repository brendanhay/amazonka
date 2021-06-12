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
-- Module      : Network.AWS.IoT.AttachPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to the specified target.
module Network.AWS.IoT.AttachPolicy
  ( -- * Creating a Request
    AttachPolicy (..),
    newAttachPolicy,

    -- * Request Lenses
    attachPolicy_policyName,
    attachPolicy_target,

    -- * Destructuring the Response
    AttachPolicyResponse (..),
    newAttachPolicyResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The name of the policy to attach.
    policyName :: Core.Text,
    -- | The
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity>
    -- to which the policy is attached.
    target :: Core.Text
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
-- 'policyName', 'attachPolicy_policyName' - The name of the policy to attach.
--
-- 'target', 'attachPolicy_target' - The
-- <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity>
-- to which the policy is attached.
newAttachPolicy ::
  -- | 'policyName'
  Core.Text ->
  -- | 'target'
  Core.Text ->
  AttachPolicy
newAttachPolicy pPolicyName_ pTarget_ =
  AttachPolicy'
    { policyName = pPolicyName_,
      target = pTarget_
    }

-- | The name of the policy to attach.
attachPolicy_policyName :: Lens.Lens' AttachPolicy Core.Text
attachPolicy_policyName = Lens.lens (\AttachPolicy' {policyName} -> policyName) (\s@AttachPolicy' {} a -> s {policyName = a} :: AttachPolicy)

-- | The
-- <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity>
-- to which the policy is attached.
attachPolicy_target :: Lens.Lens' AttachPolicy Core.Text
attachPolicy_target = Lens.lens (\AttachPolicy' {target} -> target) (\s@AttachPolicy' {} a -> s {target = a} :: AttachPolicy)

instance Core.AWSRequest AttachPolicy where
  type AWSResponse AttachPolicy = AttachPolicyResponse
  request = Request.putJSON defaultService
  response = Response.receiveNull AttachPolicyResponse'

instance Core.Hashable AttachPolicy

instance Core.NFData AttachPolicy

instance Core.ToHeaders AttachPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("target" Core..= target)]
      )

instance Core.ToPath AttachPolicy where
  toPath AttachPolicy' {..} =
    Core.mconcat
      ["/target-policies/", Core.toBS policyName]

instance Core.ToQuery AttachPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachPolicyResponse ::
  AttachPolicyResponse
newAttachPolicyResponse = AttachPolicyResponse'

instance Core.NFData AttachPolicyResponse
