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
-- Module      : Network.AWS.IoT.DetachPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from the specified target.
module Network.AWS.IoT.DetachPolicy
  ( -- * Creating a Request
    DetachPolicy (..),
    newDetachPolicy,

    -- * Request Lenses
    detachPolicy_policyName,
    detachPolicy_target,

    -- * Destructuring the Response
    DetachPolicyResponse (..),
    newDetachPolicyResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The policy to detach.
    policyName :: Prelude.Text,
    -- | The target from which the policy will be detached.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'detachPolicy_policyName' - The policy to detach.
--
-- 'target', 'detachPolicy_target' - The target from which the policy will be detached.
newDetachPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  DetachPolicy
newDetachPolicy pPolicyName_ pTarget_ =
  DetachPolicy'
    { policyName = pPolicyName_,
      target = pTarget_
    }

-- | The policy to detach.
detachPolicy_policyName :: Lens.Lens' DetachPolicy Prelude.Text
detachPolicy_policyName = Lens.lens (\DetachPolicy' {policyName} -> policyName) (\s@DetachPolicy' {} a -> s {policyName = a} :: DetachPolicy)

-- | The target from which the policy will be detached.
detachPolicy_target :: Lens.Lens' DetachPolicy Prelude.Text
detachPolicy_target = Lens.lens (\DetachPolicy' {target} -> target) (\s@DetachPolicy' {} a -> s {target = a} :: DetachPolicy)

instance Prelude.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DetachPolicyResponse'

instance Prelude.Hashable DetachPolicy

instance Prelude.NFData DetachPolicy

instance Prelude.ToHeaders DetachPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("target" Prelude..= target)]
      )

instance Prelude.ToPath DetachPolicy where
  toPath DetachPolicy' {..} =
    Prelude.mconcat
      ["/target-policies/", Prelude.toBS policyName]

instance Prelude.ToQuery DetachPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachPolicyResponse ::
  DetachPolicyResponse
newDetachPolicyResponse = DetachPolicyResponse'

instance Prelude.NFData DetachPolicyResponse
