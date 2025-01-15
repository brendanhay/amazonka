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
-- Module      : Amazonka.IoT.DetachPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from the specified target.
--
-- Because of the distributed nature of Amazon Web Services, it can take up
-- to five minutes after a policy is detached before it\'s ready to be
-- deleted.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DetachPolicy>
-- action.
module Amazonka.IoT.DetachPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The policy to detach.
    policyName :: Prelude.Text,
    -- | The target from which the policy will be detached.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DetachPolicy where
  type AWSResponse DetachPolicy = DetachPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DetachPolicyResponse'

instance Prelude.Hashable DetachPolicy where
  hashWithSalt _salt DetachPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` target

instance Prelude.NFData DetachPolicy where
  rnf DetachPolicy' {..} =
    Prelude.rnf policyName `Prelude.seq`
      Prelude.rnf target

instance Data.ToHeaders DetachPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("target" Data..= target)]
      )

instance Data.ToPath DetachPolicy where
  toPath DetachPolicy' {..} =
    Prelude.mconcat
      ["/target-policies/", Data.toBS policyName]

instance Data.ToQuery DetachPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachPolicyResponse ::
  DetachPolicyResponse
newDetachPolicyResponse = DetachPolicyResponse'

instance Prelude.NFData DetachPolicyResponse where
  rnf _ = ()
