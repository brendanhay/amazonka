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
-- Module      : Amazonka.IoT.AttachPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified policy to the specified principal (certificate or
-- other credential).
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AttachPolicy>
-- action.
module Amazonka.IoT.AttachPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The name of the policy to attach.
    policyName :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity>
    -- to which the policy is attached. For example, a thing group or a
    -- certificate.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- to which the policy is attached. For example, a thing group or a
-- certificate.
newAttachPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  AttachPolicy
newAttachPolicy pPolicyName_ pTarget_ =
  AttachPolicy'
    { policyName = pPolicyName_,
      target = pTarget_
    }

-- | The name of the policy to attach.
attachPolicy_policyName :: Lens.Lens' AttachPolicy Prelude.Text
attachPolicy_policyName = Lens.lens (\AttachPolicy' {policyName} -> policyName) (\s@AttachPolicy' {} a -> s {policyName = a} :: AttachPolicy)

-- | The
-- <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity>
-- to which the policy is attached. For example, a thing group or a
-- certificate.
attachPolicy_target :: Lens.Lens' AttachPolicy Prelude.Text
attachPolicy_target = Lens.lens (\AttachPolicy' {target} -> target) (\s@AttachPolicy' {} a -> s {target = a} :: AttachPolicy)

instance Core.AWSRequest AttachPolicy where
  type AWSResponse AttachPolicy = AttachPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response = Response.receiveNull AttachPolicyResponse'

instance Prelude.Hashable AttachPolicy where
  hashWithSalt _salt AttachPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` target

instance Prelude.NFData AttachPolicy where
  rnf AttachPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf target

instance Core.ToHeaders AttachPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("target" Core..= target)]
      )

instance Core.ToPath AttachPolicy where
  toPath AttachPolicy' {..} =
    Prelude.mconcat
      ["/target-policies/", Core.toBS policyName]

instance Core.ToQuery AttachPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachPolicyResponse ::
  AttachPolicyResponse
newAttachPolicyResponse = AttachPolicyResponse'

instance Prelude.NFData AttachPolicyResponse where
  rnf _ = ()
