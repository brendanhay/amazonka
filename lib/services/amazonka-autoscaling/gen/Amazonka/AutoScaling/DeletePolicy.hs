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
-- Module      : Amazonka.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling policy.
--
-- Deleting either a step scaling policy or a simple scaling policy deletes
-- the underlying alarm action, but does not delete the alarm, even if it
-- no longer has an associated action.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/deleting-scaling-policy.html Deleting a scaling policy>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_autoScalingGroupName,
    deletePolicy_policyName,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the policy.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'deletePolicy_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'policyName', 'deletePolicy_policyName' - The name or Amazon Resource Name (ARN) of the policy.
newDeletePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  DeletePolicy
newDeletePolicy pPolicyName_ =
  DeletePolicy'
    { autoScalingGroupName =
        Prelude.Nothing,
      policyName = pPolicyName_
    }

-- | The name of the Auto Scaling group.
deletePolicy_autoScalingGroupName :: Lens.Lens' DeletePolicy (Prelude.Maybe Prelude.Text)
deletePolicy_autoScalingGroupName = Lens.lens (\DeletePolicy' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeletePolicy' {} a -> s {autoScalingGroupName = a} :: DeletePolicy)

-- | The name or Amazon Resource Name (ARN) of the policy.
deletePolicy_policyName :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_policyName = Lens.lens (\DeletePolicy' {policyName} -> policyName) (\s@DeletePolicy' {} a -> s {policyName = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeletePolicyResponse'

instance Prelude.Hashable DeletePolicy where
  hashWithSalt _salt DeletePolicy' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeletePolicy where
  rnf DeletePolicy' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf policyName

instance Core.ToHeaders DeletePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeletePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DeletePolicy where
  toQuery DeletePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeletePolicy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "PolicyName" Core.=: policyName
      ]

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyResponse ::
  DeletePolicyResponse
newDeletePolicyResponse = DeletePolicyResponse'

instance Prelude.NFData DeletePolicyResponse where
  rnf _ = ()
