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
-- Module      : Amazonka.AutoScaling.ExecutePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes the specified policy. This can be useful for testing the design
-- of your scaling policy.
module Amazonka.AutoScaling.ExecutePolicy
  ( -- * Creating a Request
    ExecutePolicy (..),
    newExecutePolicy,

    -- * Request Lenses
    executePolicy_honorCooldown,
    executePolicy_breachThreshold,
    executePolicy_metricValue,
    executePolicy_autoScalingGroupName,
    executePolicy_policyName,

    -- * Destructuring the Response
    ExecutePolicyResponse (..),
    newExecutePolicyResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecutePolicy' smart constructor.
data ExecutePolicy = ExecutePolicy'
  { -- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
    -- to complete before executing the policy.
    --
    -- Valid only if the policy type is @SimpleScaling@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    honorCooldown :: Prelude.Maybe Prelude.Bool,
    -- | The breach threshold for the alarm.
    --
    -- Required if the policy type is @StepScaling@ and not supported
    -- otherwise.
    breachThreshold :: Prelude.Maybe Prelude.Double,
    -- | The metric value to compare to @BreachThreshold@. This enables you to
    -- execute a policy of type @StepScaling@ and determine which step
    -- adjustment to use. For example, if the breach threshold is 50 and you
    -- want to use a step adjustment with a lower bound of 0 and an upper bound
    -- of 10, you can set the metric value to 59.
    --
    -- If you specify a metric value that doesn\'t correspond to a step
    -- adjustment for the policy, the call returns an error.
    --
    -- Required if the policy type is @StepScaling@ and not supported
    -- otherwise.
    metricValue :: Prelude.Maybe Prelude.Double,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the policy.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'honorCooldown', 'executePolicy_honorCooldown' - Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
-- to complete before executing the policy.
--
-- Valid only if the policy type is @SimpleScaling@. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'breachThreshold', 'executePolicy_breachThreshold' - The breach threshold for the alarm.
--
-- Required if the policy type is @StepScaling@ and not supported
-- otherwise.
--
-- 'metricValue', 'executePolicy_metricValue' - The metric value to compare to @BreachThreshold@. This enables you to
-- execute a policy of type @StepScaling@ and determine which step
-- adjustment to use. For example, if the breach threshold is 50 and you
-- want to use a step adjustment with a lower bound of 0 and an upper bound
-- of 10, you can set the metric value to 59.
--
-- If you specify a metric value that doesn\'t correspond to a step
-- adjustment for the policy, the call returns an error.
--
-- Required if the policy type is @StepScaling@ and not supported
-- otherwise.
--
-- 'autoScalingGroupName', 'executePolicy_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'policyName', 'executePolicy_policyName' - The name or ARN of the policy.
newExecutePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  ExecutePolicy
newExecutePolicy pPolicyName_ =
  ExecutePolicy'
    { honorCooldown = Prelude.Nothing,
      breachThreshold = Prelude.Nothing,
      metricValue = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      policyName = pPolicyName_
    }

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period
-- to complete before executing the policy.
--
-- Valid only if the policy type is @SimpleScaling@. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
executePolicy_honorCooldown :: Lens.Lens' ExecutePolicy (Prelude.Maybe Prelude.Bool)
executePolicy_honorCooldown = Lens.lens (\ExecutePolicy' {honorCooldown} -> honorCooldown) (\s@ExecutePolicy' {} a -> s {honorCooldown = a} :: ExecutePolicy)

-- | The breach threshold for the alarm.
--
-- Required if the policy type is @StepScaling@ and not supported
-- otherwise.
executePolicy_breachThreshold :: Lens.Lens' ExecutePolicy (Prelude.Maybe Prelude.Double)
executePolicy_breachThreshold = Lens.lens (\ExecutePolicy' {breachThreshold} -> breachThreshold) (\s@ExecutePolicy' {} a -> s {breachThreshold = a} :: ExecutePolicy)

-- | The metric value to compare to @BreachThreshold@. This enables you to
-- execute a policy of type @StepScaling@ and determine which step
-- adjustment to use. For example, if the breach threshold is 50 and you
-- want to use a step adjustment with a lower bound of 0 and an upper bound
-- of 10, you can set the metric value to 59.
--
-- If you specify a metric value that doesn\'t correspond to a step
-- adjustment for the policy, the call returns an error.
--
-- Required if the policy type is @StepScaling@ and not supported
-- otherwise.
executePolicy_metricValue :: Lens.Lens' ExecutePolicy (Prelude.Maybe Prelude.Double)
executePolicy_metricValue = Lens.lens (\ExecutePolicy' {metricValue} -> metricValue) (\s@ExecutePolicy' {} a -> s {metricValue = a} :: ExecutePolicy)

-- | The name of the Auto Scaling group.
executePolicy_autoScalingGroupName :: Lens.Lens' ExecutePolicy (Prelude.Maybe Prelude.Text)
executePolicy_autoScalingGroupName = Lens.lens (\ExecutePolicy' {autoScalingGroupName} -> autoScalingGroupName) (\s@ExecutePolicy' {} a -> s {autoScalingGroupName = a} :: ExecutePolicy)

-- | The name or ARN of the policy.
executePolicy_policyName :: Lens.Lens' ExecutePolicy Prelude.Text
executePolicy_policyName = Lens.lens (\ExecutePolicy' {policyName} -> policyName) (\s@ExecutePolicy' {} a -> s {policyName = a} :: ExecutePolicy)

instance Core.AWSRequest ExecutePolicy where
  type
    AWSResponse ExecutePolicy =
      ExecutePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ExecutePolicyResponse'

instance Prelude.Hashable ExecutePolicy where
  hashWithSalt _salt ExecutePolicy' {..} =
    _salt `Prelude.hashWithSalt` honorCooldown
      `Prelude.hashWithSalt` breachThreshold
      `Prelude.hashWithSalt` metricValue
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData ExecutePolicy where
  rnf ExecutePolicy' {..} =
    Prelude.rnf honorCooldown
      `Prelude.seq` Prelude.rnf breachThreshold
      `Prelude.seq` Prelude.rnf metricValue
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf policyName

instance Core.ToHeaders ExecutePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ExecutePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery ExecutePolicy where
  toQuery ExecutePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ExecutePolicy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "HonorCooldown" Core.=: honorCooldown,
        "BreachThreshold" Core.=: breachThreshold,
        "MetricValue" Core.=: metricValue,
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "PolicyName" Core.=: policyName
      ]

-- | /See:/ 'newExecutePolicyResponse' smart constructor.
data ExecutePolicyResponse = ExecutePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newExecutePolicyResponse ::
  ExecutePolicyResponse
newExecutePolicyResponse = ExecutePolicyResponse'

instance Prelude.NFData ExecutePolicyResponse where
  rnf _ = ()
