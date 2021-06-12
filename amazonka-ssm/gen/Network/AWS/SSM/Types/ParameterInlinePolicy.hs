{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterInlinePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterInlinePolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | One or more policies assigned to a parameter.
--
-- /See:/ 'newParameterInlinePolicy' smart constructor.
data ParameterInlinePolicy = ParameterInlinePolicy'
  { -- | The type of policy. Parameter Store supports the following policy types:
    -- Expiration, ExpirationNotification, and NoChangeNotification.
    policyType :: Core.Maybe Core.Text,
    -- | The JSON text of the policy.
    policyText :: Core.Maybe Core.Text,
    -- | The status of the policy. Policies report the following statuses:
    -- Pending (the policy has not been enforced or applied yet), Finished (the
    -- policy was applied), Failed (the policy was not applied), or InProgress
    -- (the policy is being applied now).
    policyStatus :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterInlinePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyType', 'parameterInlinePolicy_policyType' - The type of policy. Parameter Store supports the following policy types:
-- Expiration, ExpirationNotification, and NoChangeNotification.
--
-- 'policyText', 'parameterInlinePolicy_policyText' - The JSON text of the policy.
--
-- 'policyStatus', 'parameterInlinePolicy_policyStatus' - The status of the policy. Policies report the following statuses:
-- Pending (the policy has not been enforced or applied yet), Finished (the
-- policy was applied), Failed (the policy was not applied), or InProgress
-- (the policy is being applied now).
newParameterInlinePolicy ::
  ParameterInlinePolicy
newParameterInlinePolicy =
  ParameterInlinePolicy'
    { policyType = Core.Nothing,
      policyText = Core.Nothing,
      policyStatus = Core.Nothing
    }

-- | The type of policy. Parameter Store supports the following policy types:
-- Expiration, ExpirationNotification, and NoChangeNotification.
parameterInlinePolicy_policyType :: Lens.Lens' ParameterInlinePolicy (Core.Maybe Core.Text)
parameterInlinePolicy_policyType = Lens.lens (\ParameterInlinePolicy' {policyType} -> policyType) (\s@ParameterInlinePolicy' {} a -> s {policyType = a} :: ParameterInlinePolicy)

-- | The JSON text of the policy.
parameterInlinePolicy_policyText :: Lens.Lens' ParameterInlinePolicy (Core.Maybe Core.Text)
parameterInlinePolicy_policyText = Lens.lens (\ParameterInlinePolicy' {policyText} -> policyText) (\s@ParameterInlinePolicy' {} a -> s {policyText = a} :: ParameterInlinePolicy)

-- | The status of the policy. Policies report the following statuses:
-- Pending (the policy has not been enforced or applied yet), Finished (the
-- policy was applied), Failed (the policy was not applied), or InProgress
-- (the policy is being applied now).
parameterInlinePolicy_policyStatus :: Lens.Lens' ParameterInlinePolicy (Core.Maybe Core.Text)
parameterInlinePolicy_policyStatus = Lens.lens (\ParameterInlinePolicy' {policyStatus} -> policyStatus) (\s@ParameterInlinePolicy' {} a -> s {policyStatus = a} :: ParameterInlinePolicy)

instance Core.FromJSON ParameterInlinePolicy where
  parseJSON =
    Core.withObject
      "ParameterInlinePolicy"
      ( \x ->
          ParameterInlinePolicy'
            Core.<$> (x Core..:? "PolicyType")
            Core.<*> (x Core..:? "PolicyText")
            Core.<*> (x Core..:? "PolicyStatus")
      )

instance Core.Hashable ParameterInlinePolicy

instance Core.NFData ParameterInlinePolicy
