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
-- Module      : Network.AWS.IAM.Types.PolicyDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an IAM policy, including the policy document.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newPolicyDetail' smart constructor.
data PolicyDetail = PolicyDetail'
  { -- | The name of the policy.
    policyName :: Core.Maybe Core.Text,
    -- | The policy document.
    policyDocument :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PolicyDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policyDetail_policyName' - The name of the policy.
--
-- 'policyDocument', 'policyDetail_policyDocument' - The policy document.
newPolicyDetail ::
  PolicyDetail
newPolicyDetail =
  PolicyDetail'
    { policyName = Core.Nothing,
      policyDocument = Core.Nothing
    }

-- | The name of the policy.
policyDetail_policyName :: Lens.Lens' PolicyDetail (Core.Maybe Core.Text)
policyDetail_policyName = Lens.lens (\PolicyDetail' {policyName} -> policyName) (\s@PolicyDetail' {} a -> s {policyName = a} :: PolicyDetail)

-- | The policy document.
policyDetail_policyDocument :: Lens.Lens' PolicyDetail (Core.Maybe Core.Text)
policyDetail_policyDocument = Lens.lens (\PolicyDetail' {policyDocument} -> policyDocument) (\s@PolicyDetail' {} a -> s {policyDocument = a} :: PolicyDetail)

instance Core.FromXML PolicyDetail where
  parseXML x =
    PolicyDetail'
      Core.<$> (x Core..@? "PolicyName")
      Core.<*> (x Core..@? "PolicyDocument")

instance Core.Hashable PolicyDetail

instance Core.NFData PolicyDetail
