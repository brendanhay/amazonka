{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Organizations.Types.EffectivePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EffectivePolicy where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.EffectivePolicyType
import qualified Network.AWS.Prelude as Prelude

-- | Contains rules to be applied to the affected accounts. The effective
-- policy is the aggregation of any policies the account inherits, plus any
-- policy directly attached to the account.
--
-- /See:/ 'newEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { -- | The account ID of the policy target.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The text content of the policy.
    policyContent :: Prelude.Maybe Prelude.Text,
    -- | The policy type.
    policyType :: Prelude.Maybe EffectivePolicyType,
    -- | The time of the last update to this policy.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EffectivePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'effectivePolicy_targetId' - The account ID of the policy target.
--
-- 'policyContent', 'effectivePolicy_policyContent' - The text content of the policy.
--
-- 'policyType', 'effectivePolicy_policyType' - The policy type.
--
-- 'lastUpdatedTimestamp', 'effectivePolicy_lastUpdatedTimestamp' - The time of the last update to this policy.
newEffectivePolicy ::
  EffectivePolicy
newEffectivePolicy =
  EffectivePolicy'
    { targetId = Prelude.Nothing,
      policyContent = Prelude.Nothing,
      policyType = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing
    }

-- | The account ID of the policy target.
effectivePolicy_targetId :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_targetId = Lens.lens (\EffectivePolicy' {targetId} -> targetId) (\s@EffectivePolicy' {} a -> s {targetId = a} :: EffectivePolicy)

-- | The text content of the policy.
effectivePolicy_policyContent :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_policyContent = Lens.lens (\EffectivePolicy' {policyContent} -> policyContent) (\s@EffectivePolicy' {} a -> s {policyContent = a} :: EffectivePolicy)

-- | The policy type.
effectivePolicy_policyType :: Lens.Lens' EffectivePolicy (Prelude.Maybe EffectivePolicyType)
effectivePolicy_policyType = Lens.lens (\EffectivePolicy' {policyType} -> policyType) (\s@EffectivePolicy' {} a -> s {policyType = a} :: EffectivePolicy)

-- | The time of the last update to this policy.
effectivePolicy_lastUpdatedTimestamp :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.UTCTime)
effectivePolicy_lastUpdatedTimestamp = Lens.lens (\EffectivePolicy' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@EffectivePolicy' {} a -> s {lastUpdatedTimestamp = a} :: EffectivePolicy) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON EffectivePolicy where
  parseJSON =
    Prelude.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Prelude.<$> (x Prelude..:? "TargetId")
            Prelude.<*> (x Prelude..:? "PolicyContent")
            Prelude.<*> (x Prelude..:? "PolicyType")
            Prelude.<*> (x Prelude..:? "LastUpdatedTimestamp")
      )

instance Prelude.Hashable EffectivePolicy

instance Prelude.NFData EffectivePolicy
