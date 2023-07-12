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
-- Module      : Amazonka.Organizations.Types.EffectivePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.EffectivePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.EffectivePolicyType
import qualified Amazonka.Prelude as Prelude

-- | Contains rules to be applied to the affected accounts. The effective
-- policy is the aggregation of any policies the account inherits, plus any
-- policy directly attached to the account.
--
-- /See:/ 'newEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { -- | The time of the last update to this policy.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The text content of the policy.
    policyContent :: Prelude.Maybe Prelude.Text,
    -- | The policy type.
    policyType :: Prelude.Maybe EffectivePolicyType,
    -- | The account ID of the policy target.
    targetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EffectivePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'effectivePolicy_lastUpdatedTimestamp' - The time of the last update to this policy.
--
-- 'policyContent', 'effectivePolicy_policyContent' - The text content of the policy.
--
-- 'policyType', 'effectivePolicy_policyType' - The policy type.
--
-- 'targetId', 'effectivePolicy_targetId' - The account ID of the policy target.
newEffectivePolicy ::
  EffectivePolicy
newEffectivePolicy =
  EffectivePolicy'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      policyContent = Prelude.Nothing,
      policyType = Prelude.Nothing,
      targetId = Prelude.Nothing
    }

-- | The time of the last update to this policy.
effectivePolicy_lastUpdatedTimestamp :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.UTCTime)
effectivePolicy_lastUpdatedTimestamp = Lens.lens (\EffectivePolicy' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@EffectivePolicy' {} a -> s {lastUpdatedTimestamp = a} :: EffectivePolicy) Prelude.. Lens.mapping Data._Time

-- | The text content of the policy.
effectivePolicy_policyContent :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_policyContent = Lens.lens (\EffectivePolicy' {policyContent} -> policyContent) (\s@EffectivePolicy' {} a -> s {policyContent = a} :: EffectivePolicy)

-- | The policy type.
effectivePolicy_policyType :: Lens.Lens' EffectivePolicy (Prelude.Maybe EffectivePolicyType)
effectivePolicy_policyType = Lens.lens (\EffectivePolicy' {policyType} -> policyType) (\s@EffectivePolicy' {} a -> s {policyType = a} :: EffectivePolicy)

-- | The account ID of the policy target.
effectivePolicy_targetId :: Lens.Lens' EffectivePolicy (Prelude.Maybe Prelude.Text)
effectivePolicy_targetId = Lens.lens (\EffectivePolicy' {targetId} -> targetId) (\s@EffectivePolicy' {} a -> s {targetId = a} :: EffectivePolicy)

instance Data.FromJSON EffectivePolicy where
  parseJSON =
    Data.withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            Prelude.<$> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "PolicyContent")
            Prelude.<*> (x Data..:? "PolicyType")
            Prelude.<*> (x Data..:? "TargetId")
      )

instance Prelude.Hashable EffectivePolicy where
  hashWithSalt _salt EffectivePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` policyContent
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` targetId

instance Prelude.NFData EffectivePolicy where
  rnf EffectivePolicy' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf policyContent
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf targetId
