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
-- Module      : Amazonka.Organizations.Types.PolicyTypeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.PolicyTypeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types.PolicyType
import Amazonka.Organizations.Types.PolicyTypeStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a policy type and its status in the
-- associated root.
--
-- /See:/ 'newPolicyTypeSummary' smart constructor.
data PolicyTypeSummary = PolicyTypeSummary'
  { -- | The name of the policy type.
    type' :: Prelude.Maybe PolicyType,
    -- | The status of the policy type as it relates to the associated root. To
    -- attach a policy of the specified type to a root or to an OU or account
    -- in that root, it must be available in the organization and enabled for
    -- that root.
    status :: Prelude.Maybe PolicyTypeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyTypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'policyTypeSummary_type' - The name of the policy type.
--
-- 'status', 'policyTypeSummary_status' - The status of the policy type as it relates to the associated root. To
-- attach a policy of the specified type to a root or to an OU or account
-- in that root, it must be available in the organization and enabled for
-- that root.
newPolicyTypeSummary ::
  PolicyTypeSummary
newPolicyTypeSummary =
  PolicyTypeSummary'
    { type' = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the policy type.
policyTypeSummary_type :: Lens.Lens' PolicyTypeSummary (Prelude.Maybe PolicyType)
policyTypeSummary_type = Lens.lens (\PolicyTypeSummary' {type'} -> type') (\s@PolicyTypeSummary' {} a -> s {type' = a} :: PolicyTypeSummary)

-- | The status of the policy type as it relates to the associated root. To
-- attach a policy of the specified type to a root or to an OU or account
-- in that root, it must be available in the organization and enabled for
-- that root.
policyTypeSummary_status :: Lens.Lens' PolicyTypeSummary (Prelude.Maybe PolicyTypeStatus)
policyTypeSummary_status = Lens.lens (\PolicyTypeSummary' {status} -> status) (\s@PolicyTypeSummary' {} a -> s {status = a} :: PolicyTypeSummary)

instance Core.FromJSON PolicyTypeSummary where
  parseJSON =
    Core.withObject
      "PolicyTypeSummary"
      ( \x ->
          PolicyTypeSummary'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Status")
      )

instance Prelude.Hashable PolicyTypeSummary where
  hashWithSalt _salt PolicyTypeSummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` status

instance Prelude.NFData PolicyTypeSummary where
  rnf PolicyTypeSummary' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf status
