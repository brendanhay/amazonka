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
-- Module      : Network.AWS.Organizations.Types.PolicyTypeSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeSummary where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Organizations.Types.PolicyTypeStatus
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a policy type and its status in the
-- associated root.
--
-- /See:/ 'newPolicyTypeSummary' smart constructor.
data PolicyTypeSummary = PolicyTypeSummary'
  { -- | The status of the policy type as it relates to the associated root. To
    -- attach a policy of the specified type to a root or to an OU or account
    -- in that root, it must be available in the organization and enabled for
    -- that root.
    status :: Prelude.Maybe PolicyTypeStatus,
    -- | The name of the policy type.
    type' :: Prelude.Maybe PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyTypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'policyTypeSummary_status' - The status of the policy type as it relates to the associated root. To
-- attach a policy of the specified type to a root or to an OU or account
-- in that root, it must be available in the organization and enabled for
-- that root.
--
-- 'type'', 'policyTypeSummary_type' - The name of the policy type.
newPolicyTypeSummary ::
  PolicyTypeSummary
newPolicyTypeSummary =
  PolicyTypeSummary'
    { status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The status of the policy type as it relates to the associated root. To
-- attach a policy of the specified type to a root or to an OU or account
-- in that root, it must be available in the organization and enabled for
-- that root.
policyTypeSummary_status :: Lens.Lens' PolicyTypeSummary (Prelude.Maybe PolicyTypeStatus)
policyTypeSummary_status = Lens.lens (\PolicyTypeSummary' {status} -> status) (\s@PolicyTypeSummary' {} a -> s {status = a} :: PolicyTypeSummary)

-- | The name of the policy type.
policyTypeSummary_type :: Lens.Lens' PolicyTypeSummary (Prelude.Maybe PolicyType)
policyTypeSummary_type = Lens.lens (\PolicyTypeSummary' {type'} -> type') (\s@PolicyTypeSummary' {} a -> s {type' = a} :: PolicyTypeSummary)

instance Prelude.FromJSON PolicyTypeSummary where
  parseJSON =
    Prelude.withObject
      "PolicyTypeSummary"
      ( \x ->
          PolicyTypeSummary'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable PolicyTypeSummary

instance Prelude.NFData PolicyTypeSummary
