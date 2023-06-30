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
-- Module      : Amazonka.FMS.Types.DnsDuplicateRuleGroupViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.DnsDuplicateRuleGroupViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A DNS Firewall rule group that Firewall Manager tried to associate with
-- a VPC is already associated with the VPC and can\'t be associated again.
--
-- /See:/ 'newDnsDuplicateRuleGroupViolation' smart constructor.
data DnsDuplicateRuleGroupViolation = DnsDuplicateRuleGroupViolation'
  { -- | Information about the VPC ID.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | A description of the violation that specifies the rule group and VPC.
    violationTargetDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsDuplicateRuleGroupViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationTarget', 'dnsDuplicateRuleGroupViolation_violationTarget' - Information about the VPC ID.
--
-- 'violationTargetDescription', 'dnsDuplicateRuleGroupViolation_violationTargetDescription' - A description of the violation that specifies the rule group and VPC.
newDnsDuplicateRuleGroupViolation ::
  DnsDuplicateRuleGroupViolation
newDnsDuplicateRuleGroupViolation =
  DnsDuplicateRuleGroupViolation'
    { violationTarget =
        Prelude.Nothing,
      violationTargetDescription =
        Prelude.Nothing
    }

-- | Information about the VPC ID.
dnsDuplicateRuleGroupViolation_violationTarget :: Lens.Lens' DnsDuplicateRuleGroupViolation (Prelude.Maybe Prelude.Text)
dnsDuplicateRuleGroupViolation_violationTarget = Lens.lens (\DnsDuplicateRuleGroupViolation' {violationTarget} -> violationTarget) (\s@DnsDuplicateRuleGroupViolation' {} a -> s {violationTarget = a} :: DnsDuplicateRuleGroupViolation)

-- | A description of the violation that specifies the rule group and VPC.
dnsDuplicateRuleGroupViolation_violationTargetDescription :: Lens.Lens' DnsDuplicateRuleGroupViolation (Prelude.Maybe Prelude.Text)
dnsDuplicateRuleGroupViolation_violationTargetDescription = Lens.lens (\DnsDuplicateRuleGroupViolation' {violationTargetDescription} -> violationTargetDescription) (\s@DnsDuplicateRuleGroupViolation' {} a -> s {violationTargetDescription = a} :: DnsDuplicateRuleGroupViolation)

instance Data.FromJSON DnsDuplicateRuleGroupViolation where
  parseJSON =
    Data.withObject
      "DnsDuplicateRuleGroupViolation"
      ( \x ->
          DnsDuplicateRuleGroupViolation'
            Prelude.<$> (x Data..:? "ViolationTarget")
            Prelude.<*> (x Data..:? "ViolationTargetDescription")
      )

instance
  Prelude.Hashable
    DnsDuplicateRuleGroupViolation
  where
  hashWithSalt
    _salt
    DnsDuplicateRuleGroupViolation' {..} =
      _salt
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` violationTargetDescription

instance
  Prelude.NFData
    DnsDuplicateRuleGroupViolation
  where
  rnf DnsDuplicateRuleGroupViolation' {..} =
    Prelude.rnf violationTarget
      `Prelude.seq` Prelude.rnf violationTargetDescription
