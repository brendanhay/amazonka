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
-- Module      : Amazonka.AMP.Types.RuleGroupsNamespaceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.RuleGroupsNamespaceStatus where

import Amazonka.AMP.Types.RuleGroupsNamespaceStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the status of a namespace.
--
-- /See:/ 'newRuleGroupsNamespaceStatus' smart constructor.
data RuleGroupsNamespaceStatus = RuleGroupsNamespaceStatus'
  { -- | The reason for failure if any.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Status code of this namespace.
    statusCode :: RuleGroupsNamespaceStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupsNamespaceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'ruleGroupsNamespaceStatus_statusReason' - The reason for failure if any.
--
-- 'statusCode', 'ruleGroupsNamespaceStatus_statusCode' - Status code of this namespace.
newRuleGroupsNamespaceStatus ::
  -- | 'statusCode'
  RuleGroupsNamespaceStatusCode ->
  RuleGroupsNamespaceStatus
newRuleGroupsNamespaceStatus pStatusCode_ =
  RuleGroupsNamespaceStatus'
    { statusReason =
        Prelude.Nothing,
      statusCode = pStatusCode_
    }

-- | The reason for failure if any.
ruleGroupsNamespaceStatus_statusReason :: Lens.Lens' RuleGroupsNamespaceStatus (Prelude.Maybe Prelude.Text)
ruleGroupsNamespaceStatus_statusReason = Lens.lens (\RuleGroupsNamespaceStatus' {statusReason} -> statusReason) (\s@RuleGroupsNamespaceStatus' {} a -> s {statusReason = a} :: RuleGroupsNamespaceStatus)

-- | Status code of this namespace.
ruleGroupsNamespaceStatus_statusCode :: Lens.Lens' RuleGroupsNamespaceStatus RuleGroupsNamespaceStatusCode
ruleGroupsNamespaceStatus_statusCode = Lens.lens (\RuleGroupsNamespaceStatus' {statusCode} -> statusCode) (\s@RuleGroupsNamespaceStatus' {} a -> s {statusCode = a} :: RuleGroupsNamespaceStatus)

instance Data.FromJSON RuleGroupsNamespaceStatus where
  parseJSON =
    Data.withObject
      "RuleGroupsNamespaceStatus"
      ( \x ->
          RuleGroupsNamespaceStatus'
            Prelude.<$> (x Data..:? "statusReason")
            Prelude.<*> (x Data..: "statusCode")
      )

instance Prelude.Hashable RuleGroupsNamespaceStatus where
  hashWithSalt _salt RuleGroupsNamespaceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData RuleGroupsNamespaceStatus where
  rnf RuleGroupsNamespaceStatus' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf statusCode
