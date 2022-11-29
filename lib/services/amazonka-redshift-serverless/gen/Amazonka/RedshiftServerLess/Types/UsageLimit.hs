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
-- Module      : Amazonka.RedshiftServerLess.Types.UsageLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.UsageLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.UsageLimitBreachAction
import Amazonka.RedshiftServerLess.Types.UsageLimitPeriod
import Amazonka.RedshiftServerLess.Types.UsageLimitUsageType

-- | The usage limit object.
--
-- /See:/ 'newUsageLimit' smart constructor.
data UsageLimit = UsageLimit'
  { -- | The identifier of the usage limit.
    usageLimitId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource associated with the usage
    -- limit.
    usageLimitArn :: Prelude.Maybe Prelude.Text,
    -- | The time period that the amount applies to. A weekly period begins on
    -- Sunday. The default is monthly.
    period :: Prelude.Maybe UsageLimitPeriod,
    -- | The Amazon Redshift Serverless feature to limit.
    usageType :: Prelude.Maybe UsageLimitUsageType,
    -- | The action that Amazon Redshift Serverless takes when the limit is
    -- reached.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The Amazon Resource Name (ARN) that identifies the Amazon Redshift
    -- Serverless resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The limit amount. If time-based, this amount is in RPUs consumed per
    -- hour. If data-based, this amount is in terabytes (TB). The value must be
    -- a positive number.
    amount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimitId', 'usageLimit_usageLimitId' - The identifier of the usage limit.
--
-- 'usageLimitArn', 'usageLimit_usageLimitArn' - The Amazon Resource Name (ARN) of the resource associated with the usage
-- limit.
--
-- 'period', 'usageLimit_period' - The time period that the amount applies to. A weekly period begins on
-- Sunday. The default is monthly.
--
-- 'usageType', 'usageLimit_usageType' - The Amazon Redshift Serverless feature to limit.
--
-- 'breachAction', 'usageLimit_breachAction' - The action that Amazon Redshift Serverless takes when the limit is
-- reached.
--
-- 'resourceArn', 'usageLimit_resourceArn' - The Amazon Resource Name (ARN) that identifies the Amazon Redshift
-- Serverless resource.
--
-- 'amount', 'usageLimit_amount' - The limit amount. If time-based, this amount is in RPUs consumed per
-- hour. If data-based, this amount is in terabytes (TB). The value must be
-- a positive number.
newUsageLimit ::
  UsageLimit
newUsageLimit =
  UsageLimit'
    { usageLimitId = Prelude.Nothing,
      usageLimitArn = Prelude.Nothing,
      period = Prelude.Nothing,
      usageType = Prelude.Nothing,
      breachAction = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      amount = Prelude.Nothing
    }

-- | The identifier of the usage limit.
usageLimit_usageLimitId :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_usageLimitId = Lens.lens (\UsageLimit' {usageLimitId} -> usageLimitId) (\s@UsageLimit' {} a -> s {usageLimitId = a} :: UsageLimit)

-- | The Amazon Resource Name (ARN) of the resource associated with the usage
-- limit.
usageLimit_usageLimitArn :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_usageLimitArn = Lens.lens (\UsageLimit' {usageLimitArn} -> usageLimitArn) (\s@UsageLimit' {} a -> s {usageLimitArn = a} :: UsageLimit)

-- | The time period that the amount applies to. A weekly period begins on
-- Sunday. The default is monthly.
usageLimit_period :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitPeriod)
usageLimit_period = Lens.lens (\UsageLimit' {period} -> period) (\s@UsageLimit' {} a -> s {period = a} :: UsageLimit)

-- | The Amazon Redshift Serverless feature to limit.
usageLimit_usageType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitUsageType)
usageLimit_usageType = Lens.lens (\UsageLimit' {usageType} -> usageType) (\s@UsageLimit' {} a -> s {usageType = a} :: UsageLimit)

-- | The action that Amazon Redshift Serverless takes when the limit is
-- reached.
usageLimit_breachAction :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitBreachAction)
usageLimit_breachAction = Lens.lens (\UsageLimit' {breachAction} -> breachAction) (\s@UsageLimit' {} a -> s {breachAction = a} :: UsageLimit)

-- | The Amazon Resource Name (ARN) that identifies the Amazon Redshift
-- Serverless resource.
usageLimit_resourceArn :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_resourceArn = Lens.lens (\UsageLimit' {resourceArn} -> resourceArn) (\s@UsageLimit' {} a -> s {resourceArn = a} :: UsageLimit)

-- | The limit amount. If time-based, this amount is in RPUs consumed per
-- hour. If data-based, this amount is in terabytes (TB). The value must be
-- a positive number.
usageLimit_amount :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Integer)
usageLimit_amount = Lens.lens (\UsageLimit' {amount} -> amount) (\s@UsageLimit' {} a -> s {amount = a} :: UsageLimit)

instance Core.FromJSON UsageLimit where
  parseJSON =
    Core.withObject
      "UsageLimit"
      ( \x ->
          UsageLimit'
            Prelude.<$> (x Core..:? "usageLimitId")
            Prelude.<*> (x Core..:? "usageLimitArn")
            Prelude.<*> (x Core..:? "period")
            Prelude.<*> (x Core..:? "usageType")
            Prelude.<*> (x Core..:? "breachAction")
            Prelude.<*> (x Core..:? "resourceArn")
            Prelude.<*> (x Core..:? "amount")
      )

instance Prelude.Hashable UsageLimit where
  hashWithSalt _salt UsageLimit' {..} =
    _salt `Prelude.hashWithSalt` usageLimitId
      `Prelude.hashWithSalt` usageLimitArn
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` usageType
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` amount

instance Prelude.NFData UsageLimit where
  rnf UsageLimit' {..} =
    Prelude.rnf usageLimitId
      `Prelude.seq` Prelude.rnf usageLimitArn
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf usageType
      `Prelude.seq` Prelude.rnf breachAction
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf amount
