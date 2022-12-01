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
-- Module      : Amazonka.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.RiskExceptionConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The type of the configuration to override the risk decision.
--
-- /See:/ 'newRiskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { -- | Risk detection isn\'t performed on the IP addresses in this range list.
    -- The IP range is in CIDR notation.
    skippedIPRangeList :: Prelude.Maybe [Prelude.Text],
    -- | Overrides the risk decision to always block the pre-authentication
    -- requests. The IP range is in CIDR notation, a compact representation of
    -- an IP address and its routing prefix.
    blockedIPRangeList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RiskExceptionConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skippedIPRangeList', 'riskExceptionConfigurationType_skippedIPRangeList' - Risk detection isn\'t performed on the IP addresses in this range list.
-- The IP range is in CIDR notation.
--
-- 'blockedIPRangeList', 'riskExceptionConfigurationType_blockedIPRangeList' - Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation, a compact representation of
-- an IP address and its routing prefix.
newRiskExceptionConfigurationType ::
  RiskExceptionConfigurationType
newRiskExceptionConfigurationType =
  RiskExceptionConfigurationType'
    { skippedIPRangeList =
        Prelude.Nothing,
      blockedIPRangeList = Prelude.Nothing
    }

-- | Risk detection isn\'t performed on the IP addresses in this range list.
-- The IP range is in CIDR notation.
riskExceptionConfigurationType_skippedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Prelude.Maybe [Prelude.Text])
riskExceptionConfigurationType_skippedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {skippedIPRangeList} -> skippedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {skippedIPRangeList = a} :: RiskExceptionConfigurationType) Prelude.. Lens.mapping Lens.coerced

-- | Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation, a compact representation of
-- an IP address and its routing prefix.
riskExceptionConfigurationType_blockedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Prelude.Maybe [Prelude.Text])
riskExceptionConfigurationType_blockedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {blockedIPRangeList} -> blockedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {blockedIPRangeList = a} :: RiskExceptionConfigurationType) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON RiskExceptionConfigurationType where
  parseJSON =
    Core.withObject
      "RiskExceptionConfigurationType"
      ( \x ->
          RiskExceptionConfigurationType'
            Prelude.<$> ( x Core..:? "SkippedIPRangeList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "BlockedIPRangeList"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RiskExceptionConfigurationType
  where
  hashWithSalt
    _salt
    RiskExceptionConfigurationType' {..} =
      _salt `Prelude.hashWithSalt` skippedIPRangeList
        `Prelude.hashWithSalt` blockedIPRangeList

instance
  Prelude.NFData
    RiskExceptionConfigurationType
  where
  rnf RiskExceptionConfigurationType' {..} =
    Prelude.rnf skippedIPRangeList
      `Prelude.seq` Prelude.rnf blockedIPRangeList

instance Core.ToJSON RiskExceptionConfigurationType where
  toJSON RiskExceptionConfigurationType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SkippedIPRangeList" Core..=)
              Prelude.<$> skippedIPRangeList,
            ("BlockedIPRangeList" Core..=)
              Prelude.<$> blockedIPRangeList
          ]
      )
