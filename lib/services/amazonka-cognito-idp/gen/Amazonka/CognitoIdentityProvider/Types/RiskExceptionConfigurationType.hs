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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of the configuration to override the risk decision.
--
-- /See:/ 'newRiskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { -- | Overrides the risk decision to always block the pre-authentication
    -- requests. The IP range is in CIDR notation, a compact representation of
    -- an IP address and its routing prefix.
    blockedIPRangeList :: Prelude.Maybe [Prelude.Text],
    -- | Risk detection isn\'t performed on the IP addresses in this range list.
    -- The IP range is in CIDR notation.
    skippedIPRangeList :: Prelude.Maybe [Prelude.Text]
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
-- 'blockedIPRangeList', 'riskExceptionConfigurationType_blockedIPRangeList' - Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation, a compact representation of
-- an IP address and its routing prefix.
--
-- 'skippedIPRangeList', 'riskExceptionConfigurationType_skippedIPRangeList' - Risk detection isn\'t performed on the IP addresses in this range list.
-- The IP range is in CIDR notation.
newRiskExceptionConfigurationType ::
  RiskExceptionConfigurationType
newRiskExceptionConfigurationType =
  RiskExceptionConfigurationType'
    { blockedIPRangeList =
        Prelude.Nothing,
      skippedIPRangeList = Prelude.Nothing
    }

-- | Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation, a compact representation of
-- an IP address and its routing prefix.
riskExceptionConfigurationType_blockedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Prelude.Maybe [Prelude.Text])
riskExceptionConfigurationType_blockedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {blockedIPRangeList} -> blockedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {blockedIPRangeList = a} :: RiskExceptionConfigurationType) Prelude.. Lens.mapping Lens.coerced

-- | Risk detection isn\'t performed on the IP addresses in this range list.
-- The IP range is in CIDR notation.
riskExceptionConfigurationType_skippedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Prelude.Maybe [Prelude.Text])
riskExceptionConfigurationType_skippedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {skippedIPRangeList} -> skippedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {skippedIPRangeList = a} :: RiskExceptionConfigurationType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RiskExceptionConfigurationType where
  parseJSON =
    Data.withObject
      "RiskExceptionConfigurationType"
      ( \x ->
          RiskExceptionConfigurationType'
            Prelude.<$> ( x Data..:? "BlockedIPRangeList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "SkippedIPRangeList"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RiskExceptionConfigurationType
  where
  hashWithSalt
    _salt
    RiskExceptionConfigurationType' {..} =
      _salt `Prelude.hashWithSalt` blockedIPRangeList
        `Prelude.hashWithSalt` skippedIPRangeList

instance
  Prelude.NFData
    RiskExceptionConfigurationType
  where
  rnf RiskExceptionConfigurationType' {..} =
    Prelude.rnf blockedIPRangeList
      `Prelude.seq` Prelude.rnf skippedIPRangeList

instance Data.ToJSON RiskExceptionConfigurationType where
  toJSON RiskExceptionConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlockedIPRangeList" Data..=)
              Prelude.<$> blockedIPRangeList,
            ("SkippedIPRangeList" Data..=)
              Prelude.<$> skippedIPRangeList
          ]
      )
