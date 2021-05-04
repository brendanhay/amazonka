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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The type of the configuration to override the risk decision.
--
-- /See:/ 'newRiskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { -- | Risk detection is not performed on the IP addresses in the range list.
    -- The IP range is in CIDR notation.
    skippedIPRangeList :: Prelude.Maybe [Prelude.Text],
    -- | Overrides the risk decision to always block the pre-authentication
    -- requests. The IP range is in CIDR notation: a compact representation of
    -- an IP address and its associated routing prefix.
    blockedIPRangeList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RiskExceptionConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skippedIPRangeList', 'riskExceptionConfigurationType_skippedIPRangeList' - Risk detection is not performed on the IP addresses in the range list.
-- The IP range is in CIDR notation.
--
-- 'blockedIPRangeList', 'riskExceptionConfigurationType_blockedIPRangeList' - Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation: a compact representation of
-- an IP address and its associated routing prefix.
newRiskExceptionConfigurationType ::
  RiskExceptionConfigurationType
newRiskExceptionConfigurationType =
  RiskExceptionConfigurationType'
    { skippedIPRangeList =
        Prelude.Nothing,
      blockedIPRangeList = Prelude.Nothing
    }

-- | Risk detection is not performed on the IP addresses in the range list.
-- The IP range is in CIDR notation.
riskExceptionConfigurationType_skippedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Prelude.Maybe [Prelude.Text])
riskExceptionConfigurationType_skippedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {skippedIPRangeList} -> skippedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {skippedIPRangeList = a} :: RiskExceptionConfigurationType) Prelude.. Lens.mapping Prelude._Coerce

-- | Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation: a compact representation of
-- an IP address and its associated routing prefix.
riskExceptionConfigurationType_blockedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Prelude.Maybe [Prelude.Text])
riskExceptionConfigurationType_blockedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {blockedIPRangeList} -> blockedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {blockedIPRangeList = a} :: RiskExceptionConfigurationType) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    RiskExceptionConfigurationType
  where
  parseJSON =
    Prelude.withObject
      "RiskExceptionConfigurationType"
      ( \x ->
          RiskExceptionConfigurationType'
            Prelude.<$> ( x Prelude..:? "SkippedIPRangeList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "BlockedIPRangeList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RiskExceptionConfigurationType

instance
  Prelude.NFData
    RiskExceptionConfigurationType

instance
  Prelude.ToJSON
    RiskExceptionConfigurationType
  where
  toJSON RiskExceptionConfigurationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SkippedIPRangeList" Prelude..=)
              Prelude.<$> skippedIPRangeList,
            ("BlockedIPRangeList" Prelude..=)
              Prelude.<$> blockedIPRangeList
          ]
      )
