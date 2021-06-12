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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The type of the configuration to override the risk decision.
--
-- /See:/ 'newRiskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { -- | Risk detection is not performed on the IP addresses in the range list.
    -- The IP range is in CIDR notation.
    skippedIPRangeList :: Core.Maybe [Core.Text],
    -- | Overrides the risk decision to always block the pre-authentication
    -- requests. The IP range is in CIDR notation: a compact representation of
    -- an IP address and its associated routing prefix.
    blockedIPRangeList :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      blockedIPRangeList = Core.Nothing
    }

-- | Risk detection is not performed on the IP addresses in the range list.
-- The IP range is in CIDR notation.
riskExceptionConfigurationType_skippedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Core.Maybe [Core.Text])
riskExceptionConfigurationType_skippedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {skippedIPRangeList} -> skippedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {skippedIPRangeList = a} :: RiskExceptionConfigurationType) Core.. Lens.mapping Lens._Coerce

-- | Overrides the risk decision to always block the pre-authentication
-- requests. The IP range is in CIDR notation: a compact representation of
-- an IP address and its associated routing prefix.
riskExceptionConfigurationType_blockedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Core.Maybe [Core.Text])
riskExceptionConfigurationType_blockedIPRangeList = Lens.lens (\RiskExceptionConfigurationType' {blockedIPRangeList} -> blockedIPRangeList) (\s@RiskExceptionConfigurationType' {} a -> s {blockedIPRangeList = a} :: RiskExceptionConfigurationType) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON RiskExceptionConfigurationType where
  parseJSON =
    Core.withObject
      "RiskExceptionConfigurationType"
      ( \x ->
          RiskExceptionConfigurationType'
            Core.<$> ( x Core..:? "SkippedIPRangeList"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "BlockedIPRangeList"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable RiskExceptionConfigurationType

instance Core.NFData RiskExceptionConfigurationType

instance Core.ToJSON RiskExceptionConfigurationType where
  toJSON RiskExceptionConfigurationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SkippedIPRangeList" Core..=)
              Core.<$> skippedIPRangeList,
            ("BlockedIPRangeList" Core..=)
              Core.<$> blockedIPRangeList
          ]
      )
