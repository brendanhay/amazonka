{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
  ( RiskExceptionConfigurationType (..),

    -- * Smart constructor
    mkRiskExceptionConfigurationType,

    -- * Lenses
    rectSkippedIPRangeList,
    rectBlockedIPRangeList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type of the configuration to override the risk decision.
--
-- /See:/ 'mkRiskExceptionConfigurationType' smart constructor.
data RiskExceptionConfigurationType = RiskExceptionConfigurationType'
  { skippedIPRangeList ::
      Lude.Maybe [Lude.Text],
    blockedIPRangeList ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RiskExceptionConfigurationType' with the minimum fields required to make a request.
--
-- * 'blockedIPRangeList' - Overrides the risk decision to always block the pre-authentication requests. The IP range is in CIDR notation: a compact representation of an IP address and its associated routing prefix.
-- * 'skippedIPRangeList' - Risk detection is not performed on the IP addresses in the range list. The IP range is in CIDR notation.
mkRiskExceptionConfigurationType ::
  RiskExceptionConfigurationType
mkRiskExceptionConfigurationType =
  RiskExceptionConfigurationType'
    { skippedIPRangeList =
        Lude.Nothing,
      blockedIPRangeList = Lude.Nothing
    }

-- | Risk detection is not performed on the IP addresses in the range list. The IP range is in CIDR notation.
--
-- /Note:/ Consider using 'skippedIPRangeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rectSkippedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Lude.Maybe [Lude.Text])
rectSkippedIPRangeList = Lens.lens (skippedIPRangeList :: RiskExceptionConfigurationType -> Lude.Maybe [Lude.Text]) (\s a -> s {skippedIPRangeList = a} :: RiskExceptionConfigurationType)
{-# DEPRECATED rectSkippedIPRangeList "Use generic-lens or generic-optics with 'skippedIPRangeList' instead." #-}

-- | Overrides the risk decision to always block the pre-authentication requests. The IP range is in CIDR notation: a compact representation of an IP address and its associated routing prefix.
--
-- /Note:/ Consider using 'blockedIPRangeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rectBlockedIPRangeList :: Lens.Lens' RiskExceptionConfigurationType (Lude.Maybe [Lude.Text])
rectBlockedIPRangeList = Lens.lens (blockedIPRangeList :: RiskExceptionConfigurationType -> Lude.Maybe [Lude.Text]) (\s a -> s {blockedIPRangeList = a} :: RiskExceptionConfigurationType)
{-# DEPRECATED rectBlockedIPRangeList "Use generic-lens or generic-optics with 'blockedIPRangeList' instead." #-}

instance Lude.FromJSON RiskExceptionConfigurationType where
  parseJSON =
    Lude.withObject
      "RiskExceptionConfigurationType"
      ( \x ->
          RiskExceptionConfigurationType'
            Lude.<$> (x Lude..:? "SkippedIPRangeList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "BlockedIPRangeList" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON RiskExceptionConfigurationType where
  toJSON RiskExceptionConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkippedIPRangeList" Lude..=) Lude.<$> skippedIPRangeList,
            ("BlockedIPRangeList" Lude..=) Lude.<$> blockedIPRangeList
          ]
      )
