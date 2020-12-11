-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.IPRuleItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.IPRuleItem
  ( IPRuleItem (..),

    -- * Smart constructor
    mkIPRuleItem,

    -- * Lenses
    iriRuleDesc,
    iriIpRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a rule for an IP access control group.
--
-- /See:/ 'mkIPRuleItem' smart constructor.
data IPRuleItem = IPRuleItem'
  { ruleDesc :: Lude.Maybe Lude.Text,
    ipRule :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPRuleItem' with the minimum fields required to make a request.
--
-- * 'ipRule' - The IP address range, in CIDR notation.
-- * 'ruleDesc' - The description.
mkIPRuleItem ::
  IPRuleItem
mkIPRuleItem =
  IPRuleItem' {ruleDesc = Lude.Nothing, ipRule = Lude.Nothing}

-- | The description.
--
-- /Note:/ Consider using 'ruleDesc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriRuleDesc :: Lens.Lens' IPRuleItem (Lude.Maybe Lude.Text)
iriRuleDesc = Lens.lens (ruleDesc :: IPRuleItem -> Lude.Maybe Lude.Text) (\s a -> s {ruleDesc = a} :: IPRuleItem)
{-# DEPRECATED iriRuleDesc "Use generic-lens or generic-optics with 'ruleDesc' instead." #-}

-- | The IP address range, in CIDR notation.
--
-- /Note:/ Consider using 'ipRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriIpRule :: Lens.Lens' IPRuleItem (Lude.Maybe Lude.Text)
iriIpRule = Lens.lens (ipRule :: IPRuleItem -> Lude.Maybe Lude.Text) (\s a -> s {ipRule = a} :: IPRuleItem)
{-# DEPRECATED iriIpRule "Use generic-lens or generic-optics with 'ipRule' instead." #-}

instance Lude.FromJSON IPRuleItem where
  parseJSON =
    Lude.withObject
      "IPRuleItem"
      ( \x ->
          IPRuleItem'
            Lude.<$> (x Lude..:? "ruleDesc") Lude.<*> (x Lude..:? "ipRule")
      )

instance Lude.ToJSON IPRuleItem where
  toJSON IPRuleItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ruleDesc" Lude..=) Lude.<$> ruleDesc,
            ("ipRule" Lude..=) Lude.<$> ipRule
          ]
      )
