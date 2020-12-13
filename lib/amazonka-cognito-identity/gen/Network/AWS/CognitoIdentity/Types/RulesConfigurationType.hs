{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RulesConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RulesConfigurationType
  ( RulesConfigurationType (..),

    -- * Smart constructor
    mkRulesConfigurationType,

    -- * Lenses
    rctRules,
  )
where

import Network.AWS.CognitoIdentity.Types.MappingRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for rules.
--
-- /See:/ 'mkRulesConfigurationType' smart constructor.
newtype RulesConfigurationType = RulesConfigurationType'
  { -- | An array of rules. You can specify up to 25 rules per identity provider.
    --
    -- Rules are evaluated in order. The first one to match specifies the role.
    rules :: Lude.NonEmpty MappingRule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RulesConfigurationType' with the minimum fields required to make a request.
--
-- * 'rules' - An array of rules. You can specify up to 25 rules per identity provider.
--
-- Rules are evaluated in order. The first one to match specifies the role.
mkRulesConfigurationType ::
  -- | 'rules'
  Lude.NonEmpty MappingRule ->
  RulesConfigurationType
mkRulesConfigurationType pRules_ =
  RulesConfigurationType' {rules = pRules_}

-- | An array of rules. You can specify up to 25 rules per identity provider.
--
-- Rules are evaluated in order. The first one to match specifies the role.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctRules :: Lens.Lens' RulesConfigurationType (Lude.NonEmpty MappingRule)
rctRules = Lens.lens (rules :: RulesConfigurationType -> Lude.NonEmpty MappingRule) (\s a -> s {rules = a} :: RulesConfigurationType)
{-# DEPRECATED rctRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.FromJSON RulesConfigurationType where
  parseJSON =
    Lude.withObject
      "RulesConfigurationType"
      (\x -> RulesConfigurationType' Lude.<$> (x Lude..: "Rules"))

instance Lude.ToJSON RulesConfigurationType where
  toJSON RulesConfigurationType' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Rules" Lude..= rules)])
