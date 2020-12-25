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

import qualified Network.AWS.CognitoIdentity.Types.MappingRule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for rules.
--
-- /See:/ 'mkRulesConfigurationType' smart constructor.
newtype RulesConfigurationType = RulesConfigurationType'
  { -- | An array of rules. You can specify up to 25 rules per identity provider.
    --
    -- Rules are evaluated in order. The first one to match specifies the role.
    rules :: Core.NonEmpty Types.MappingRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RulesConfigurationType' value with any optional fields omitted.
mkRulesConfigurationType ::
  -- | 'rules'
  Core.NonEmpty Types.MappingRule ->
  RulesConfigurationType
mkRulesConfigurationType rules = RulesConfigurationType' {rules}

-- | An array of rules. You can specify up to 25 rules per identity provider.
--
-- Rules are evaluated in order. The first one to match specifies the role.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctRules :: Lens.Lens' RulesConfigurationType (Core.NonEmpty Types.MappingRule)
rctRules = Lens.field @"rules"
{-# DEPRECATED rctRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Core.FromJSON RulesConfigurationType where
  toJSON RulesConfigurationType {..} =
    Core.object (Core.catMaybes [Core.Just ("Rules" Core..= rules)])

instance Core.FromJSON RulesConfigurationType where
  parseJSON =
    Core.withObject "RulesConfigurationType" Core.$
      \x -> RulesConfigurationType' Core.<$> (x Core..: "Rules")
