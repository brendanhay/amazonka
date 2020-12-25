{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.MappingRule
  ( MappingRule (..),

    -- * Smart constructor
    mkMappingRule,

    -- * Lenses
    mrClaim,
    mrMatchType,
    mrValue,
    mrRoleARN,
  )
where

import qualified Network.AWS.CognitoIdentity.Types.Claim as Types
import qualified Network.AWS.CognitoIdentity.Types.MappingRuleMatchType as Types
import qualified Network.AWS.CognitoIdentity.Types.RoleARN as Types
import qualified Network.AWS.CognitoIdentity.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A rule that maps a claim name, a claim value, and a match type to a role ARN.
--
-- /See:/ 'mkMappingRule' smart constructor.
data MappingRule = MappingRule'
  { -- | The claim name that must be present in the token, for example, "isAdmin" or "paid".
    claim :: Types.Claim,
    -- | The match condition that specifies how closely the claim value in the IdP token must match @Value@ .
    matchType :: Types.MappingRuleMatchType,
    -- | A brief string that the claim must match, for example, "paid" or "yes".
    value :: Types.Value,
    -- | The role ARN.
    roleARN :: Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MappingRule' value with any optional fields omitted.
mkMappingRule ::
  -- | 'claim'
  Types.Claim ->
  -- | 'matchType'
  Types.MappingRuleMatchType ->
  -- | 'value'
  Types.Value ->
  -- | 'roleARN'
  Types.RoleARN ->
  MappingRule
mkMappingRule claim matchType value roleARN =
  MappingRule' {claim, matchType, value, roleARN}

-- | The claim name that must be present in the token, for example, "isAdmin" or "paid".
--
-- /Note:/ Consider using 'claim' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrClaim :: Lens.Lens' MappingRule Types.Claim
mrClaim = Lens.field @"claim"
{-# DEPRECATED mrClaim "Use generic-lens or generic-optics with 'claim' instead." #-}

-- | The match condition that specifies how closely the claim value in the IdP token must match @Value@ .
--
-- /Note:/ Consider using 'matchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrMatchType :: Lens.Lens' MappingRule Types.MappingRuleMatchType
mrMatchType = Lens.field @"matchType"
{-# DEPRECATED mrMatchType "Use generic-lens or generic-optics with 'matchType' instead." #-}

-- | A brief string that the claim must match, for example, "paid" or "yes".
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrValue :: Lens.Lens' MappingRule Types.Value
mrValue = Lens.field @"value"
{-# DEPRECATED mrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrRoleARN :: Lens.Lens' MappingRule Types.RoleARN
mrRoleARN = Lens.field @"roleARN"
{-# DEPRECATED mrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON MappingRule where
  toJSON MappingRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Claim" Core..= claim),
            Core.Just ("MatchType" Core..= matchType),
            Core.Just ("Value" Core..= value),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )

instance Core.FromJSON MappingRule where
  parseJSON =
    Core.withObject "MappingRule" Core.$
      \x ->
        MappingRule'
          Core.<$> (x Core..: "Claim")
          Core.<*> (x Core..: "MatchType")
          Core.<*> (x Core..: "Value")
          Core.<*> (x Core..: "RoleARN")
