{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRule
  ( OrganizationConfigRule (..),

    -- * Smart constructor
    mkOrganizationConfigRule,

    -- * Lenses
    ocrOrganizationConfigRuleName,
    ocrOrganizationConfigRuleArn,
    ocrExcludedAccounts,
    ocrLastUpdateTime,
    ocrOrganizationCustomRuleMetadata,
    ocrOrganizationManagedRuleMetadata,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.OrganizationConfigRuleArn as Types
import qualified Network.AWS.Config.Types.OrganizationConfigRuleName as Types
import qualified Network.AWS.Config.Types.OrganizationCustomRuleMetadata as Types
import qualified Network.AWS.Config.Types.OrganizationManagedRuleMetadata as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An organization config rule that has information about config rules that AWS Config creates in member accounts.
--
-- /See:/ 'mkOrganizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { -- | The name that you assign to organization config rule.
    organizationConfigRuleName :: Types.OrganizationConfigRuleName,
    -- | Amazon Resource Name (ARN) of organization config rule.
    organizationConfigRuleArn :: Types.OrganizationConfigRuleArn,
    -- | A comma-separated list of accounts excluded from organization config rule.
    excludedAccounts :: Core.Maybe [Types.AccountId],
    -- | The timestamp of the last update.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | An @OrganizationCustomRuleMetadata@ object.
    organizationCustomRuleMetadata :: Core.Maybe Types.OrganizationCustomRuleMetadata,
    -- | An @OrganizationManagedRuleMetadata@ object.
    organizationManagedRuleMetadata :: Core.Maybe Types.OrganizationManagedRuleMetadata
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OrganizationConfigRule' value with any optional fields omitted.
mkOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Types.OrganizationConfigRuleName ->
  -- | 'organizationConfigRuleArn'
  Types.OrganizationConfigRuleArn ->
  OrganizationConfigRule
mkOrganizationConfigRule
  organizationConfigRuleName
  organizationConfigRuleArn =
    OrganizationConfigRule'
      { organizationConfigRuleName,
        organizationConfigRuleArn,
        excludedAccounts = Core.Nothing,
        lastUpdateTime = Core.Nothing,
        organizationCustomRuleMetadata = Core.Nothing,
        organizationManagedRuleMetadata = Core.Nothing
      }

-- | The name that you assign to organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationConfigRuleName :: Lens.Lens' OrganizationConfigRule Types.OrganizationConfigRuleName
ocrOrganizationConfigRuleName = Lens.field @"organizationConfigRuleName"
{-# DEPRECATED ocrOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

-- | Amazon Resource Name (ARN) of organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationConfigRuleArn :: Lens.Lens' OrganizationConfigRule Types.OrganizationConfigRuleArn
ocrOrganizationConfigRuleArn = Lens.field @"organizationConfigRuleArn"
{-# DEPRECATED ocrOrganizationConfigRuleArn "Use generic-lens or generic-optics with 'organizationConfigRuleArn' instead." #-}

-- | A comma-separated list of accounts excluded from organization config rule.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrExcludedAccounts :: Lens.Lens' OrganizationConfigRule (Core.Maybe [Types.AccountId])
ocrExcludedAccounts = Lens.field @"excludedAccounts"
{-# DEPRECATED ocrExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

-- | The timestamp of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrLastUpdateTime :: Lens.Lens' OrganizationConfigRule (Core.Maybe Core.NominalDiffTime)
ocrLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED ocrLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | An @OrganizationCustomRuleMetadata@ object.
--
-- /Note:/ Consider using 'organizationCustomRuleMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationCustomRuleMetadata :: Lens.Lens' OrganizationConfigRule (Core.Maybe Types.OrganizationCustomRuleMetadata)
ocrOrganizationCustomRuleMetadata = Lens.field @"organizationCustomRuleMetadata"
{-# DEPRECATED ocrOrganizationCustomRuleMetadata "Use generic-lens or generic-optics with 'organizationCustomRuleMetadata' instead." #-}

-- | An @OrganizationManagedRuleMetadata@ object.
--
-- /Note:/ Consider using 'organizationManagedRuleMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationManagedRuleMetadata :: Lens.Lens' OrganizationConfigRule (Core.Maybe Types.OrganizationManagedRuleMetadata)
ocrOrganizationManagedRuleMetadata = Lens.field @"organizationManagedRuleMetadata"
{-# DEPRECATED ocrOrganizationManagedRuleMetadata "Use generic-lens or generic-optics with 'organizationManagedRuleMetadata' instead." #-}

instance Core.FromJSON OrganizationConfigRule where
  parseJSON =
    Core.withObject "OrganizationConfigRule" Core.$
      \x ->
        OrganizationConfigRule'
          Core.<$> (x Core..: "OrganizationConfigRuleName")
          Core.<*> (x Core..: "OrganizationConfigRuleArn")
          Core.<*> (x Core..:? "ExcludedAccounts")
          Core.<*> (x Core..:? "LastUpdateTime")
          Core.<*> (x Core..:? "OrganizationCustomRuleMetadata")
          Core.<*> (x Core..:? "OrganizationManagedRuleMetadata")
