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
    ocrOrganizationManagedRuleMetadata,
    ocrExcludedAccounts,
    ocrOrganizationCustomRuleMetadata,
    ocrLastUpdateTime,
    ocrOrganizationConfigRuleName,
    ocrOrganizationConfigRuleARN,
  )
where

import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An organization config rule that has information about config rules that AWS Config creates in member accounts.
--
-- /See:/ 'mkOrganizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { organizationManagedRuleMetadata ::
      Lude.Maybe OrganizationManagedRuleMetadata,
    excludedAccounts :: Lude.Maybe [Lude.Text],
    organizationCustomRuleMetadata ::
      Lude.Maybe OrganizationCustomRuleMetadata,
    lastUpdateTime :: Lude.Maybe Lude.Timestamp,
    organizationConfigRuleName :: Lude.Text,
    organizationConfigRuleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationConfigRule' with the minimum fields required to make a request.
--
-- * 'excludedAccounts' - A comma-separated list of accounts excluded from organization config rule.
-- * 'lastUpdateTime' - The timestamp of the last update.
-- * 'organizationConfigRuleARN' - Amazon Resource Name (ARN) of organization config rule.
-- * 'organizationConfigRuleName' - The name that you assign to organization config rule.
-- * 'organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
-- * 'organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
mkOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Lude.Text ->
  -- | 'organizationConfigRuleARN'
  Lude.Text ->
  OrganizationConfigRule
mkOrganizationConfigRule
  pOrganizationConfigRuleName_
  pOrganizationConfigRuleARN_ =
    OrganizationConfigRule'
      { organizationManagedRuleMetadata =
          Lude.Nothing,
        excludedAccounts = Lude.Nothing,
        organizationCustomRuleMetadata = Lude.Nothing,
        lastUpdateTime = Lude.Nothing,
        organizationConfigRuleName = pOrganizationConfigRuleName_,
        organizationConfigRuleARN = pOrganizationConfigRuleARN_
      }

-- | An @OrganizationManagedRuleMetadata@ object.
--
-- /Note:/ Consider using 'organizationManagedRuleMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationManagedRuleMetadata :: Lens.Lens' OrganizationConfigRule (Lude.Maybe OrganizationManagedRuleMetadata)
ocrOrganizationManagedRuleMetadata = Lens.lens (organizationManagedRuleMetadata :: OrganizationConfigRule -> Lude.Maybe OrganizationManagedRuleMetadata) (\s a -> s {organizationManagedRuleMetadata = a} :: OrganizationConfigRule)
{-# DEPRECATED ocrOrganizationManagedRuleMetadata "Use generic-lens or generic-optics with 'organizationManagedRuleMetadata' instead." #-}

-- | A comma-separated list of accounts excluded from organization config rule.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrExcludedAccounts :: Lens.Lens' OrganizationConfigRule (Lude.Maybe [Lude.Text])
ocrExcludedAccounts = Lens.lens (excludedAccounts :: OrganizationConfigRule -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedAccounts = a} :: OrganizationConfigRule)
{-# DEPRECATED ocrExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

-- | An @OrganizationCustomRuleMetadata@ object.
--
-- /Note:/ Consider using 'organizationCustomRuleMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationCustomRuleMetadata :: Lens.Lens' OrganizationConfigRule (Lude.Maybe OrganizationCustomRuleMetadata)
ocrOrganizationCustomRuleMetadata = Lens.lens (organizationCustomRuleMetadata :: OrganizationConfigRule -> Lude.Maybe OrganizationCustomRuleMetadata) (\s a -> s {organizationCustomRuleMetadata = a} :: OrganizationConfigRule)
{-# DEPRECATED ocrOrganizationCustomRuleMetadata "Use generic-lens or generic-optics with 'organizationCustomRuleMetadata' instead." #-}

-- | The timestamp of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrLastUpdateTime :: Lens.Lens' OrganizationConfigRule (Lude.Maybe Lude.Timestamp)
ocrLastUpdateTime = Lens.lens (lastUpdateTime :: OrganizationConfigRule -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: OrganizationConfigRule)
{-# DEPRECATED ocrLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name that you assign to organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationConfigRuleName :: Lens.Lens' OrganizationConfigRule Lude.Text
ocrOrganizationConfigRuleName = Lens.lens (organizationConfigRuleName :: OrganizationConfigRule -> Lude.Text) (\s a -> s {organizationConfigRuleName = a} :: OrganizationConfigRule)
{-# DEPRECATED ocrOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

-- | Amazon Resource Name (ARN) of organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrOrganizationConfigRuleARN :: Lens.Lens' OrganizationConfigRule Lude.Text
ocrOrganizationConfigRuleARN = Lens.lens (organizationConfigRuleARN :: OrganizationConfigRule -> Lude.Text) (\s a -> s {organizationConfigRuleARN = a} :: OrganizationConfigRule)
{-# DEPRECATED ocrOrganizationConfigRuleARN "Use generic-lens or generic-optics with 'organizationConfigRuleARN' instead." #-}

instance Lude.FromJSON OrganizationConfigRule where
  parseJSON =
    Lude.withObject
      "OrganizationConfigRule"
      ( \x ->
          OrganizationConfigRule'
            Lude.<$> (x Lude..:? "OrganizationManagedRuleMetadata")
            Lude.<*> (x Lude..:? "ExcludedAccounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OrganizationCustomRuleMetadata")
            Lude.<*> (x Lude..:? "LastUpdateTime")
            Lude.<*> (x Lude..: "OrganizationConfigRuleName")
            Lude.<*> (x Lude..: "OrganizationConfigRuleArn")
      )
