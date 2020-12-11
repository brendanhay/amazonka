-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTypeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeSummary
  ( PolicyTypeSummary (..),

    -- * Smart constructor
    mkPolicyTypeSummary,

    -- * Lenses
    ptsStatus,
    ptsType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Organizations.Types.PolicyTypeStatus
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a policy type and its status in the associated root.
--
-- /See:/ 'mkPolicyTypeSummary' smart constructor.
data PolicyTypeSummary = PolicyTypeSummary'
  { status ::
      Lude.Maybe PolicyTypeStatus,
    type' :: Lude.Maybe PolicyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyTypeSummary' with the minimum fields required to make a request.
--
-- * 'status' - The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
-- * 'type'' - The name of the policy type.
mkPolicyTypeSummary ::
  PolicyTypeSummary
mkPolicyTypeSummary =
  PolicyTypeSummary' {status = Lude.Nothing, type' = Lude.Nothing}

-- | The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsStatus :: Lens.Lens' PolicyTypeSummary (Lude.Maybe PolicyTypeStatus)
ptsStatus = Lens.lens (status :: PolicyTypeSummary -> Lude.Maybe PolicyTypeStatus) (\s a -> s {status = a} :: PolicyTypeSummary)
{-# DEPRECATED ptsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the policy type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsType :: Lens.Lens' PolicyTypeSummary (Lude.Maybe PolicyType)
ptsType = Lens.lens (type' :: PolicyTypeSummary -> Lude.Maybe PolicyType) (\s a -> s {type' = a} :: PolicyTypeSummary)
{-# DEPRECATED ptsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON PolicyTypeSummary where
  parseJSON =
    Lude.withObject
      "PolicyTypeSummary"
      ( \x ->
          PolicyTypeSummary'
            Lude.<$> (x Lude..:? "Status") Lude.<*> (x Lude..:? "Type")
      )
