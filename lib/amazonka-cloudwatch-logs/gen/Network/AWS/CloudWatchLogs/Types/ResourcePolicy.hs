-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ResourcePolicy
  ( ResourcePolicy (..),

    -- * Smart constructor
    mkResourcePolicy,

    -- * Lenses
    rpPolicyName,
    rpPolicyDocument,
    rpLastUpdatedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A policy enabling one or more entities to put logs to a log group in this account.
--
-- /See:/ 'mkResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { policyName ::
      Lude.Maybe Lude.Text,
    policyDocument :: Lude.Maybe Lude.Text,
    lastUpdatedTime :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourcePolicy' with the minimum fields required to make a request.
--
-- * 'lastUpdatedTime' - Timestamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'policyDocument' - The details of the policy.
-- * 'policyName' - The name of the resource policy.
mkResourcePolicy ::
  ResourcePolicy
mkResourcePolicy =
  ResourcePolicy'
    { policyName = Lude.Nothing,
      policyDocument = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing
    }

-- | The name of the resource policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPolicyName :: Lens.Lens' ResourcePolicy (Lude.Maybe Lude.Text)
rpPolicyName = Lens.lens (policyName :: ResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: ResourcePolicy)
{-# DEPRECATED rpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The details of the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPolicyDocument :: Lens.Lens' ResourcePolicy (Lude.Maybe Lude.Text)
rpPolicyDocument = Lens.lens (policyDocument :: ResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: ResourcePolicy)
{-# DEPRECATED rpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | Timestamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpLastUpdatedTime :: Lens.Lens' ResourcePolicy (Lude.Maybe Lude.Natural)
rpLastUpdatedTime = Lens.lens (lastUpdatedTime :: ResourcePolicy -> Lude.Maybe Lude.Natural) (\s a -> s {lastUpdatedTime = a} :: ResourcePolicy)
{-# DEPRECATED rpLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

instance Lude.FromJSON ResourcePolicy where
  parseJSON =
    Lude.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Lude.<$> (x Lude..:? "policyName")
            Lude.<*> (x Lude..:? "policyDocument")
            Lude.<*> (x Lude..:? "lastUpdatedTime")
      )
