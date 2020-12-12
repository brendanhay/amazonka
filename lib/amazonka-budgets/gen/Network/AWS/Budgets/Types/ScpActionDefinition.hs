{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ScpActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ScpActionDefinition
  ( ScpActionDefinition (..),

    -- * Smart constructor
    mkScpActionDefinition,

    -- * Lenses
    sadPolicyId,
    sadTargetIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The service control policies (SCP) action definition details.
--
-- /See:/ 'mkScpActionDefinition' smart constructor.
data ScpActionDefinition = ScpActionDefinition'
  { policyId ::
      Lude.Text,
    targetIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScpActionDefinition' with the minimum fields required to make a request.
--
-- * 'policyId' - The policy ID attached.
-- * 'targetIds' - A list of target IDs.
mkScpActionDefinition ::
  -- | 'policyId'
  Lude.Text ->
  -- | 'targetIds'
  Lude.NonEmpty Lude.Text ->
  ScpActionDefinition
mkScpActionDefinition pPolicyId_ pTargetIds_ =
  ScpActionDefinition'
    { policyId = pPolicyId_,
      targetIds = pTargetIds_
    }

-- | The policy ID attached.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadPolicyId :: Lens.Lens' ScpActionDefinition Lude.Text
sadPolicyId = Lens.lens (policyId :: ScpActionDefinition -> Lude.Text) (\s a -> s {policyId = a} :: ScpActionDefinition)
{-# DEPRECATED sadPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | A list of target IDs.
--
-- /Note:/ Consider using 'targetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadTargetIds :: Lens.Lens' ScpActionDefinition (Lude.NonEmpty Lude.Text)
sadTargetIds = Lens.lens (targetIds :: ScpActionDefinition -> Lude.NonEmpty Lude.Text) (\s a -> s {targetIds = a} :: ScpActionDefinition)
{-# DEPRECATED sadTargetIds "Use generic-lens or generic-optics with 'targetIds' instead." #-}

instance Lude.FromJSON ScpActionDefinition where
  parseJSON =
    Lude.withObject
      "ScpActionDefinition"
      ( \x ->
          ScpActionDefinition'
            Lude.<$> (x Lude..: "PolicyId") Lude.<*> (x Lude..: "TargetIds")
      )

instance Lude.ToJSON ScpActionDefinition where
  toJSON ScpActionDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyId" Lude..= policyId),
            Lude.Just ("TargetIds" Lude..= targetIds)
          ]
      )
