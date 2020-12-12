{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PolicyToPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PolicyToPath
  ( PolicyToPath (..),

    -- * Smart constructor
    mkPolicyToPath,

    -- * Lenses
    ptpPath,
    ptpPolicies,
  )
where

import Network.AWS.CloudDirectory.Types.PolicyAttachment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used when a regular object exists in a 'Directory' and you want to find all of the policies that are associated with that object and the parent to that object.
--
-- /See:/ 'mkPolicyToPath' smart constructor.
data PolicyToPath = PolicyToPath'
  { path :: Lude.Maybe Lude.Text,
    policies :: Lude.Maybe [PolicyAttachment]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyToPath' with the minimum fields required to make a request.
--
-- * 'path' - The path that is referenced from the root.
-- * 'policies' - List of policy objects.
mkPolicyToPath ::
  PolicyToPath
mkPolicyToPath =
  PolicyToPath' {path = Lude.Nothing, policies = Lude.Nothing}

-- | The path that is referenced from the root.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpPath :: Lens.Lens' PolicyToPath (Lude.Maybe Lude.Text)
ptpPath = Lens.lens (path :: PolicyToPath -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: PolicyToPath)
{-# DEPRECATED ptpPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | List of policy objects.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpPolicies :: Lens.Lens' PolicyToPath (Lude.Maybe [PolicyAttachment])
ptpPolicies = Lens.lens (policies :: PolicyToPath -> Lude.Maybe [PolicyAttachment]) (\s a -> s {policies = a} :: PolicyToPath)
{-# DEPRECATED ptpPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromJSON PolicyToPath where
  parseJSON =
    Lude.withObject
      "PolicyToPath"
      ( \x ->
          PolicyToPath'
            Lude.<$> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "Policies" Lude..!= Lude.mempty)
      )
