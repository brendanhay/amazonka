{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GluePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GluePolicy
  ( GluePolicy (..),

    -- * Smart constructor
    mkGluePolicy,

    -- * Lenses
    gpPolicyInJSON,
    gpUpdateTime,
    gpPolicyHash,
    gpCreateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure for returning a resource policy.
--
-- /See:/ 'mkGluePolicy' smart constructor.
data GluePolicy = GluePolicy'
  { -- | Contains the requested policy document, in JSON format.
    policyInJSON :: Lude.Maybe Lude.Text,
    -- | The date and time at which the policy was last updated.
    updateTime :: Lude.Maybe Lude.Timestamp,
    -- | Contains the hash value associated with this policy.
    policyHash :: Lude.Maybe Lude.Text,
    -- | The date and time at which the policy was created.
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GluePolicy' with the minimum fields required to make a request.
--
-- * 'policyInJSON' - Contains the requested policy document, in JSON format.
-- * 'updateTime' - The date and time at which the policy was last updated.
-- * 'policyHash' - Contains the hash value associated with this policy.
-- * 'createTime' - The date and time at which the policy was created.
mkGluePolicy ::
  GluePolicy
mkGluePolicy =
  GluePolicy'
    { policyInJSON = Lude.Nothing,
      updateTime = Lude.Nothing,
      policyHash = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | Contains the requested policy document, in JSON format.
--
-- /Note:/ Consider using 'policyInJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyInJSON :: Lens.Lens' GluePolicy (Lude.Maybe Lude.Text)
gpPolicyInJSON = Lens.lens (policyInJSON :: GluePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyInJSON = a} :: GluePolicy)
{-# DEPRECATED gpPolicyInJSON "Use generic-lens or generic-optics with 'policyInJSON' instead." #-}

-- | The date and time at which the policy was last updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpUpdateTime :: Lens.Lens' GluePolicy (Lude.Maybe Lude.Timestamp)
gpUpdateTime = Lens.lens (updateTime :: GluePolicy -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateTime = a} :: GluePolicy)
{-# DEPRECATED gpUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | Contains the hash value associated with this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyHash :: Lens.Lens' GluePolicy (Lude.Maybe Lude.Text)
gpPolicyHash = Lens.lens (policyHash :: GluePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyHash = a} :: GluePolicy)
{-# DEPRECATED gpPolicyHash "Use generic-lens or generic-optics with 'policyHash' instead." #-}

-- | The date and time at which the policy was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpCreateTime :: Lens.Lens' GluePolicy (Lude.Maybe Lude.Timestamp)
gpCreateTime = Lens.lens (createTime :: GluePolicy -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: GluePolicy)
{-# DEPRECATED gpCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON GluePolicy where
  parseJSON =
    Lude.withObject
      "GluePolicy"
      ( \x ->
          GluePolicy'
            Lude.<$> (x Lude..:? "PolicyInJson")
            Lude.<*> (x Lude..:? "UpdateTime")
            Lude.<*> (x Lude..:? "PolicyHash")
            Lude.<*> (x Lude..:? "CreateTime")
      )
