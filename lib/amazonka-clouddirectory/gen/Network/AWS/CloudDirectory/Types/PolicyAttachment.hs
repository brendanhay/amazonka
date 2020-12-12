{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PolicyAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PolicyAttachment
  ( PolicyAttachment (..),

    -- * Smart constructor
    mkPolicyAttachment,

    -- * Lenses
    paPolicyId,
    paPolicyType,
    paObjectIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the @PolicyType@ , @PolicyId@ , and the @ObjectIdentifier@ to which it is attached. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /See:/ 'mkPolicyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
  { policyId ::
      Lude.Maybe Lude.Text,
    policyType :: Lude.Maybe Lude.Text,
    objectIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyAttachment' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
-- * 'policyId' - The ID of @PolicyAttachment@ .
-- * 'policyType' - The type of policy that can be associated with @PolicyAttachment@ .
mkPolicyAttachment ::
  PolicyAttachment
mkPolicyAttachment =
  PolicyAttachment'
    { policyId = Lude.Nothing,
      policyType = Lude.Nothing,
      objectIdentifier = Lude.Nothing
    }

-- | The ID of @PolicyAttachment@ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPolicyId :: Lens.Lens' PolicyAttachment (Lude.Maybe Lude.Text)
paPolicyId = Lens.lens (policyId :: PolicyAttachment -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: PolicyAttachment)
{-# DEPRECATED paPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The type of policy that can be associated with @PolicyAttachment@ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPolicyType :: Lens.Lens' PolicyAttachment (Lude.Maybe Lude.Text)
paPolicyType = Lens.lens (policyType :: PolicyAttachment -> Lude.Maybe Lude.Text) (\s a -> s {policyType = a} :: PolicyAttachment)
{-# DEPRECATED paPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paObjectIdentifier :: Lens.Lens' PolicyAttachment (Lude.Maybe Lude.Text)
paObjectIdentifier = Lens.lens (objectIdentifier :: PolicyAttachment -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: PolicyAttachment)
{-# DEPRECATED paObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Lude.FromJSON PolicyAttachment where
  parseJSON =
    Lude.withObject
      "PolicyAttachment"
      ( \x ->
          PolicyAttachment'
            Lude.<$> (x Lude..:? "PolicyId")
            Lude.<*> (x Lude..:? "PolicyType")
            Lude.<*> (x Lude..:? "ObjectIdentifier")
      )
