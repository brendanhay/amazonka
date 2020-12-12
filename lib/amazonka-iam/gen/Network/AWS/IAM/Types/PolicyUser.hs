{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyUser
  ( PolicyUser (..),

    -- * Smart constructor
    mkPolicyUser,

    -- * Lenses
    puUserName,
    puUserId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a user that a managed policy is attached to.
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicyUser' smart constructor.
data PolicyUser = PolicyUser'
  { userName :: Lude.Maybe Lude.Text,
    userId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyUser' with the minimum fields required to make a request.
--
-- * 'userId' - The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'userName' - The name (friendly name, not ARN) identifying the user.
mkPolicyUser ::
  PolicyUser
mkPolicyUser =
  PolicyUser' {userName = Lude.Nothing, userId = Lude.Nothing}

-- | The name (friendly name, not ARN) identifying the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puUserName :: Lens.Lens' PolicyUser (Lude.Maybe Lude.Text)
puUserName = Lens.lens (userName :: PolicyUser -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: PolicyUser)
{-# DEPRECATED puUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puUserId :: Lens.Lens' PolicyUser (Lude.Maybe Lude.Text)
puUserId = Lens.lens (userId :: PolicyUser -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: PolicyUser)
{-# DEPRECATED puUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.FromXML PolicyUser where
  parseXML x =
    PolicyUser'
      Lude.<$> (x Lude..@? "UserName") Lude.<*> (x Lude..@? "UserId")
