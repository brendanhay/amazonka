{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionRequest
  ( LoadPermissionRequest (..),

    -- * Smart constructor
    mkLoadPermissionRequest,

    -- * Lenses
    lprGroup,
    lprUserId,
  )
where

import Network.AWS.EC2.Types.PermissionGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a load permission.
--
-- /See:/ 'mkLoadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { -- | The name of the group.
    group :: Lude.Maybe PermissionGroup,
    -- | The AWS account ID.
    userId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadPermissionRequest' with the minimum fields required to make a request.
--
-- * 'group' - The name of the group.
-- * 'userId' - The AWS account ID.
mkLoadPermissionRequest ::
  LoadPermissionRequest
mkLoadPermissionRequest =
  LoadPermissionRequest'
    { group = Lude.Nothing,
      userId = Lude.Nothing
    }

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprGroup :: Lens.Lens' LoadPermissionRequest (Lude.Maybe PermissionGroup)
lprGroup = Lens.lens (group :: LoadPermissionRequest -> Lude.Maybe PermissionGroup) (\s a -> s {group = a} :: LoadPermissionRequest)
{-# DEPRECATED lprGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprUserId :: Lens.Lens' LoadPermissionRequest (Lude.Maybe Lude.Text)
lprUserId = Lens.lens (userId :: LoadPermissionRequest -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: LoadPermissionRequest)
{-# DEPRECATED lprUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.ToQuery LoadPermissionRequest where
  toQuery LoadPermissionRequest' {..} =
    Lude.mconcat ["Group" Lude.=: group, "UserId" Lude.=: userId]
