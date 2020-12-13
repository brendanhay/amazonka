{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UntagRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.UntagRole
  ( -- * Creating a request
    UntagRole (..),
    mkUntagRole,

    -- ** Request lenses
    uRoleName,
    uTagKeys,

    -- * Destructuring the response
    UntagRoleResponse (..),
    mkUntagRoleResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagRole' smart constructor.
data UntagRole = UntagRole'
  { -- | The name of the IAM role from which you want to remove tags.
    --
    -- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified role.
    tagKeys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the IAM role from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'tagKeys' - A list of key names as a simple array of strings. The tags with matching keys are removed from the specified role.
mkUntagRole ::
  -- | 'roleName'
  Lude.Text ->
  UntagRole
mkUntagRole pRoleName_ =
  UntagRole' {roleName = pRoleName_, tagKeys = Lude.mempty}

-- | The name of the IAM role from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRoleName :: Lens.Lens' UntagRole Lude.Text
uRoleName = Lens.lens (roleName :: UntagRole -> Lude.Text) (\s a -> s {roleName = a} :: UntagRole)
{-# DEPRECATED uRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified role.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTagKeys :: Lens.Lens' UntagRole [Lude.Text]
uTagKeys = Lens.lens (tagKeys :: UntagRole -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagRole)
{-# DEPRECATED uTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest UntagRole where
  type Rs UntagRole = UntagRoleResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UntagRoleResponse'

instance Lude.ToHeaders UntagRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagRole where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagRole where
  toQuery UntagRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UntagRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "TagKeys" Lude.=: Lude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'mkUntagRoleResponse' smart constructor.
data UntagRoleResponse = UntagRoleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagRoleResponse' with the minimum fields required to make a request.
mkUntagRoleResponse ::
  UntagRoleResponse
mkUntagRoleResponse = UntagRoleResponse'
