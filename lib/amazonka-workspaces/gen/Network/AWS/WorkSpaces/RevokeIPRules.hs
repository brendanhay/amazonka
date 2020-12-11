{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RevokeIPRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more rules from the specified IP access control group.
module Network.AWS.WorkSpaces.RevokeIPRules
  ( -- * Creating a request
    RevokeIPRules (..),
    mkRevokeIPRules,

    -- ** Request lenses
    rirGroupId,
    rirUserRules,

    -- * Destructuring the response
    RevokeIPRulesResponse (..),
    mkRevokeIPRulesResponse,

    -- ** Response lenses
    rirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkRevokeIPRules' smart constructor.
data RevokeIPRules = RevokeIPRules'
  { groupId :: Lude.Text,
    userRules :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeIPRules' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier of the group.
-- * 'userRules' - The rules to remove from the group.
mkRevokeIPRules ::
  -- | 'groupId'
  Lude.Text ->
  RevokeIPRules
mkRevokeIPRules pGroupId_ =
  RevokeIPRules' {groupId = pGroupId_, userRules = Lude.mempty}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirGroupId :: Lens.Lens' RevokeIPRules Lude.Text
rirGroupId = Lens.lens (groupId :: RevokeIPRules -> Lude.Text) (\s a -> s {groupId = a} :: RevokeIPRules)
{-# DEPRECATED rirGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The rules to remove from the group.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirUserRules :: Lens.Lens' RevokeIPRules [Lude.Text]
rirUserRules = Lens.lens (userRules :: RevokeIPRules -> [Lude.Text]) (\s a -> s {userRules = a} :: RevokeIPRules)
{-# DEPRECATED rirUserRules "Use generic-lens or generic-optics with 'userRules' instead." #-}

instance Lude.AWSRequest RevokeIPRules where
  type Rs RevokeIPRules = RevokeIPRulesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RevokeIPRulesResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeIPRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.RevokeIpRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RevokeIPRules where
  toJSON RevokeIPRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GroupId" Lude..= groupId),
            Lude.Just ("UserRules" Lude..= userRules)
          ]
      )

instance Lude.ToPath RevokeIPRules where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeIPRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRevokeIPRulesResponse' smart constructor.
newtype RevokeIPRulesResponse = RevokeIPRulesResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeIPRulesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRevokeIPRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeIPRulesResponse
mkRevokeIPRulesResponse pResponseStatus_ =
  RevokeIPRulesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RevokeIPRulesResponse Lude.Int
rirrsResponseStatus = Lens.lens (responseStatus :: RevokeIPRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeIPRulesResponse)
{-# DEPRECATED rirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
