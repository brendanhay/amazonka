{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateRulesOfIPGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current rules of the specified IP access control group with the specified rules.
module Network.AWS.WorkSpaces.UpdateRulesOfIPGroup
  ( -- * Creating a request
    UpdateRulesOfIPGroup (..),
    mkUpdateRulesOfIPGroup,

    -- ** Request lenses
    uroigUserRules,
    uroigGroupId,

    -- * Destructuring the response
    UpdateRulesOfIPGroupResponse (..),
    mkUpdateRulesOfIPGroupResponse,

    -- ** Response lenses
    uroigrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkUpdateRulesOfIPGroup' smart constructor.
data UpdateRulesOfIPGroup = UpdateRulesOfIPGroup'
  { -- | One or more rules.
    userRules :: [IPRuleItem],
    -- | The identifier of the group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRulesOfIPGroup' with the minimum fields required to make a request.
--
-- * 'userRules' - One or more rules.
-- * 'groupId' - The identifier of the group.
mkUpdateRulesOfIPGroup ::
  -- | 'groupId'
  Lude.Text ->
  UpdateRulesOfIPGroup
mkUpdateRulesOfIPGroup pGroupId_ =
  UpdateRulesOfIPGroup'
    { userRules = Lude.mempty,
      groupId = pGroupId_
    }

-- | One or more rules.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uroigUserRules :: Lens.Lens' UpdateRulesOfIPGroup [IPRuleItem]
uroigUserRules = Lens.lens (userRules :: UpdateRulesOfIPGroup -> [IPRuleItem]) (\s a -> s {userRules = a} :: UpdateRulesOfIPGroup)
{-# DEPRECATED uroigUserRules "Use generic-lens or generic-optics with 'userRules' instead." #-}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uroigGroupId :: Lens.Lens' UpdateRulesOfIPGroup Lude.Text
uroigGroupId = Lens.lens (groupId :: UpdateRulesOfIPGroup -> Lude.Text) (\s a -> s {groupId = a} :: UpdateRulesOfIPGroup)
{-# DEPRECATED uroigGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest UpdateRulesOfIPGroup where
  type Rs UpdateRulesOfIPGroup = UpdateRulesOfIPGroupResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateRulesOfIPGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRulesOfIPGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.UpdateRulesOfIpGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRulesOfIPGroup where
  toJSON UpdateRulesOfIPGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserRules" Lude..= userRules),
            Lude.Just ("GroupId" Lude..= groupId)
          ]
      )

instance Lude.ToPath UpdateRulesOfIPGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRulesOfIPGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRulesOfIPGroupResponse' smart constructor.
newtype UpdateRulesOfIPGroupResponse = UpdateRulesOfIPGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRulesOfIPGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateRulesOfIPGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRulesOfIPGroupResponse
mkUpdateRulesOfIPGroupResponse pResponseStatus_ =
  UpdateRulesOfIPGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uroigrsResponseStatus :: Lens.Lens' UpdateRulesOfIPGroupResponse Lude.Int
uroigrsResponseStatus = Lens.lens (responseStatus :: UpdateRulesOfIPGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRulesOfIPGroupResponse)
{-# DEPRECATED uroigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
