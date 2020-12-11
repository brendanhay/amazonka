{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified resource group.
module Network.AWS.ResourceGroups.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggGroup,
    ggGroupName,

    -- * Destructuring the response
    GetGroupResponse (..),
    mkGetGroupResponse,

    -- ** Response lenses
    ggrsGroup,
    ggrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { group :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group to retrieve.
-- * 'groupName' - Don't use this parameter. Use @Group@ instead.
mkGetGroup ::
  GetGroup
mkGetGroup =
  GetGroup' {group = Lude.Nothing, groupName = Lude.Nothing}

-- | The name or the ARN of the resource group to retrieve.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroup :: Lens.Lens' GetGroup (Lude.Maybe Lude.Text)
ggGroup = Lens.lens (group :: GetGroup -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: GetGroup)
{-# DEPRECATED ggGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup (Lude.Maybe Lude.Text)
ggGroupName = Lens.lens (groupName :: GetGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GetGroup)
{-# DEPRECATED ggGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetGroup where
  toJSON GetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath GetGroup where
  toPath = Lude.const "/get-group"

instance Lude.ToQuery GetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { group ::
      Lude.Maybe Group,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - A full description of the resource group.
-- * 'responseStatus' - The response status code.
mkGetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupResponse
mkGetGroupResponse pResponseStatus_ =
  GetGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A full description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsGroup :: Lens.Lens' GetGroupResponse (Lude.Maybe Group)
ggrsGroup = Lens.lens (group :: GetGroupResponse -> Lude.Maybe Group) (\s a -> s {group = a} :: GetGroupResponse)
{-# DEPRECATED ggrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsResponseStatus :: Lens.Lens' GetGroupResponse Lude.Int
ggrsResponseStatus = Lens.lens (responseStatus :: GetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupResponse)
{-# DEPRECATED ggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
