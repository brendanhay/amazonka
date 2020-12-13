{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves group resource details.
module Network.AWS.XRay.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggGroupARN,
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
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The ARN of the group that was generated on creation.
    groupARN :: Lude.Maybe Lude.Text,
    -- | The case-sensitive name of the group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- * 'groupARN' - The ARN of the group that was generated on creation.
-- * 'groupName' - The case-sensitive name of the group.
mkGetGroup ::
  GetGroup
mkGetGroup =
  GetGroup' {groupARN = Lude.Nothing, groupName = Lude.Nothing}

-- | The ARN of the group that was generated on creation.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupARN :: Lens.Lens' GetGroup (Lude.Maybe Lude.Text)
ggGroupARN = Lens.lens (groupARN :: GetGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GetGroup)
{-# DEPRECATED ggGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup (Lude.Maybe Lude.Text)
ggGroupName = Lens.lens (groupName :: GetGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GetGroup)
{-# DEPRECATED ggGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = Req.postJSON xRayService
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
          [ ("GroupARN" Lude..=) Lude.<$> groupARN,
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath GetGroup where
  toPath = Lude.const "/GetGroup"

instance Lude.ToQuery GetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | The group that was requested. Contains the name of the group, the ARN of the group, the filter expression, and the insight configuration assigned to the group.
    group :: Lude.Maybe Group,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The group that was requested. Contains the name of the group, the ARN of the group, the filter expression, and the insight configuration assigned to the group.
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

-- | The group that was requested. Contains the name of the group, the ARN of the group, the filter expression, and the insight configuration assigned to the group.
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
