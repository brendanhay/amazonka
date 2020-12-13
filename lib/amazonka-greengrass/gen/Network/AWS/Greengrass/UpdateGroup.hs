{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group.
module Network.AWS.Greengrass.UpdateGroup
  ( -- * Creating a request
    UpdateGroup (..),
    mkUpdateGroup,

    -- ** Request lenses
    ugName,
    ugGroupId,

    -- * Destructuring the response
    UpdateGroupResponse (..),
    mkUpdateGroupResponse,

    -- ** Response lenses
    ugrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The name of the definition.
    name :: Lude.Maybe Lude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- * 'name' - The name of the definition.
-- * 'groupId' - The ID of the Greengrass group.
mkUpdateGroup ::
  -- | 'groupId'
  Lude.Text ->
  UpdateGroup
mkUpdateGroup pGroupId_ =
  UpdateGroup' {name = Lude.Nothing, groupId = pGroupId_}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugName :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugName = Lens.lens (name :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateGroup)
{-# DEPRECATED ugName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupId :: Lens.Lens' UpdateGroup Lude.Text
ugGroupId = Lens.lens (groupId :: UpdateGroup -> Lude.Text) (\s a -> s {groupId = a} :: UpdateGroup)
{-# DEPRECATED ugGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateGroup where
  toPath UpdateGroup' {..} =
    Lude.mconcat ["/greengrass/groups/", Lude.toBS groupId]

instance Lude.ToQuery UpdateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
newtype UpdateGroupResponse = UpdateGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGroupResponse
mkUpdateGroupResponse pResponseStatus_ =
  UpdateGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsResponseStatus :: Lens.Lens' UpdateGroupResponse Lude.Int
ugrsResponseStatus = Lens.lens (responseStatus :: UpdateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
