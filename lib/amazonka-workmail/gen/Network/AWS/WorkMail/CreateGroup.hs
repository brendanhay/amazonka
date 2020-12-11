{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group that can be used in Amazon WorkMail by calling the 'RegisterToWorkMail' operation.
module Network.AWS.WorkMail.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgOrganizationId,
    cgName,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrsGroupId,
    cgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { organizationId :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- * 'name' - The name of the group.
-- * 'organizationId' - The organization under which the group is to be created.
mkCreateGroup ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateGroup
mkCreateGroup pOrganizationId_ pName_ =
  CreateGroup' {organizationId = pOrganizationId_, name = pName_}

-- | The organization under which the group is to be created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgOrganizationId :: Lens.Lens' CreateGroup Lude.Text
cgOrganizationId = Lens.lens (organizationId :: CreateGroup -> Lude.Text) (\s a -> s {organizationId = a} :: CreateGroup)
{-# DEPRECATED cgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' CreateGroup Lude.Text
cgName = Lens.lens (name :: CreateGroup -> Lude.Text) (\s a -> s {name = a} :: CreateGroup)
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Lude.<$> (x Lude..?> "GroupId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.CreateGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { groupId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier of the group.
-- * 'responseStatus' - The response status code.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupResponse
mkCreateGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { groupId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGroupId :: Lens.Lens' CreateGroupResponse (Lude.Maybe Lude.Text)
cgrsGroupId = Lens.lens (groupId :: CreateGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGroupResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
