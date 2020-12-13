{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateIPGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IP access control group.
--
-- An IP access control group provides you with the ability to control the IP addresses from which users are allowed to access their WorkSpaces. To specify the CIDR address ranges, add rules to your IP access control group and then associate the group with your directory. You can add rules when you create the group or at any time using 'AuthorizeIpRules' .
-- There is a default IP access control group associated with your directory. If you don't associate an IP access control group with your directory, the default group is used. The default group includes a default rule that allows users to access their WorkSpaces from anywhere. You cannot modify the default IP access control group for your directory.
module Network.AWS.WorkSpaces.CreateIPGroup
  ( -- * Creating a request
    CreateIPGroup (..),
    mkCreateIPGroup,

    -- ** Request lenses
    cigGroupDesc,
    cigUserRules,
    cigGroupName,
    cigTags,

    -- * Destructuring the response
    CreateIPGroupResponse (..),
    mkCreateIPGroupResponse,

    -- ** Response lenses
    cigrsGroupId,
    cigrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkCreateIPGroup' smart constructor.
data CreateIPGroup = CreateIPGroup'
  { -- | The description of the group.
    groupDesc :: Lude.Maybe Lude.Text,
    -- | The rules to add to the group.
    userRules :: Lude.Maybe [IPRuleItem],
    -- | The name of the group.
    groupName :: Lude.Text,
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIPGroup' with the minimum fields required to make a request.
--
-- * 'groupDesc' - The description of the group.
-- * 'userRules' - The rules to add to the group.
-- * 'groupName' - The name of the group.
-- * 'tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
mkCreateIPGroup ::
  -- | 'groupName'
  Lude.Text ->
  CreateIPGroup
mkCreateIPGroup pGroupName_ =
  CreateIPGroup'
    { groupDesc = Lude.Nothing,
      userRules = Lude.Nothing,
      groupName = pGroupName_,
      tags = Lude.Nothing
    }

-- | The description of the group.
--
-- /Note:/ Consider using 'groupDesc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigGroupDesc :: Lens.Lens' CreateIPGroup (Lude.Maybe Lude.Text)
cigGroupDesc = Lens.lens (groupDesc :: CreateIPGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupDesc = a} :: CreateIPGroup)
{-# DEPRECATED cigGroupDesc "Use generic-lens or generic-optics with 'groupDesc' instead." #-}

-- | The rules to add to the group.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigUserRules :: Lens.Lens' CreateIPGroup (Lude.Maybe [IPRuleItem])
cigUserRules = Lens.lens (userRules :: CreateIPGroup -> Lude.Maybe [IPRuleItem]) (\s a -> s {userRules = a} :: CreateIPGroup)
{-# DEPRECATED cigUserRules "Use generic-lens or generic-optics with 'userRules' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigGroupName :: Lens.Lens' CreateIPGroup Lude.Text
cigGroupName = Lens.lens (groupName :: CreateIPGroup -> Lude.Text) (\s a -> s {groupName = a} :: CreateIPGroup)
{-# DEPRECATED cigGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigTags :: Lens.Lens' CreateIPGroup (Lude.Maybe [Tag])
cigTags = Lens.lens (tags :: CreateIPGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateIPGroup)
{-# DEPRECATED cigTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateIPGroup where
  type Rs CreateIPGroup = CreateIPGroupResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateIPGroupResponse'
            Lude.<$> (x Lude..?> "GroupId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateIPGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.CreateIpGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateIPGroup where
  toJSON CreateIPGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupDesc" Lude..=) Lude.<$> groupDesc,
            ("UserRules" Lude..=) Lude.<$> userRules,
            Lude.Just ("GroupName" Lude..= groupName),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateIPGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateIPGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateIPGroupResponse' smart constructor.
data CreateIPGroupResponse = CreateIPGroupResponse'
  { -- | The identifier of the group.
    groupId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIPGroupResponse' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier of the group.
-- * 'responseStatus' - The response status code.
mkCreateIPGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateIPGroupResponse
mkCreateIPGroupResponse pResponseStatus_ =
  CreateIPGroupResponse'
    { groupId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrsGroupId :: Lens.Lens' CreateIPGroupResponse (Lude.Maybe Lude.Text)
cigrsGroupId = Lens.lens (groupId :: CreateIPGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: CreateIPGroupResponse)
{-# DEPRECATED cigrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrsResponseStatus :: Lens.Lens' CreateIPGroupResponse Lude.Int
cigrsResponseStatus = Lens.lens (responseStatus :: CreateIPGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateIPGroupResponse)
{-# DEPRECATED cigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
