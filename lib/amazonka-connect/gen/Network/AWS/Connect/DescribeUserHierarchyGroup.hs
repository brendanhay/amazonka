{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeUserHierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified hierarchy group.
module Network.AWS.Connect.DescribeUserHierarchyGroup
  ( -- * Creating a request
    DescribeUserHierarchyGroup (..),
    mkDescribeUserHierarchyGroup,

    -- ** Request lenses
    duhgInstanceId,
    duhgHierarchyGroupId,

    -- * Destructuring the response
    DescribeUserHierarchyGroupResponse (..),
    mkDescribeUserHierarchyGroupResponse,

    -- ** Response lenses
    duhgrsHierarchyGroup,
    duhgrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserHierarchyGroup' smart constructor.
data DescribeUserHierarchyGroup = DescribeUserHierarchyGroup'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserHierarchyGroup' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'hierarchyGroupId' - The identifier of the hierarchy group.
mkDescribeUserHierarchyGroup ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'hierarchyGroupId'
  Lude.Text ->
  DescribeUserHierarchyGroup
mkDescribeUserHierarchyGroup pInstanceId_ pHierarchyGroupId_ =
  DescribeUserHierarchyGroup'
    { instanceId = pInstanceId_,
      hierarchyGroupId = pHierarchyGroupId_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhgInstanceId :: Lens.Lens' DescribeUserHierarchyGroup Lude.Text
duhgInstanceId = Lens.lens (instanceId :: DescribeUserHierarchyGroup -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeUserHierarchyGroup)
{-# DEPRECATED duhgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhgHierarchyGroupId :: Lens.Lens' DescribeUserHierarchyGroup Lude.Text
duhgHierarchyGroupId = Lens.lens (hierarchyGroupId :: DescribeUserHierarchyGroup -> Lude.Text) (\s a -> s {hierarchyGroupId = a} :: DescribeUserHierarchyGroup)
{-# DEPRECATED duhgHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

instance Lude.AWSRequest DescribeUserHierarchyGroup where
  type
    Rs DescribeUserHierarchyGroup =
      DescribeUserHierarchyGroupResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserHierarchyGroupResponse'
            Lude.<$> (x Lude..?> "HierarchyGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserHierarchyGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeUserHierarchyGroup where
  toPath DescribeUserHierarchyGroup' {..} =
    Lude.mconcat
      [ "/user-hierarchy-groups/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS hierarchyGroupId
      ]

instance Lude.ToQuery DescribeUserHierarchyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserHierarchyGroupResponse' smart constructor.
data DescribeUserHierarchyGroupResponse = DescribeUserHierarchyGroupResponse'
  { -- | Information about the hierarchy group.
    hierarchyGroup :: Lude.Maybe HierarchyGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserHierarchyGroupResponse' with the minimum fields required to make a request.
--
-- * 'hierarchyGroup' - Information about the hierarchy group.
-- * 'responseStatus' - The response status code.
mkDescribeUserHierarchyGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserHierarchyGroupResponse
mkDescribeUserHierarchyGroupResponse pResponseStatus_ =
  DescribeUserHierarchyGroupResponse'
    { hierarchyGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhgrsHierarchyGroup :: Lens.Lens' DescribeUserHierarchyGroupResponse (Lude.Maybe HierarchyGroup)
duhgrsHierarchyGroup = Lens.lens (hierarchyGroup :: DescribeUserHierarchyGroupResponse -> Lude.Maybe HierarchyGroup) (\s a -> s {hierarchyGroup = a} :: DescribeUserHierarchyGroupResponse)
{-# DEPRECATED duhgrsHierarchyGroup "Use generic-lens or generic-optics with 'hierarchyGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhgrsResponseStatus :: Lens.Lens' DescribeUserHierarchyGroupResponse Lude.Int
duhgrsResponseStatus = Lens.lens (responseStatus :: DescribeUserHierarchyGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserHierarchyGroupResponse)
{-# DEPRECATED duhgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
