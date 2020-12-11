{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateUserHierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user hierarchy group.
module Network.AWS.Connect.CreateUserHierarchyGroup
  ( -- * Creating a request
    CreateUserHierarchyGroup (..),
    mkCreateUserHierarchyGroup,

    -- ** Request lenses
    cuhgParentGroupId,
    cuhgName,
    cuhgInstanceId,

    -- * Destructuring the response
    CreateUserHierarchyGroupResponse (..),
    mkCreateUserHierarchyGroupResponse,

    -- ** Response lenses
    cuhgrsHierarchyGroupARN,
    cuhgrsHierarchyGroupId,
    cuhgrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUserHierarchyGroup' smart constructor.
data CreateUserHierarchyGroup = CreateUserHierarchyGroup'
  { parentGroupId ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserHierarchyGroup' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'name' - The name of the user hierarchy group. Must not be more than 100 characters.
-- * 'parentGroupId' - The identifier for the parent hierarchy group. The user hierarchy is created at level one if the parent group ID is null.
mkCreateUserHierarchyGroup ::
  -- | 'name'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  CreateUserHierarchyGroup
mkCreateUserHierarchyGroup pName_ pInstanceId_ =
  CreateUserHierarchyGroup'
    { parentGroupId = Lude.Nothing,
      name = pName_,
      instanceId = pInstanceId_
    }

-- | The identifier for the parent hierarchy group. The user hierarchy is created at level one if the parent group ID is null.
--
-- /Note:/ Consider using 'parentGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgParentGroupId :: Lens.Lens' CreateUserHierarchyGroup (Lude.Maybe Lude.Text)
cuhgParentGroupId = Lens.lens (parentGroupId :: CreateUserHierarchyGroup -> Lude.Maybe Lude.Text) (\s a -> s {parentGroupId = a} :: CreateUserHierarchyGroup)
{-# DEPRECATED cuhgParentGroupId "Use generic-lens or generic-optics with 'parentGroupId' instead." #-}

-- | The name of the user hierarchy group. Must not be more than 100 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgName :: Lens.Lens' CreateUserHierarchyGroup Lude.Text
cuhgName = Lens.lens (name :: CreateUserHierarchyGroup -> Lude.Text) (\s a -> s {name = a} :: CreateUserHierarchyGroup)
{-# DEPRECATED cuhgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgInstanceId :: Lens.Lens' CreateUserHierarchyGroup Lude.Text
cuhgInstanceId = Lens.lens (instanceId :: CreateUserHierarchyGroup -> Lude.Text) (\s a -> s {instanceId = a} :: CreateUserHierarchyGroup)
{-# DEPRECATED cuhgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest CreateUserHierarchyGroup where
  type Rs CreateUserHierarchyGroup = CreateUserHierarchyGroupResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserHierarchyGroupResponse'
            Lude.<$> (x Lude..?> "HierarchyGroupArn")
            Lude.<*> (x Lude..?> "HierarchyGroupId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserHierarchyGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserHierarchyGroup where
  toJSON CreateUserHierarchyGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentGroupId" Lude..=) Lude.<$> parentGroupId,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateUserHierarchyGroup where
  toPath CreateUserHierarchyGroup' {..} =
    Lude.mconcat ["/user-hierarchy-groups/", Lude.toBS instanceId]

instance Lude.ToQuery CreateUserHierarchyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserHierarchyGroupResponse' smart constructor.
data CreateUserHierarchyGroupResponse = CreateUserHierarchyGroupResponse'
  { hierarchyGroupARN ::
      Lude.Maybe Lude.Text,
    hierarchyGroupId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserHierarchyGroupResponse' with the minimum fields required to make a request.
--
-- * 'hierarchyGroupARN' - The Amazon Resource Name (ARN) of the hierarchy group.
-- * 'hierarchyGroupId' - The identifier of the hierarchy group.
-- * 'responseStatus' - The response status code.
mkCreateUserHierarchyGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserHierarchyGroupResponse
mkCreateUserHierarchyGroupResponse pResponseStatus_ =
  CreateUserHierarchyGroupResponse'
    { hierarchyGroupARN =
        Lude.Nothing,
      hierarchyGroupId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgrsHierarchyGroupARN :: Lens.Lens' CreateUserHierarchyGroupResponse (Lude.Maybe Lude.Text)
cuhgrsHierarchyGroupARN = Lens.lens (hierarchyGroupARN :: CreateUserHierarchyGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {hierarchyGroupARN = a} :: CreateUserHierarchyGroupResponse)
{-# DEPRECATED cuhgrsHierarchyGroupARN "Use generic-lens or generic-optics with 'hierarchyGroupARN' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgrsHierarchyGroupId :: Lens.Lens' CreateUserHierarchyGroupResponse (Lude.Maybe Lude.Text)
cuhgrsHierarchyGroupId = Lens.lens (hierarchyGroupId :: CreateUserHierarchyGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {hierarchyGroupId = a} :: CreateUserHierarchyGroupResponse)
{-# DEPRECATED cuhgrsHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuhgrsResponseStatus :: Lens.Lens' CreateUserHierarchyGroupResponse Lude.Int
cuhgrsResponseStatus = Lens.lens (responseStatus :: CreateUserHierarchyGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserHierarchyGroupResponse)
{-# DEPRECATED cuhgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
