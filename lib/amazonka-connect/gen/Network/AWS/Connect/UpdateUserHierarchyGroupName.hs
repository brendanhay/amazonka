{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchyGroupName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of the user hierarchy group.
module Network.AWS.Connect.UpdateUserHierarchyGroupName
  ( -- * Creating a request
    UpdateUserHierarchyGroupName (..),
    mkUpdateUserHierarchyGroupName,

    -- ** Request lenses
    uuhgnInstanceId,
    uuhgnName,
    uuhgnHierarchyGroupId,

    -- * Destructuring the response
    UpdateUserHierarchyGroupNameResponse (..),
    mkUpdateUserHierarchyGroupNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserHierarchyGroupName' smart constructor.
data UpdateUserHierarchyGroupName = UpdateUserHierarchyGroupName'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The name of the hierarchy group. Must not be more than 100 characters.
    name :: Lude.Text,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserHierarchyGroupName' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'name' - The name of the hierarchy group. Must not be more than 100 characters.
-- * 'hierarchyGroupId' - The identifier of the hierarchy group.
mkUpdateUserHierarchyGroupName ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'hierarchyGroupId'
  Lude.Text ->
  UpdateUserHierarchyGroupName
mkUpdateUserHierarchyGroupName
  pInstanceId_
  pName_
  pHierarchyGroupId_ =
    UpdateUserHierarchyGroupName'
      { instanceId = pInstanceId_,
        name = pName_,
        hierarchyGroupId = pHierarchyGroupId_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhgnInstanceId :: Lens.Lens' UpdateUserHierarchyGroupName Lude.Text
uuhgnInstanceId = Lens.lens (instanceId :: UpdateUserHierarchyGroupName -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserHierarchyGroupName)
{-# DEPRECATED uuhgnInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the hierarchy group. Must not be more than 100 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhgnName :: Lens.Lens' UpdateUserHierarchyGroupName Lude.Text
uuhgnName = Lens.lens (name :: UpdateUserHierarchyGroupName -> Lude.Text) (\s a -> s {name = a} :: UpdateUserHierarchyGroupName)
{-# DEPRECATED uuhgnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhgnHierarchyGroupId :: Lens.Lens' UpdateUserHierarchyGroupName Lude.Text
uuhgnHierarchyGroupId = Lens.lens (hierarchyGroupId :: UpdateUserHierarchyGroupName -> Lude.Text) (\s a -> s {hierarchyGroupId = a} :: UpdateUserHierarchyGroupName)
{-# DEPRECATED uuhgnHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

instance Lude.AWSRequest UpdateUserHierarchyGroupName where
  type
    Rs UpdateUserHierarchyGroupName =
      UpdateUserHierarchyGroupNameResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserHierarchyGroupNameResponse'

instance Lude.ToHeaders UpdateUserHierarchyGroupName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserHierarchyGroupName where
  toJSON UpdateUserHierarchyGroupName' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath UpdateUserHierarchyGroupName where
  toPath UpdateUserHierarchyGroupName' {..} =
    Lude.mconcat
      [ "/user-hierarchy-groups/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS hierarchyGroupId,
        "/name"
      ]

instance Lude.ToQuery UpdateUserHierarchyGroupName where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserHierarchyGroupNameResponse' smart constructor.
data UpdateUserHierarchyGroupNameResponse = UpdateUserHierarchyGroupNameResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserHierarchyGroupNameResponse' with the minimum fields required to make a request.
mkUpdateUserHierarchyGroupNameResponse ::
  UpdateUserHierarchyGroupNameResponse
mkUpdateUserHierarchyGroupNameResponse =
  UpdateUserHierarchyGroupNameResponse'
