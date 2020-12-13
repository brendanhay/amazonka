{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified hierarchy group to the specified user.
module Network.AWS.Connect.UpdateUserHierarchy
  ( -- * Creating a request
    UpdateUserHierarchy (..),
    mkUpdateUserHierarchy,

    -- ** Request lenses
    uuhInstanceId,
    uuhUserId,
    uuhHierarchyGroupId,

    -- * Destructuring the response
    UpdateUserHierarchyResponse (..),
    mkUpdateUserHierarchyResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserHierarchy' smart constructor.
data UpdateUserHierarchy = UpdateUserHierarchy'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the user account.
    userId :: Lude.Text,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserHierarchy' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'userId' - The identifier of the user account.
-- * 'hierarchyGroupId' - The identifier of the hierarchy group.
mkUpdateUserHierarchy ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  UpdateUserHierarchy
mkUpdateUserHierarchy pInstanceId_ pUserId_ =
  UpdateUserHierarchy'
    { instanceId = pInstanceId_,
      userId = pUserId_,
      hierarchyGroupId = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhInstanceId :: Lens.Lens' UpdateUserHierarchy Lude.Text
uuhInstanceId = Lens.lens (instanceId :: UpdateUserHierarchy -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserHierarchy)
{-# DEPRECATED uuhInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhUserId :: Lens.Lens' UpdateUserHierarchy Lude.Text
uuhUserId = Lens.lens (userId :: UpdateUserHierarchy -> Lude.Text) (\s a -> s {userId = a} :: UpdateUserHierarchy)
{-# DEPRECATED uuhUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhHierarchyGroupId :: Lens.Lens' UpdateUserHierarchy (Lude.Maybe Lude.Text)
uuhHierarchyGroupId = Lens.lens (hierarchyGroupId :: UpdateUserHierarchy -> Lude.Maybe Lude.Text) (\s a -> s {hierarchyGroupId = a} :: UpdateUserHierarchy)
{-# DEPRECATED uuhHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

instance Lude.AWSRequest UpdateUserHierarchy where
  type Rs UpdateUserHierarchy = UpdateUserHierarchyResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserHierarchyResponse'

instance Lude.ToHeaders UpdateUserHierarchy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserHierarchy where
  toJSON UpdateUserHierarchy' {..} =
    Lude.object
      ( Lude.catMaybes
          [("HierarchyGroupId" Lude..=) Lude.<$> hierarchyGroupId]
      )

instance Lude.ToPath UpdateUserHierarchy where
  toPath UpdateUserHierarchy' {..} =
    Lude.mconcat
      [ "/users/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS userId,
        "/hierarchy"
      ]

instance Lude.ToQuery UpdateUserHierarchy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserHierarchyResponse' smart constructor.
data UpdateUserHierarchyResponse = UpdateUserHierarchyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserHierarchyResponse' with the minimum fields required to make a request.
mkUpdateUserHierarchyResponse ::
  UpdateUserHierarchyResponse
mkUpdateUserHierarchyResponse = UpdateUserHierarchyResponse'
