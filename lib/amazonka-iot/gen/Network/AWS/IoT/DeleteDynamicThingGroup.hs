{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dynamic thing group.
module Network.AWS.IoT.DeleteDynamicThingGroup
  ( -- * Creating a request
    DeleteDynamicThingGroup (..),
    mkDeleteDynamicThingGroup,

    -- ** Request lenses
    ddtgExpectedVersion,
    ddtgThingGroupName,

    -- * Destructuring the response
    DeleteDynamicThingGroupResponse (..),
    mkDeleteDynamicThingGroupResponse,

    -- ** Response lenses
    ddtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDynamicThingGroup' smart constructor.
data DeleteDynamicThingGroup = DeleteDynamicThingGroup'
  { -- | The expected version of the dynamic thing group to delete.
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The name of the dynamic thing group to delete.
    thingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDynamicThingGroup' with the minimum fields required to make a request.
--
-- * 'expectedVersion' - The expected version of the dynamic thing group to delete.
-- * 'thingGroupName' - The name of the dynamic thing group to delete.
mkDeleteDynamicThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  DeleteDynamicThingGroup
mkDeleteDynamicThingGroup pThingGroupName_ =
  DeleteDynamicThingGroup'
    { expectedVersion = Lude.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The expected version of the dynamic thing group to delete.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgExpectedVersion :: Lens.Lens' DeleteDynamicThingGroup (Lude.Maybe Lude.Integer)
ddtgExpectedVersion = Lens.lens (expectedVersion :: DeleteDynamicThingGroup -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: DeleteDynamicThingGroup)
{-# DEPRECATED ddtgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the dynamic thing group to delete.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgThingGroupName :: Lens.Lens' DeleteDynamicThingGroup Lude.Text
ddtgThingGroupName = Lens.lens (thingGroupName :: DeleteDynamicThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: DeleteDynamicThingGroup)
{-# DEPRECATED ddtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Lude.AWSRequest DeleteDynamicThingGroup where
  type Rs DeleteDynamicThingGroup = DeleteDynamicThingGroupResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDynamicThingGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDynamicThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDynamicThingGroup where
  toPath DeleteDynamicThingGroup' {..} =
    Lude.mconcat ["/dynamic-thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery DeleteDynamicThingGroup where
  toQuery DeleteDynamicThingGroup' {..} =
    Lude.mconcat ["expectedVersion" Lude.=: expectedVersion]

-- | /See:/ 'mkDeleteDynamicThingGroupResponse' smart constructor.
newtype DeleteDynamicThingGroupResponse = DeleteDynamicThingGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDynamicThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDynamicThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDynamicThingGroupResponse
mkDeleteDynamicThingGroupResponse pResponseStatus_ =
  DeleteDynamicThingGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgrsResponseStatus :: Lens.Lens' DeleteDynamicThingGroupResponse Lude.Int
ddtgrsResponseStatus = Lens.lens (responseStatus :: DeleteDynamicThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDynamicThingGroupResponse)
{-# DEPRECATED ddtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
