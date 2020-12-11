{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a thing group.
module Network.AWS.IoT.DeleteThingGroup
  ( -- * Creating a request
    DeleteThingGroup (..),
    mkDeleteThingGroup,

    -- ** Request lenses
    dExpectedVersion,
    dThingGroupName,

    -- * Destructuring the response
    DeleteThingGroupResponse (..),
    mkDeleteThingGroupResponse,

    -- ** Response lenses
    dtgtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteThingGroup' smart constructor.
data DeleteThingGroup = DeleteThingGroup'
  { expectedVersion ::
      Lude.Maybe Lude.Integer,
    thingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingGroup' with the minimum fields required to make a request.
--
-- * 'expectedVersion' - The expected version of the thing group to delete.
-- * 'thingGroupName' - The name of the thing group to delete.
mkDeleteThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  DeleteThingGroup
mkDeleteThingGroup pThingGroupName_ =
  DeleteThingGroup'
    { expectedVersion = Lude.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The expected version of the thing group to delete.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpectedVersion :: Lens.Lens' DeleteThingGroup (Lude.Maybe Lude.Integer)
dExpectedVersion = Lens.lens (expectedVersion :: DeleteThingGroup -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: DeleteThingGroup)
{-# DEPRECATED dExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the thing group to delete.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingGroupName :: Lens.Lens' DeleteThingGroup Lude.Text
dThingGroupName = Lens.lens (thingGroupName :: DeleteThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: DeleteThingGroup)
{-# DEPRECATED dThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Lude.AWSRequest DeleteThingGroup where
  type Rs DeleteThingGroup = DeleteThingGroupResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteThingGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteThingGroup where
  toPath DeleteThingGroup' {..} =
    Lude.mconcat ["/thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery DeleteThingGroup where
  toQuery DeleteThingGroup' {..} =
    Lude.mconcat ["expectedVersion" Lude.=: expectedVersion]

-- | /See:/ 'mkDeleteThingGroupResponse' smart constructor.
newtype DeleteThingGroupResponse = DeleteThingGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteThingGroupResponse
mkDeleteThingGroupResponse pResponseStatus_ =
  DeleteThingGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgtrsResponseStatus :: Lens.Lens' DeleteThingGroupResponse Lude.Int
dtgtrsResponseStatus = Lens.lens (responseStatus :: DeleteThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteThingGroupResponse)
{-# DEPRECATED dtgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
