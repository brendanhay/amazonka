{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a thing group.
module Network.AWS.IoT.UpdateThingGroup
  ( -- * Creating a request
    UpdateThingGroup (..),
    mkUpdateThingGroup,

    -- ** Request lenses
    utgExpectedVersion,
    utgThingGroupName,
    utgThingGroupProperties,

    -- * Destructuring the response
    UpdateThingGroupResponse (..),
    mkUpdateThingGroupResponse,

    -- ** Response lenses
    utgrsVersion,
    utgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateThingGroup' smart constructor.
data UpdateThingGroup = UpdateThingGroup'
  { -- | The expected version of the thing group. If this does not match the version of the thing group being updated, the update will fail.
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The thing group to update.
    thingGroupName :: Lude.Text,
    -- | The thing group properties.
    thingGroupProperties :: ThingGroupProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingGroup' with the minimum fields required to make a request.
--
-- * 'expectedVersion' - The expected version of the thing group. If this does not match the version of the thing group being updated, the update will fail.
-- * 'thingGroupName' - The thing group to update.
-- * 'thingGroupProperties' - The thing group properties.
mkUpdateThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  -- | 'thingGroupProperties'
  ThingGroupProperties ->
  UpdateThingGroup
mkUpdateThingGroup pThingGroupName_ pThingGroupProperties_ =
  UpdateThingGroup'
    { expectedVersion = Lude.Nothing,
      thingGroupName = pThingGroupName_,
      thingGroupProperties = pThingGroupProperties_
    }

-- | The expected version of the thing group. If this does not match the version of the thing group being updated, the update will fail.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgExpectedVersion :: Lens.Lens' UpdateThingGroup (Lude.Maybe Lude.Integer)
utgExpectedVersion = Lens.lens (expectedVersion :: UpdateThingGroup -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: UpdateThingGroup)
{-# DEPRECATED utgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The thing group to update.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgThingGroupName :: Lens.Lens' UpdateThingGroup Lude.Text
utgThingGroupName = Lens.lens (thingGroupName :: UpdateThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: UpdateThingGroup)
{-# DEPRECATED utgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgThingGroupProperties :: Lens.Lens' UpdateThingGroup ThingGroupProperties
utgThingGroupProperties = Lens.lens (thingGroupProperties :: UpdateThingGroup -> ThingGroupProperties) (\s a -> s {thingGroupProperties = a} :: UpdateThingGroup)
{-# DEPRECATED utgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

instance Lude.AWSRequest UpdateThingGroup where
  type Rs UpdateThingGroup = UpdateThingGroupResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateThingGroupResponse'
            Lude.<$> (x Lude..?> "version") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateThingGroup where
  toJSON UpdateThingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expectedVersion" Lude..=) Lude.<$> expectedVersion,
            Lude.Just ("thingGroupProperties" Lude..= thingGroupProperties)
          ]
      )

instance Lude.ToPath UpdateThingGroup where
  toPath UpdateThingGroup' {..} =
    Lude.mconcat ["/thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery UpdateThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateThingGroupResponse' smart constructor.
data UpdateThingGroupResponse = UpdateThingGroupResponse'
  { -- | The version of the updated thing group.
    version :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'version' - The version of the updated thing group.
-- * 'responseStatus' - The response status code.
mkUpdateThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateThingGroupResponse
mkUpdateThingGroupResponse pResponseStatus_ =
  UpdateThingGroupResponse'
    { version = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The version of the updated thing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgrsVersion :: Lens.Lens' UpdateThingGroupResponse (Lude.Maybe Lude.Integer)
utgrsVersion = Lens.lens (version :: UpdateThingGroupResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: UpdateThingGroupResponse)
{-# DEPRECATED utgrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgrsResponseStatus :: Lens.Lens' UpdateThingGroupResponse Lude.Int
utgrsResponseStatus = Lens.lens (responseStatus :: UpdateThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateThingGroupResponse)
{-# DEPRECATED utgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
