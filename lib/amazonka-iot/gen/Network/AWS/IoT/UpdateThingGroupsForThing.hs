{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateThingGroupsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the groups to which the thing belongs.
module Network.AWS.IoT.UpdateThingGroupsForThing
  ( -- * Creating a request
    UpdateThingGroupsForThing (..),
    mkUpdateThingGroupsForThing,

    -- ** Request lenses
    utgftThingGroupsToAdd,
    utgftThingGroupsToRemove,
    utgftOverrideDynamicGroups,
    utgftThingName,

    -- * Destructuring the response
    UpdateThingGroupsForThingResponse (..),
    mkUpdateThingGroupsForThingResponse,

    -- ** Response lenses
    utgftrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateThingGroupsForThing' smart constructor.
data UpdateThingGroupsForThing = UpdateThingGroupsForThing'
  { thingGroupsToAdd ::
      Lude.Maybe [Lude.Text],
    thingGroupsToRemove ::
      Lude.Maybe [Lude.Text],
    overrideDynamicGroups ::
      Lude.Maybe Lude.Bool,
    thingName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingGroupsForThing' with the minimum fields required to make a request.
--
-- * 'overrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
-- * 'thingGroupsToAdd' - The groups to which the thing will be added.
-- * 'thingGroupsToRemove' - The groups from which the thing will be removed.
-- * 'thingName' - The thing whose group memberships will be updated.
mkUpdateThingGroupsForThing ::
  UpdateThingGroupsForThing
mkUpdateThingGroupsForThing =
  UpdateThingGroupsForThing'
    { thingGroupsToAdd = Lude.Nothing,
      thingGroupsToRemove = Lude.Nothing,
      overrideDynamicGroups = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The groups to which the thing will be added.
--
-- /Note:/ Consider using 'thingGroupsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgftThingGroupsToAdd :: Lens.Lens' UpdateThingGroupsForThing (Lude.Maybe [Lude.Text])
utgftThingGroupsToAdd = Lens.lens (thingGroupsToAdd :: UpdateThingGroupsForThing -> Lude.Maybe [Lude.Text]) (\s a -> s {thingGroupsToAdd = a} :: UpdateThingGroupsForThing)
{-# DEPRECATED utgftThingGroupsToAdd "Use generic-lens or generic-optics with 'thingGroupsToAdd' instead." #-}

-- | The groups from which the thing will be removed.
--
-- /Note:/ Consider using 'thingGroupsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgftThingGroupsToRemove :: Lens.Lens' UpdateThingGroupsForThing (Lude.Maybe [Lude.Text])
utgftThingGroupsToRemove = Lens.lens (thingGroupsToRemove :: UpdateThingGroupsForThing -> Lude.Maybe [Lude.Text]) (\s a -> s {thingGroupsToRemove = a} :: UpdateThingGroupsForThing)
{-# DEPRECATED utgftThingGroupsToRemove "Use generic-lens or generic-optics with 'thingGroupsToRemove' instead." #-}

-- | Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
--
-- /Note:/ Consider using 'overrideDynamicGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgftOverrideDynamicGroups :: Lens.Lens' UpdateThingGroupsForThing (Lude.Maybe Lude.Bool)
utgftOverrideDynamicGroups = Lens.lens (overrideDynamicGroups :: UpdateThingGroupsForThing -> Lude.Maybe Lude.Bool) (\s a -> s {overrideDynamicGroups = a} :: UpdateThingGroupsForThing)
{-# DEPRECATED utgftOverrideDynamicGroups "Use generic-lens or generic-optics with 'overrideDynamicGroups' instead." #-}

-- | The thing whose group memberships will be updated.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgftThingName :: Lens.Lens' UpdateThingGroupsForThing (Lude.Maybe Lude.Text)
utgftThingName = Lens.lens (thingName :: UpdateThingGroupsForThing -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: UpdateThingGroupsForThing)
{-# DEPRECATED utgftThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest UpdateThingGroupsForThing where
  type
    Rs UpdateThingGroupsForThing =
      UpdateThingGroupsForThingResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateThingGroupsForThingResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateThingGroupsForThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateThingGroupsForThing where
  toJSON UpdateThingGroupsForThing' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingGroupsToAdd" Lude..=) Lude.<$> thingGroupsToAdd,
            ("thingGroupsToRemove" Lude..=) Lude.<$> thingGroupsToRemove,
            ("overrideDynamicGroups" Lude..=) Lude.<$> overrideDynamicGroups,
            ("thingName" Lude..=) Lude.<$> thingName
          ]
      )

instance Lude.ToPath UpdateThingGroupsForThing where
  toPath = Lude.const "/thing-groups/updateThingGroupsForThing"

instance Lude.ToQuery UpdateThingGroupsForThing where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateThingGroupsForThingResponse' smart constructor.
newtype UpdateThingGroupsForThingResponse = UpdateThingGroupsForThingResponse'
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

-- | Creates a value of 'UpdateThingGroupsForThingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateThingGroupsForThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateThingGroupsForThingResponse
mkUpdateThingGroupsForThingResponse pResponseStatus_ =
  UpdateThingGroupsForThingResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgftrsResponseStatus :: Lens.Lens' UpdateThingGroupsForThingResponse Lude.Int
utgftrsResponseStatus = Lens.lens (responseStatus :: UpdateThingGroupsForThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateThingGroupsForThingResponse)
{-# DEPRECATED utgftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
