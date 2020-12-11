{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AddThingToThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a thing group.
module Network.AWS.IoT.AddThingToThingGroup
  ( -- * Creating a request
    AddThingToThingGroup (..),
    mkAddThingToThingGroup,

    -- ** Request lenses
    atttgThingGroupARN,
    atttgThingARN,
    atttgThingGroupName,
    atttgOverrideDynamicGroups,
    atttgThingName,

    -- * Destructuring the response
    AddThingToThingGroupResponse (..),
    mkAddThingToThingGroupResponse,

    -- ** Response lenses
    atttgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddThingToThingGroup' smart constructor.
data AddThingToThingGroup = AddThingToThingGroup'
  { thingGroupARN ::
      Lude.Maybe Lude.Text,
    thingARN :: Lude.Maybe Lude.Text,
    thingGroupName :: Lude.Maybe Lude.Text,
    overrideDynamicGroups :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'AddThingToThingGroup' with the minimum fields required to make a request.
--
-- * 'overrideDynamicGroups' - Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
-- * 'thingARN' - The ARN of the thing to add to a group.
-- * 'thingGroupARN' - The ARN of the group to which you are adding a thing.
-- * 'thingGroupName' - The name of the group to which you are adding a thing.
-- * 'thingName' - The name of the thing to add to a group.
mkAddThingToThingGroup ::
  AddThingToThingGroup
mkAddThingToThingGroup =
  AddThingToThingGroup'
    { thingGroupARN = Lude.Nothing,
      thingARN = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      overrideDynamicGroups = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The ARN of the group to which you are adding a thing.
--
-- /Note:/ Consider using 'thingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingGroupARN :: Lens.Lens' AddThingToThingGroup (Lude.Maybe Lude.Text)
atttgThingGroupARN = Lens.lens (thingGroupARN :: AddThingToThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupARN = a} :: AddThingToThingGroup)
{-# DEPRECATED atttgThingGroupARN "Use generic-lens or generic-optics with 'thingGroupARN' instead." #-}

-- | The ARN of the thing to add to a group.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingARN :: Lens.Lens' AddThingToThingGroup (Lude.Maybe Lude.Text)
atttgThingARN = Lens.lens (thingARN :: AddThingToThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: AddThingToThingGroup)
{-# DEPRECATED atttgThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | The name of the group to which you are adding a thing.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingGroupName :: Lens.Lens' AddThingToThingGroup (Lude.Maybe Lude.Text)
atttgThingGroupName = Lens.lens (thingGroupName :: AddThingToThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: AddThingToThingGroup)
{-# DEPRECATED atttgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
--
-- /Note:/ Consider using 'overrideDynamicGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgOverrideDynamicGroups :: Lens.Lens' AddThingToThingGroup (Lude.Maybe Lude.Bool)
atttgOverrideDynamicGroups = Lens.lens (overrideDynamicGroups :: AddThingToThingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {overrideDynamicGroups = a} :: AddThingToThingGroup)
{-# DEPRECATED atttgOverrideDynamicGroups "Use generic-lens or generic-optics with 'overrideDynamicGroups' instead." #-}

-- | The name of the thing to add to a group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingName :: Lens.Lens' AddThingToThingGroup (Lude.Maybe Lude.Text)
atttgThingName = Lens.lens (thingName :: AddThingToThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: AddThingToThingGroup)
{-# DEPRECATED atttgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest AddThingToThingGroup where
  type Rs AddThingToThingGroup = AddThingToThingGroupResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddThingToThingGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddThingToThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AddThingToThingGroup where
  toJSON AddThingToThingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingGroupArn" Lude..=) Lude.<$> thingGroupARN,
            ("thingArn" Lude..=) Lude.<$> thingARN,
            ("thingGroupName" Lude..=) Lude.<$> thingGroupName,
            ("overrideDynamicGroups" Lude..=) Lude.<$> overrideDynamicGroups,
            ("thingName" Lude..=) Lude.<$> thingName
          ]
      )

instance Lude.ToPath AddThingToThingGroup where
  toPath = Lude.const "/thing-groups/addThingToThingGroup"

instance Lude.ToQuery AddThingToThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddThingToThingGroupResponse' smart constructor.
newtype AddThingToThingGroupResponse = AddThingToThingGroupResponse'
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

-- | Creates a value of 'AddThingToThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddThingToThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddThingToThingGroupResponse
mkAddThingToThingGroupResponse pResponseStatus_ =
  AddThingToThingGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgrsResponseStatus :: Lens.Lens' AddThingToThingGroupResponse Lude.Int
atttgrsResponseStatus = Lens.lens (responseStatus :: AddThingToThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddThingToThingGroupResponse)
{-# DEPRECATED atttgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
