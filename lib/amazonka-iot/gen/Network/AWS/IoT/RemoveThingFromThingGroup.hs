{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RemoveThingFromThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the specified thing from the specified group.
--
-- You must specify either a @thingGroupArn@ or a @thingGroupName@ to identify the thing group and either a @thingArn@ or a @thingName@ to identify the thing to remove from the thing group.
module Network.AWS.IoT.RemoveThingFromThingGroup
  ( -- * Creating a request
    RemoveThingFromThingGroup (..),
    mkRemoveThingFromThingGroup,

    -- ** Request lenses
    rtftgThingGroupARN,
    rtftgThingARN,
    rtftgThingGroupName,
    rtftgThingName,

    -- * Destructuring the response
    RemoveThingFromThingGroupResponse (..),
    mkRemoveThingFromThingGroupResponse,

    -- ** Response lenses
    rtftgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveThingFromThingGroup' smart constructor.
data RemoveThingFromThingGroup = RemoveThingFromThingGroup'
  { thingGroupARN ::
      Lude.Maybe Lude.Text,
    thingARN :: Lude.Maybe Lude.Text,
    thingGroupName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RemoveThingFromThingGroup' with the minimum fields required to make a request.
--
-- * 'thingARN' - The ARN of the thing to remove from the group.
-- * 'thingGroupARN' - The group ARN.
-- * 'thingGroupName' - The group name.
-- * 'thingName' - The name of the thing to remove from the group.
mkRemoveThingFromThingGroup ::
  RemoveThingFromThingGroup
mkRemoveThingFromThingGroup =
  RemoveThingFromThingGroup'
    { thingGroupARN = Lude.Nothing,
      thingARN = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The group ARN.
--
-- /Note:/ Consider using 'thingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingGroupARN :: Lens.Lens' RemoveThingFromThingGroup (Lude.Maybe Lude.Text)
rtftgThingGroupARN = Lens.lens (thingGroupARN :: RemoveThingFromThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupARN = a} :: RemoveThingFromThingGroup)
{-# DEPRECATED rtftgThingGroupARN "Use generic-lens or generic-optics with 'thingGroupARN' instead." #-}

-- | The ARN of the thing to remove from the group.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingARN :: Lens.Lens' RemoveThingFromThingGroup (Lude.Maybe Lude.Text)
rtftgThingARN = Lens.lens (thingARN :: RemoveThingFromThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: RemoveThingFromThingGroup)
{-# DEPRECATED rtftgThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | The group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingGroupName :: Lens.Lens' RemoveThingFromThingGroup (Lude.Maybe Lude.Text)
rtftgThingGroupName = Lens.lens (thingGroupName :: RemoveThingFromThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: RemoveThingFromThingGroup)
{-# DEPRECATED rtftgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The name of the thing to remove from the group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgThingName :: Lens.Lens' RemoveThingFromThingGroup (Lude.Maybe Lude.Text)
rtftgThingName = Lens.lens (thingName :: RemoveThingFromThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: RemoveThingFromThingGroup)
{-# DEPRECATED rtftgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest RemoveThingFromThingGroup where
  type
    Rs RemoveThingFromThingGroup =
      RemoveThingFromThingGroupResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveThingFromThingGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveThingFromThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RemoveThingFromThingGroup where
  toJSON RemoveThingFromThingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingGroupArn" Lude..=) Lude.<$> thingGroupARN,
            ("thingArn" Lude..=) Lude.<$> thingARN,
            ("thingGroupName" Lude..=) Lude.<$> thingGroupName,
            ("thingName" Lude..=) Lude.<$> thingName
          ]
      )

instance Lude.ToPath RemoveThingFromThingGroup where
  toPath = Lude.const "/thing-groups/removeThingFromThingGroup"

instance Lude.ToQuery RemoveThingFromThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveThingFromThingGroupResponse' smart constructor.
newtype RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse'
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

-- | Creates a value of 'RemoveThingFromThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveThingFromThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveThingFromThingGroupResponse
mkRemoveThingFromThingGroupResponse pResponseStatus_ =
  RemoveThingFromThingGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtftgrsResponseStatus :: Lens.Lens' RemoveThingFromThingGroupResponse Lude.Int
rtftgrsResponseStatus = Lens.lens (responseStatus :: RemoveThingFromThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveThingFromThingGroupResponse)
{-# DEPRECATED rtftgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
