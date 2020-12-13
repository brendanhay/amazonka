{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a thing group.
module Network.AWS.IoT.CreateThingGroup
  ( -- * Creating a request
    CreateThingGroup (..),
    mkCreateThingGroup,

    -- ** Request lenses
    ctgParentGroupName,
    ctgThingGroupName,
    ctgThingGroupProperties,
    ctgTags,

    -- * Destructuring the response
    CreateThingGroupResponse (..),
    mkCreateThingGroupResponse,

    -- ** Response lenses
    ctgrsThingGroupARN,
    ctgrsThingGroupId,
    ctgrsThingGroupName,
    ctgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateThingGroup' smart constructor.
data CreateThingGroup = CreateThingGroup'
  { -- | The name of the parent thing group.
    parentGroupName :: Lude.Maybe Lude.Text,
    -- | The thing group name to create.
    thingGroupName :: Lude.Text,
    -- | The thing group properties.
    thingGroupProperties :: Lude.Maybe ThingGroupProperties,
    -- | Metadata which can be used to manage the thing group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateThingGroup' with the minimum fields required to make a request.
--
-- * 'parentGroupName' - The name of the parent thing group.
-- * 'thingGroupName' - The thing group name to create.
-- * 'thingGroupProperties' - The thing group properties.
-- * 'tags' - Metadata which can be used to manage the thing group.
mkCreateThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  CreateThingGroup
mkCreateThingGroup pThingGroupName_ =
  CreateThingGroup'
    { parentGroupName = Lude.Nothing,
      thingGroupName = pThingGroupName_,
      thingGroupProperties = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the parent thing group.
--
-- /Note:/ Consider using 'parentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgParentGroupName :: Lens.Lens' CreateThingGroup (Lude.Maybe Lude.Text)
ctgParentGroupName = Lens.lens (parentGroupName :: CreateThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {parentGroupName = a} :: CreateThingGroup)
{-# DEPRECATED ctgParentGroupName "Use generic-lens or generic-optics with 'parentGroupName' instead." #-}

-- | The thing group name to create.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgThingGroupName :: Lens.Lens' CreateThingGroup Lude.Text
ctgThingGroupName = Lens.lens (thingGroupName :: CreateThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: CreateThingGroup)
{-# DEPRECATED ctgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgThingGroupProperties :: Lens.Lens' CreateThingGroup (Lude.Maybe ThingGroupProperties)
ctgThingGroupProperties = Lens.lens (thingGroupProperties :: CreateThingGroup -> Lude.Maybe ThingGroupProperties) (\s a -> s {thingGroupProperties = a} :: CreateThingGroup)
{-# DEPRECATED ctgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

-- | Metadata which can be used to manage the thing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTags :: Lens.Lens' CreateThingGroup (Lude.Maybe [Tag])
ctgTags = Lens.lens (tags :: CreateThingGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateThingGroup)
{-# DEPRECATED ctgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateThingGroup where
  type Rs CreateThingGroup = CreateThingGroupResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateThingGroupResponse'
            Lude.<$> (x Lude..?> "thingGroupArn")
            Lude.<*> (x Lude..?> "thingGroupId")
            Lude.<*> (x Lude..?> "thingGroupName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateThingGroup where
  toJSON CreateThingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("parentGroupName" Lude..=) Lude.<$> parentGroupName,
            ("thingGroupProperties" Lude..=) Lude.<$> thingGroupProperties,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateThingGroup where
  toPath CreateThingGroup' {..} =
    Lude.mconcat ["/thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery CreateThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { -- | The thing group ARN.
    thingGroupARN :: Lude.Maybe Lude.Text,
    -- | The thing group ID.
    thingGroupId :: Lude.Maybe Lude.Text,
    -- | The thing group name.
    thingGroupName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'thingGroupARN' - The thing group ARN.
-- * 'thingGroupId' - The thing group ID.
-- * 'thingGroupName' - The thing group name.
-- * 'responseStatus' - The response status code.
mkCreateThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateThingGroupResponse
mkCreateThingGroupResponse pResponseStatus_ =
  CreateThingGroupResponse'
    { thingGroupARN = Lude.Nothing,
      thingGroupId = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The thing group ARN.
--
-- /Note:/ Consider using 'thingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsThingGroupARN :: Lens.Lens' CreateThingGroupResponse (Lude.Maybe Lude.Text)
ctgrsThingGroupARN = Lens.lens (thingGroupARN :: CreateThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupARN = a} :: CreateThingGroupResponse)
{-# DEPRECATED ctgrsThingGroupARN "Use generic-lens or generic-optics with 'thingGroupARN' instead." #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsThingGroupId :: Lens.Lens' CreateThingGroupResponse (Lude.Maybe Lude.Text)
ctgrsThingGroupId = Lens.lens (thingGroupId :: CreateThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupId = a} :: CreateThingGroupResponse)
{-# DEPRECATED ctgrsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsThingGroupName :: Lens.Lens' CreateThingGroupResponse (Lude.Maybe Lude.Text)
ctgrsThingGroupName = Lens.lens (thingGroupName :: CreateThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: CreateThingGroupResponse)
{-# DEPRECATED ctgrsThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsResponseStatus :: Lens.Lens' CreateThingGroupResponse Lude.Int
ctgrsResponseStatus = Lens.lens (responseStatus :: CreateThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateThingGroupResponse)
{-# DEPRECATED ctgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
