{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dynamic thing group.
module Network.AWS.IoT.UpdateDynamicThingGroup
  ( -- * Creating a request
    UpdateDynamicThingGroup (..),
    mkUpdateDynamicThingGroup,

    -- ** Request lenses
    udtgQueryVersion,
    udtgExpectedVersion,
    udtgThingGroupName,
    udtgQueryString,
    udtgThingGroupProperties,
    udtgIndexName,

    -- * Destructuring the response
    UpdateDynamicThingGroupResponse (..),
    mkUpdateDynamicThingGroupResponse,

    -- ** Response lenses
    udtgrsVersion,
    udtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDynamicThingGroup' smart constructor.
data UpdateDynamicThingGroup = UpdateDynamicThingGroup'
  { -- | The dynamic thing group query version to update.
    queryVersion :: Lude.Maybe Lude.Text,
    -- | The expected version of the dynamic thing group to update.
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The name of the dynamic thing group to update.
    thingGroupName :: Lude.Text,
    -- | The dynamic thing group search query string to update.
    queryString :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group properties to update.
    thingGroupProperties :: ThingGroupProperties,
    -- | The dynamic thing group index to update.
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDynamicThingGroup' with the minimum fields required to make a request.
--
-- * 'queryVersion' - The dynamic thing group query version to update.
-- * 'expectedVersion' - The expected version of the dynamic thing group to update.
-- * 'thingGroupName' - The name of the dynamic thing group to update.
-- * 'queryString' - The dynamic thing group search query string to update.
-- * 'thingGroupProperties' - The dynamic thing group properties to update.
-- * 'indexName' - The dynamic thing group index to update.
mkUpdateDynamicThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  -- | 'thingGroupProperties'
  ThingGroupProperties ->
  UpdateDynamicThingGroup
mkUpdateDynamicThingGroup pThingGroupName_ pThingGroupProperties_ =
  UpdateDynamicThingGroup'
    { queryVersion = Lude.Nothing,
      expectedVersion = Lude.Nothing,
      thingGroupName = pThingGroupName_,
      queryString = Lude.Nothing,
      thingGroupProperties = pThingGroupProperties_,
      indexName = Lude.Nothing
    }

-- | The dynamic thing group query version to update.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgQueryVersion :: Lens.Lens' UpdateDynamicThingGroup (Lude.Maybe Lude.Text)
udtgQueryVersion = Lens.lens (queryVersion :: UpdateDynamicThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: UpdateDynamicThingGroup)
{-# DEPRECATED udtgQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The expected version of the dynamic thing group to update.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgExpectedVersion :: Lens.Lens' UpdateDynamicThingGroup (Lude.Maybe Lude.Integer)
udtgExpectedVersion = Lens.lens (expectedVersion :: UpdateDynamicThingGroup -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: UpdateDynamicThingGroup)
{-# DEPRECATED udtgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the dynamic thing group to update.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgThingGroupName :: Lens.Lens' UpdateDynamicThingGroup Lude.Text
udtgThingGroupName = Lens.lens (thingGroupName :: UpdateDynamicThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: UpdateDynamicThingGroup)
{-# DEPRECATED udtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The dynamic thing group search query string to update.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgQueryString :: Lens.Lens' UpdateDynamicThingGroup (Lude.Maybe Lude.Text)
udtgQueryString = Lens.lens (queryString :: UpdateDynamicThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {queryString = a} :: UpdateDynamicThingGroup)
{-# DEPRECATED udtgQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group properties to update.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgThingGroupProperties :: Lens.Lens' UpdateDynamicThingGroup ThingGroupProperties
udtgThingGroupProperties = Lens.lens (thingGroupProperties :: UpdateDynamicThingGroup -> ThingGroupProperties) (\s a -> s {thingGroupProperties = a} :: UpdateDynamicThingGroup)
{-# DEPRECATED udtgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

-- | The dynamic thing group index to update.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgIndexName :: Lens.Lens' UpdateDynamicThingGroup (Lude.Maybe Lude.Text)
udtgIndexName = Lens.lens (indexName :: UpdateDynamicThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: UpdateDynamicThingGroup)
{-# DEPRECATED udtgIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.AWSRequest UpdateDynamicThingGroup where
  type Rs UpdateDynamicThingGroup = UpdateDynamicThingGroupResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDynamicThingGroupResponse'
            Lude.<$> (x Lude..?> "version") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDynamicThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateDynamicThingGroup where
  toJSON UpdateDynamicThingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryVersion" Lude..=) Lude.<$> queryVersion,
            ("expectedVersion" Lude..=) Lude.<$> expectedVersion,
            ("queryString" Lude..=) Lude.<$> queryString,
            Lude.Just ("thingGroupProperties" Lude..= thingGroupProperties),
            ("indexName" Lude..=) Lude.<$> indexName
          ]
      )

instance Lude.ToPath UpdateDynamicThingGroup where
  toPath UpdateDynamicThingGroup' {..} =
    Lude.mconcat ["/dynamic-thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery UpdateDynamicThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDynamicThingGroupResponse' smart constructor.
data UpdateDynamicThingGroupResponse = UpdateDynamicThingGroupResponse'
  { -- | The dynamic thing group version.
    version :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDynamicThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'version' - The dynamic thing group version.
-- * 'responseStatus' - The response status code.
mkUpdateDynamicThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDynamicThingGroupResponse
mkUpdateDynamicThingGroupResponse pResponseStatus_ =
  UpdateDynamicThingGroupResponse'
    { version = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The dynamic thing group version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgrsVersion :: Lens.Lens' UpdateDynamicThingGroupResponse (Lude.Maybe Lude.Integer)
udtgrsVersion = Lens.lens (version :: UpdateDynamicThingGroupResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: UpdateDynamicThingGroupResponse)
{-# DEPRECATED udtgrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgrsResponseStatus :: Lens.Lens' UpdateDynamicThingGroupResponse Lude.Int
udtgrsResponseStatus = Lens.lens (responseStatus :: UpdateDynamicThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDynamicThingGroupResponse)
{-# DEPRECATED udtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
