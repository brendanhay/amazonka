{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dynamic thing group.
module Network.AWS.IoT.CreateDynamicThingGroup
  ( -- * Creating a request
    CreateDynamicThingGroup (..),
    mkCreateDynamicThingGroup,

    -- ** Request lenses
    cdtgQueryVersion,
    cdtgThingGroupName,
    cdtgQueryString,
    cdtgThingGroupProperties,
    cdtgIndexName,
    cdtgTags,

    -- * Destructuring the response
    CreateDynamicThingGroupResponse (..),
    mkCreateDynamicThingGroupResponse,

    -- ** Response lenses
    cdtgrsQueryVersion,
    cdtgrsThingGroupARN,
    cdtgrsThingGroupId,
    cdtgrsThingGroupName,
    cdtgrsQueryString,
    cdtgrsIndexName,
    cdtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDynamicThingGroup' smart constructor.
data CreateDynamicThingGroup = CreateDynamicThingGroup'
  { -- | The dynamic thing group query version.
    queryVersion :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group name to create.
    thingGroupName :: Lude.Text,
    -- | The dynamic thing group search query string.
    --
    -- See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
    queryString :: Lude.Text,
    -- | The dynamic thing group properties.
    thingGroupProperties :: Lude.Maybe ThingGroupProperties,
    -- | The dynamic thing group index name.
    indexName :: Lude.Maybe Lude.Text,
    -- | Metadata which can be used to manage the dynamic thing group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDynamicThingGroup' with the minimum fields required to make a request.
--
-- * 'queryVersion' - The dynamic thing group query version.
-- * 'thingGroupName' - The dynamic thing group name to create.
-- * 'queryString' - The dynamic thing group search query string.
--
-- See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
-- * 'thingGroupProperties' - The dynamic thing group properties.
-- * 'indexName' - The dynamic thing group index name.
-- * 'tags' - Metadata which can be used to manage the dynamic thing group.
mkCreateDynamicThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  -- | 'queryString'
  Lude.Text ->
  CreateDynamicThingGroup
mkCreateDynamicThingGroup pThingGroupName_ pQueryString_ =
  CreateDynamicThingGroup'
    { queryVersion = Lude.Nothing,
      thingGroupName = pThingGroupName_,
      queryString = pQueryString_,
      thingGroupProperties = Lude.Nothing,
      indexName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The dynamic thing group query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgQueryVersion :: Lens.Lens' CreateDynamicThingGroup (Lude.Maybe Lude.Text)
cdtgQueryVersion = Lens.lens (queryVersion :: CreateDynamicThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: CreateDynamicThingGroup)
{-# DEPRECATED cdtgQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The dynamic thing group name to create.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgThingGroupName :: Lens.Lens' CreateDynamicThingGroup Lude.Text
cdtgThingGroupName = Lens.lens (thingGroupName :: CreateDynamicThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: CreateDynamicThingGroup)
{-# DEPRECATED cdtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The dynamic thing group search query string.
--
-- See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgQueryString :: Lens.Lens' CreateDynamicThingGroup Lude.Text
cdtgQueryString = Lens.lens (queryString :: CreateDynamicThingGroup -> Lude.Text) (\s a -> s {queryString = a} :: CreateDynamicThingGroup)
{-# DEPRECATED cdtgQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgThingGroupProperties :: Lens.Lens' CreateDynamicThingGroup (Lude.Maybe ThingGroupProperties)
cdtgThingGroupProperties = Lens.lens (thingGroupProperties :: CreateDynamicThingGroup -> Lude.Maybe ThingGroupProperties) (\s a -> s {thingGroupProperties = a} :: CreateDynamicThingGroup)
{-# DEPRECATED cdtgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

-- | The dynamic thing group index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgIndexName :: Lens.Lens' CreateDynamicThingGroup (Lude.Maybe Lude.Text)
cdtgIndexName = Lens.lens (indexName :: CreateDynamicThingGroup -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: CreateDynamicThingGroup)
{-# DEPRECATED cdtgIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | Metadata which can be used to manage the dynamic thing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgTags :: Lens.Lens' CreateDynamicThingGroup (Lude.Maybe [Tag])
cdtgTags = Lens.lens (tags :: CreateDynamicThingGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDynamicThingGroup)
{-# DEPRECATED cdtgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDynamicThingGroup where
  type Rs CreateDynamicThingGroup = CreateDynamicThingGroupResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDynamicThingGroupResponse'
            Lude.<$> (x Lude..?> "queryVersion")
            Lude.<*> (x Lude..?> "thingGroupArn")
            Lude.<*> (x Lude..?> "thingGroupId")
            Lude.<*> (x Lude..?> "thingGroupName")
            Lude.<*> (x Lude..?> "queryString")
            Lude.<*> (x Lude..?> "indexName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDynamicThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateDynamicThingGroup where
  toJSON CreateDynamicThingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("queryVersion" Lude..=) Lude.<$> queryVersion,
            Lude.Just ("queryString" Lude..= queryString),
            ("thingGroupProperties" Lude..=) Lude.<$> thingGroupProperties,
            ("indexName" Lude..=) Lude.<$> indexName,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDynamicThingGroup where
  toPath CreateDynamicThingGroup' {..} =
    Lude.mconcat ["/dynamic-thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery CreateDynamicThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDynamicThingGroupResponse' smart constructor.
data CreateDynamicThingGroupResponse = CreateDynamicThingGroupResponse'
  { -- | The dynamic thing group query version.
    queryVersion :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group ARN.
    thingGroupARN :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group ID.
    thingGroupId :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group name.
    thingGroupName :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group search query string.
    queryString :: Lude.Maybe Lude.Text,
    -- | The dynamic thing group index name.
    indexName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDynamicThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'queryVersion' - The dynamic thing group query version.
-- * 'thingGroupARN' - The dynamic thing group ARN.
-- * 'thingGroupId' - The dynamic thing group ID.
-- * 'thingGroupName' - The dynamic thing group name.
-- * 'queryString' - The dynamic thing group search query string.
-- * 'indexName' - The dynamic thing group index name.
-- * 'responseStatus' - The response status code.
mkCreateDynamicThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDynamicThingGroupResponse
mkCreateDynamicThingGroupResponse pResponseStatus_ =
  CreateDynamicThingGroupResponse'
    { queryVersion = Lude.Nothing,
      thingGroupARN = Lude.Nothing,
      thingGroupId = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      queryString = Lude.Nothing,
      indexName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The dynamic thing group query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsQueryVersion :: Lens.Lens' CreateDynamicThingGroupResponse (Lude.Maybe Lude.Text)
cdtgrsQueryVersion = Lens.lens (queryVersion :: CreateDynamicThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The dynamic thing group ARN.
--
-- /Note:/ Consider using 'thingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsThingGroupARN :: Lens.Lens' CreateDynamicThingGroupResponse (Lude.Maybe Lude.Text)
cdtgrsThingGroupARN = Lens.lens (thingGroupARN :: CreateDynamicThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupARN = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsThingGroupARN "Use generic-lens or generic-optics with 'thingGroupARN' instead." #-}

-- | The dynamic thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsThingGroupId :: Lens.Lens' CreateDynamicThingGroupResponse (Lude.Maybe Lude.Text)
cdtgrsThingGroupId = Lens.lens (thingGroupId :: CreateDynamicThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupId = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The dynamic thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsThingGroupName :: Lens.Lens' CreateDynamicThingGroupResponse (Lude.Maybe Lude.Text)
cdtgrsThingGroupName = Lens.lens (thingGroupName :: CreateDynamicThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The dynamic thing group search query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsQueryString :: Lens.Lens' CreateDynamicThingGroupResponse (Lude.Maybe Lude.Text)
cdtgrsQueryString = Lens.lens (queryString :: CreateDynamicThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryString = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsIndexName :: Lens.Lens' CreateDynamicThingGroupResponse (Lude.Maybe Lude.Text)
cdtgrsIndexName = Lens.lens (indexName :: CreateDynamicThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrsResponseStatus :: Lens.Lens' CreateDynamicThingGroupResponse Lude.Int
cdtgrsResponseStatus = Lens.lens (responseStatus :: CreateDynamicThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDynamicThingGroupResponse)
{-# DEPRECATED cdtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
