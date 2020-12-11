{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a thing group.
module Network.AWS.IoT.DescribeThingGroup
  ( -- * Creating a request
    DescribeThingGroup (..),
    mkDescribeThingGroup,

    -- ** Request lenses
    dtgThingGroupName,

    -- * Destructuring the response
    DescribeThingGroupResponse (..),
    mkDescribeThingGroupResponse,

    -- ** Response lenses
    dtgrsStatus,
    dtgrsQueryVersion,
    dtgrsThingGroupARN,
    dtgrsThingGroupId,
    dtgrsThingGroupMetadata,
    dtgrsThingGroupName,
    dtgrsQueryString,
    dtgrsVersion,
    dtgrsThingGroupProperties,
    dtgrsIndexName,
    dtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeThingGroup' smart constructor.
newtype DescribeThingGroup = DescribeThingGroup'
  { thingGroupName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingGroup' with the minimum fields required to make a request.
--
-- * 'thingGroupName' - The name of the thing group.
mkDescribeThingGroup ::
  -- | 'thingGroupName'
  Lude.Text ->
  DescribeThingGroup
mkDescribeThingGroup pThingGroupName_ =
  DescribeThingGroup' {thingGroupName = pThingGroupName_}

-- | The name of the thing group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgThingGroupName :: Lens.Lens' DescribeThingGroup Lude.Text
dtgThingGroupName = Lens.lens (thingGroupName :: DescribeThingGroup -> Lude.Text) (\s a -> s {thingGroupName = a} :: DescribeThingGroup)
{-# DEPRECATED dtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Lude.AWSRequest DescribeThingGroup where
  type Rs DescribeThingGroup = DescribeThingGroupResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeThingGroupResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "queryVersion")
            Lude.<*> (x Lude..?> "thingGroupArn")
            Lude.<*> (x Lude..?> "thingGroupId")
            Lude.<*> (x Lude..?> "thingGroupMetadata")
            Lude.<*> (x Lude..?> "thingGroupName")
            Lude.<*> (x Lude..?> "queryString")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "thingGroupProperties")
            Lude.<*> (x Lude..?> "indexName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeThingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeThingGroup where
  toPath DescribeThingGroup' {..} =
    Lude.mconcat ["/thing-groups/", Lude.toBS thingGroupName]

instance Lude.ToQuery DescribeThingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeThingGroupResponse' smart constructor.
data DescribeThingGroupResponse = DescribeThingGroupResponse'
  { status ::
      Lude.Maybe DynamicGroupStatus,
    queryVersion :: Lude.Maybe Lude.Text,
    thingGroupARN :: Lude.Maybe Lude.Text,
    thingGroupId :: Lude.Maybe Lude.Text,
    thingGroupMetadata ::
      Lude.Maybe ThingGroupMetadata,
    thingGroupName ::
      Lude.Maybe Lude.Text,
    queryString :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Integer,
    thingGroupProperties ::
      Lude.Maybe ThingGroupProperties,
    indexName :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingGroupResponse' with the minimum fields required to make a request.
--
-- * 'indexName' - The dynamic thing group index name.
-- * 'queryString' - The dynamic thing group search query string.
-- * 'queryVersion' - The dynamic thing group query version.
-- * 'responseStatus' - The response status code.
-- * 'status' - The dynamic thing group status.
-- * 'thingGroupARN' - The thing group ARN.
-- * 'thingGroupId' - The thing group ID.
-- * 'thingGroupMetadata' - Thing group metadata.
-- * 'thingGroupName' - The name of the thing group.
-- * 'thingGroupProperties' - The thing group properties.
-- * 'version' - The version of the thing group.
mkDescribeThingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeThingGroupResponse
mkDescribeThingGroupResponse pResponseStatus_ =
  DescribeThingGroupResponse'
    { status = Lude.Nothing,
      queryVersion = Lude.Nothing,
      thingGroupARN = Lude.Nothing,
      thingGroupId = Lude.Nothing,
      thingGroupMetadata = Lude.Nothing,
      thingGroupName = Lude.Nothing,
      queryString = Lude.Nothing,
      version = Lude.Nothing,
      thingGroupProperties = Lude.Nothing,
      indexName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The dynamic thing group status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsStatus :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe DynamicGroupStatus)
dtgrsStatus = Lens.lens (status :: DescribeThingGroupResponse -> Lude.Maybe DynamicGroupStatus) (\s a -> s {status = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The dynamic thing group query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsQueryVersion :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Text)
dtgrsQueryVersion = Lens.lens (queryVersion :: DescribeThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryVersion = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The thing group ARN.
--
-- /Note:/ Consider using 'thingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsThingGroupARN :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Text)
dtgrsThingGroupARN = Lens.lens (thingGroupARN :: DescribeThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupARN = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsThingGroupARN "Use generic-lens or generic-optics with 'thingGroupARN' instead." #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsThingGroupId :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Text)
dtgrsThingGroupId = Lens.lens (thingGroupId :: DescribeThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupId = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | Thing group metadata.
--
-- /Note:/ Consider using 'thingGroupMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsThingGroupMetadata :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe ThingGroupMetadata)
dtgrsThingGroupMetadata = Lens.lens (thingGroupMetadata :: DescribeThingGroupResponse -> Lude.Maybe ThingGroupMetadata) (\s a -> s {thingGroupMetadata = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsThingGroupMetadata "Use generic-lens or generic-optics with 'thingGroupMetadata' instead." #-}

-- | The name of the thing group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsThingGroupName :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Text)
dtgrsThingGroupName = Lens.lens (thingGroupName :: DescribeThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupName = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The dynamic thing group search query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsQueryString :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Text)
dtgrsQueryString = Lens.lens (queryString :: DescribeThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryString = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The version of the thing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsVersion :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Integer)
dtgrsVersion = Lens.lens (version :: DescribeThingGroupResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsThingGroupProperties :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe ThingGroupProperties)
dtgrsThingGroupProperties = Lens.lens (thingGroupProperties :: DescribeThingGroupResponse -> Lude.Maybe ThingGroupProperties) (\s a -> s {thingGroupProperties = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

-- | The dynamic thing group index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsIndexName :: Lens.Lens' DescribeThingGroupResponse (Lude.Maybe Lude.Text)
dtgrsIndexName = Lens.lens (indexName :: DescribeThingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsResponseStatus :: Lens.Lens' DescribeThingGroupResponse Lude.Int
dtgrsResponseStatus = Lens.lens (responseStatus :: DescribeThingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeThingGroupResponse)
{-# DEPRECATED dtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
