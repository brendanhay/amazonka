{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeUserHierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the hierarchy structure of the specified Amazon Connect instance.
module Network.AWS.Connect.DescribeUserHierarchyStructure
  ( -- * Creating a request
    DescribeUserHierarchyStructure (..),
    mkDescribeUserHierarchyStructure,

    -- ** Request lenses
    duhsInstanceId,

    -- * Destructuring the response
    DescribeUserHierarchyStructureResponse (..),
    mkDescribeUserHierarchyStructureResponse,

    -- ** Response lenses
    duhsrsHierarchyStructure,
    duhsrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserHierarchyStructure' smart constructor.
newtype DescribeUserHierarchyStructure = DescribeUserHierarchyStructure'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserHierarchyStructure' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkDescribeUserHierarchyStructure ::
  -- | 'instanceId'
  Lude.Text ->
  DescribeUserHierarchyStructure
mkDescribeUserHierarchyStructure pInstanceId_ =
  DescribeUserHierarchyStructure' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhsInstanceId :: Lens.Lens' DescribeUserHierarchyStructure Lude.Text
duhsInstanceId = Lens.lens (instanceId :: DescribeUserHierarchyStructure -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeUserHierarchyStructure)
{-# DEPRECATED duhsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DescribeUserHierarchyStructure where
  type
    Rs DescribeUserHierarchyStructure =
      DescribeUserHierarchyStructureResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserHierarchyStructureResponse'
            Lude.<$> (x Lude..?> "HierarchyStructure")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserHierarchyStructure where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeUserHierarchyStructure where
  toPath DescribeUserHierarchyStructure' {..} =
    Lude.mconcat ["/user-hierarchy-structure/", Lude.toBS instanceId]

instance Lude.ToQuery DescribeUserHierarchyStructure where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserHierarchyStructureResponse' smart constructor.
data DescribeUserHierarchyStructureResponse = DescribeUserHierarchyStructureResponse'
  { -- | Information about the hierarchy structure.
    hierarchyStructure :: Lude.Maybe HierarchyStructure,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserHierarchyStructureResponse' with the minimum fields required to make a request.
--
-- * 'hierarchyStructure' - Information about the hierarchy structure.
-- * 'responseStatus' - The response status code.
mkDescribeUserHierarchyStructureResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserHierarchyStructureResponse
mkDescribeUserHierarchyStructureResponse pResponseStatus_ =
  DescribeUserHierarchyStructureResponse'
    { hierarchyStructure =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the hierarchy structure.
--
-- /Note:/ Consider using 'hierarchyStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhsrsHierarchyStructure :: Lens.Lens' DescribeUserHierarchyStructureResponse (Lude.Maybe HierarchyStructure)
duhsrsHierarchyStructure = Lens.lens (hierarchyStructure :: DescribeUserHierarchyStructureResponse -> Lude.Maybe HierarchyStructure) (\s a -> s {hierarchyStructure = a} :: DescribeUserHierarchyStructureResponse)
{-# DEPRECATED duhsrsHierarchyStructure "Use generic-lens or generic-optics with 'hierarchyStructure' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhsrsResponseStatus :: Lens.Lens' DescribeUserHierarchyStructureResponse Lude.Int
duhsrsResponseStatus = Lens.lens (responseStatus :: DescribeUserHierarchyStructureResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserHierarchyStructureResponse)
{-# DEPRECATED duhsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
