{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags on an Amazon RDS resource.
--
-- For an overview on tagging an Amazon RDS resource, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide/ .
module Network.AWS.RDS.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrFilters,
    ltfrResourceName,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsTagList,
    ltfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { filters ::
      Lude.Maybe [Filter],
    resourceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'resourceName' - The Amazon RDS resource with tags to be listed. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ .
mkListTagsForResource ::
  -- | 'resourceName'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceName_ =
  ListTagsForResource'
    { filters = Lude.Nothing,
      resourceName = pResourceName_
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrFilters :: Lens.Lens' ListTagsForResource (Lude.Maybe [Filter])
ltfrFilters = Lens.lens (filters :: ListTagsForResource -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: ListTagsForResource)
{-# DEPRECATED ltfrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceName :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceName = Lens.lens (resourceName :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceName = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> ( x Lude..@? "TagList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Tag")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTagsForResource" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "ResourceName" Lude.=: resourceName
      ]

-- |
--
-- /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { tagList ::
      Lude.Maybe [Tag],
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

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tagList' - List of tags returned by the ListTagsForResource operation.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { tagList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of tags returned by the ListTagsForResource operation.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTagList :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe [Tag])
ltfrrsTagList = Lens.lens (tagList :: ListTagsForResourceResponse -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
