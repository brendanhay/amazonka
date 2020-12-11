{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a search index.
module Network.AWS.IoT.DescribeIndex
  ( -- * Creating a request
    DescribeIndex (..),
    mkDescribeIndex,

    -- ** Request lenses
    diIndexName,

    -- * Destructuring the response
    DescribeIndexResponse (..),
    mkDescribeIndexResponse,

    -- ** Response lenses
    dirsIndexStatus,
    dirsSchema,
    dirsIndexName,
    dirsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeIndex' smart constructor.
newtype DescribeIndex = DescribeIndex' {indexName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIndex' with the minimum fields required to make a request.
--
-- * 'indexName' - The index name.
mkDescribeIndex ::
  -- | 'indexName'
  Lude.Text ->
  DescribeIndex
mkDescribeIndex pIndexName_ =
  DescribeIndex' {indexName = pIndexName_}

-- | The index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIndexName :: Lens.Lens' DescribeIndex Lude.Text
diIndexName = Lens.lens (indexName :: DescribeIndex -> Lude.Text) (\s a -> s {indexName = a} :: DescribeIndex)
{-# DEPRECATED diIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.AWSRequest DescribeIndex where
  type Rs DescribeIndex = DescribeIndexResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            Lude.<$> (x Lude..?> "indexStatus")
            Lude.<*> (x Lude..?> "schema")
            Lude.<*> (x Lude..?> "indexName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeIndex where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeIndex where
  toPath DescribeIndex' {..} =
    Lude.mconcat ["/indices/", Lude.toBS indexName]

instance Lude.ToQuery DescribeIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { indexStatus ::
      Lude.Maybe IndexStatus,
    schema :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeIndexResponse' with the minimum fields required to make a request.
--
-- * 'indexName' - The index name.
-- * 'indexStatus' - The index status.
-- * 'responseStatus' - The response status code.
-- * 'schema' - Contains a value that specifies the type of indexing performed. Valid values are:
--
--
--     * REGISTRY – Your thing index contains only registry data.
--
--
--     * REGISTRY_AND_SHADOW - Your thing index contains registry data and shadow data.
--
--
--     * REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains registry data and thing connectivity status data.
--
--
--     * REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index contains registry data, shadow data, and thing connectivity status data.
mkDescribeIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeIndexResponse
mkDescribeIndexResponse pResponseStatus_ =
  DescribeIndexResponse'
    { indexStatus = Lude.Nothing,
      schema = Lude.Nothing,
      indexName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The index status.
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsIndexStatus :: Lens.Lens' DescribeIndexResponse (Lude.Maybe IndexStatus)
dirsIndexStatus = Lens.lens (indexStatus :: DescribeIndexResponse -> Lude.Maybe IndexStatus) (\s a -> s {indexStatus = a} :: DescribeIndexResponse)
{-# DEPRECATED dirsIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

-- | Contains a value that specifies the type of indexing performed. Valid values are:
--
--
--     * REGISTRY – Your thing index contains only registry data.
--
--
--     * REGISTRY_AND_SHADOW - Your thing index contains registry data and shadow data.
--
--
--     * REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains registry data and thing connectivity status data.
--
--
--     * REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index contains registry data, shadow data, and thing connectivity status data.
--
--
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsSchema :: Lens.Lens' DescribeIndexResponse (Lude.Maybe Lude.Text)
dirsSchema = Lens.lens (schema :: DescribeIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {schema = a} :: DescribeIndexResponse)
{-# DEPRECATED dirsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsIndexName :: Lens.Lens' DescribeIndexResponse (Lude.Maybe Lude.Text)
dirsIndexName = Lens.lens (indexName :: DescribeIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: DescribeIndexResponse)
{-# DEPRECATED dirsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DescribeIndexResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DescribeIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeIndexResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
