{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListSchemaExtensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all schema extensions applied to a Microsoft AD Directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListSchemaExtensions
  ( -- * Creating a request
    ListSchemaExtensions (..),
    mkListSchemaExtensions,

    -- ** Request lenses
    lseDirectoryId,
    lseNextToken,
    lseLimit,

    -- * Destructuring the response
    ListSchemaExtensionsResponse (..),
    mkListSchemaExtensionsResponse,

    -- ** Response lenses
    lsersSchemaExtensionsInfo,
    lsersNextToken,
    lsersResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSchemaExtensions' smart constructor.
data ListSchemaExtensions = ListSchemaExtensions'
  { -- | The identifier of the directory from which to retrieve the schema extension information.
    directoryId :: Lude.Text,
    -- | The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSchemaExtensions' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory from which to retrieve the schema extension information.
-- * 'nextToken' - The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
-- * 'limit' - The maximum number of items to return.
mkListSchemaExtensions ::
  -- | 'directoryId'
  Lude.Text ->
  ListSchemaExtensions
mkListSchemaExtensions pDirectoryId_ =
  ListSchemaExtensions'
    { directoryId = pDirectoryId_,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The identifier of the directory from which to retrieve the schema extension information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseDirectoryId :: Lens.Lens' ListSchemaExtensions Lude.Text
lseDirectoryId = Lens.lens (directoryId :: ListSchemaExtensions -> Lude.Text) (\s a -> s {directoryId = a} :: ListSchemaExtensions)
{-# DEPRECATED lseDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseNextToken :: Lens.Lens' ListSchemaExtensions (Lude.Maybe Lude.Text)
lseNextToken = Lens.lens (nextToken :: ListSchemaExtensions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSchemaExtensions)
{-# DEPRECATED lseNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseLimit :: Lens.Lens' ListSchemaExtensions (Lude.Maybe Lude.Natural)
lseLimit = Lens.lens (limit :: ListSchemaExtensions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListSchemaExtensions)
{-# DEPRECATED lseLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListSchemaExtensions where
  page rq rs
    | Page.stop (rs Lens.^. lsersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsersSchemaExtensionsInfo) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lseNextToken Lens..~ rs Lens.^. lsersNextToken

instance Lude.AWSRequest ListSchemaExtensions where
  type Rs ListSchemaExtensions = ListSchemaExtensionsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSchemaExtensionsResponse'
            Lude.<$> (x Lude..?> "SchemaExtensionsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSchemaExtensions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.ListSchemaExtensions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSchemaExtensions where
  toJSON ListSchemaExtensions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListSchemaExtensions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSchemaExtensions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSchemaExtensionsResponse' smart constructor.
data ListSchemaExtensionsResponse = ListSchemaExtensionsResponse'
  { -- | Information about the schema extensions applied to the directory.
    schemaExtensionsInfo :: Lude.Maybe [SchemaExtensionInfo],
    -- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSchemaExtensionsResponse' with the minimum fields required to make a request.
--
-- * 'schemaExtensionsInfo' - Information about the schema extensions applied to the directory.
-- * 'nextToken' - If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkListSchemaExtensionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSchemaExtensionsResponse
mkListSchemaExtensionsResponse pResponseStatus_ =
  ListSchemaExtensionsResponse'
    { schemaExtensionsInfo =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the schema extensions applied to the directory.
--
-- /Note:/ Consider using 'schemaExtensionsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsersSchemaExtensionsInfo :: Lens.Lens' ListSchemaExtensionsResponse (Lude.Maybe [SchemaExtensionInfo])
lsersSchemaExtensionsInfo = Lens.lens (schemaExtensionsInfo :: ListSchemaExtensionsResponse -> Lude.Maybe [SchemaExtensionInfo]) (\s a -> s {schemaExtensionsInfo = a} :: ListSchemaExtensionsResponse)
{-# DEPRECATED lsersSchemaExtensionsInfo "Use generic-lens or generic-optics with 'schemaExtensionsInfo' instead." #-}

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsersNextToken :: Lens.Lens' ListSchemaExtensionsResponse (Lude.Maybe Lude.Text)
lsersNextToken = Lens.lens (nextToken :: ListSchemaExtensionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSchemaExtensionsResponse)
{-# DEPRECATED lsersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsersResponseStatus :: Lens.Lens' ListSchemaExtensionsResponse Lude.Int
lsersResponseStatus = Lens.lens (responseStatus :: ListSchemaExtensionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSchemaExtensionsResponse)
{-# DEPRECATED lsersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
