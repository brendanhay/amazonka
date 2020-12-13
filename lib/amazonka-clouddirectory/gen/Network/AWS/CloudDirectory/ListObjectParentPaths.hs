{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectParentPaths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- Use this API to evaluate all parents for an object. The call returns all objects from the root of the directory up to the requested object. The API returns the number of paths based on user-defined @MaxResults@ , in case there are multiple paths to the parent. The order of the paths and nodes returned is consistent among multiple API calls unless the objects are deleted or moved. Paths not leading to the directory root are ignored from the target object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectParentPaths
  ( -- * Creating a request
    ListObjectParentPaths (..),
    mkListObjectParentPaths,

    -- ** Request lenses
    loppDirectoryARN,
    loppNextToken,
    loppObjectReference,
    loppMaxResults,

    -- * Destructuring the response
    ListObjectParentPathsResponse (..),
    mkListObjectParentPathsResponse,

    -- ** Response lenses
    lopprsPathToObjectIdentifiersList,
    lopprsNextToken,
    lopprsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListObjectParentPaths' smart constructor.
data ListObjectParentPaths = ListObjectParentPaths'
  { -- | The ARN of the directory to which the parent path applies.
    directoryARN :: Lude.Text,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The reference that identifies the object whose parent paths are listed.
    objectReference :: ObjectReference,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectParentPaths' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory to which the parent path applies.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - The reference that identifies the object whose parent paths are listed.
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
mkListObjectParentPaths ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectParentPaths
mkListObjectParentPaths pDirectoryARN_ pObjectReference_ =
  ListObjectParentPaths'
    { directoryARN = pDirectoryARN_,
      nextToken = Lude.Nothing,
      objectReference = pObjectReference_,
      maxResults = Lude.Nothing
    }

-- | The ARN of the directory to which the parent path applies.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppDirectoryARN :: Lens.Lens' ListObjectParentPaths Lude.Text
loppDirectoryARN = Lens.lens (directoryARN :: ListObjectParentPaths -> Lude.Text) (\s a -> s {directoryARN = a} :: ListObjectParentPaths)
{-# DEPRECATED loppDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppNextToken :: Lens.Lens' ListObjectParentPaths (Lude.Maybe Lude.Text)
loppNextToken = Lens.lens (nextToken :: ListObjectParentPaths -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectParentPaths)
{-# DEPRECATED loppNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The reference that identifies the object whose parent paths are listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppObjectReference :: Lens.Lens' ListObjectParentPaths ObjectReference
loppObjectReference = Lens.lens (objectReference :: ListObjectParentPaths -> ObjectReference) (\s a -> s {objectReference = a} :: ListObjectParentPaths)
{-# DEPRECATED loppObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppMaxResults :: Lens.Lens' ListObjectParentPaths (Lude.Maybe Lude.Natural)
loppMaxResults = Lens.lens (maxResults :: ListObjectParentPaths -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListObjectParentPaths)
{-# DEPRECATED loppMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListObjectParentPaths where
  page rq rs
    | Page.stop (rs Lens.^. lopprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lopprsPathToObjectIdentifiersList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loppNextToken Lens..~ rs Lens.^. lopprsNextToken

instance Lude.AWSRequest ListObjectParentPaths where
  type Rs ListObjectParentPaths = ListObjectParentPathsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListObjectParentPathsResponse'
            Lude.<$> (x Lude..?> "PathToObjectIdentifiersList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectParentPaths where
  toHeaders ListObjectParentPaths' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON ListObjectParentPaths where
  toJSON ListObjectParentPaths' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ObjectReference" Lude..= objectReference),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListObjectParentPaths where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/parentpaths"

instance Lude.ToQuery ListObjectParentPaths where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListObjectParentPathsResponse' smart constructor.
data ListObjectParentPathsResponse = ListObjectParentPathsResponse'
  { -- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
    pathToObjectIdentifiersList :: Lude.Maybe [PathToObjectIdentifiers],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectParentPathsResponse' with the minimum fields required to make a request.
--
-- * 'pathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListObjectParentPathsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectParentPathsResponse
mkListObjectParentPathsResponse pResponseStatus_ =
  ListObjectParentPathsResponse'
    { pathToObjectIdentifiersList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- /Note:/ Consider using 'pathToObjectIdentifiersList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopprsPathToObjectIdentifiersList :: Lens.Lens' ListObjectParentPathsResponse (Lude.Maybe [PathToObjectIdentifiers])
lopprsPathToObjectIdentifiersList = Lens.lens (pathToObjectIdentifiersList :: ListObjectParentPathsResponse -> Lude.Maybe [PathToObjectIdentifiers]) (\s a -> s {pathToObjectIdentifiersList = a} :: ListObjectParentPathsResponse)
{-# DEPRECATED lopprsPathToObjectIdentifiersList "Use generic-lens or generic-optics with 'pathToObjectIdentifiersList' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopprsNextToken :: Lens.Lens' ListObjectParentPathsResponse (Lude.Maybe Lude.Text)
lopprsNextToken = Lens.lens (nextToken :: ListObjectParentPathsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectParentPathsResponse)
{-# DEPRECATED lopprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopprsResponseStatus :: Lens.Lens' ListObjectParentPathsResponse Lude.Int
lopprsResponseStatus = Lens.lens (responseStatus :: ListObjectParentPathsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectParentPathsResponse)
{-# DEPRECATED lopprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
