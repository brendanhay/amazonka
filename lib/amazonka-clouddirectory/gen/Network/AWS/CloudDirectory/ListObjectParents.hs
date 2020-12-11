{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists parent objects that are associated with a given object in pagination fashion.
module Network.AWS.CloudDirectory.ListObjectParents
  ( -- * Creating a request
    ListObjectParents (..),
    mkListObjectParents,

    -- ** Request lenses
    lopConsistencyLevel,
    lopIncludeAllLinksToEachParent,
    lopNextToken,
    lopMaxResults,
    lopDirectoryARN,
    lopObjectReference,

    -- * Destructuring the response
    ListObjectParentsResponse (..),
    mkListObjectParentsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsParents,
    lrsParentLinks,
    lrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListObjectParents' smart constructor.
data ListObjectParents = ListObjectParents'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
    includeAllLinksToEachParent :: Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    directoryARN :: Lude.Text,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectParents' with the minimum fields required to make a request.
--
-- * 'consistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
-- * 'includeAllLinksToEachParent' - When set to True, returns all 'ListObjectParentsResponse$ParentLinks' . There could be multiple links between a parent-child pair.
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - The reference that identifies the object for which parent objects are being listed.
mkListObjectParents ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectParents
mkListObjectParents pDirectoryARN_ pObjectReference_ =
  ListObjectParents'
    { consistencyLevel = Lude.Nothing,
      includeAllLinksToEachParent = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      objectReference = pObjectReference_
    }

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopConsistencyLevel :: Lens.Lens' ListObjectParents (Lude.Maybe ConsistencyLevel)
lopConsistencyLevel = Lens.lens (consistencyLevel :: ListObjectParents -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListObjectParents)
{-# DEPRECATED lopConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | When set to True, returns all 'ListObjectParentsResponse$ParentLinks' . There could be multiple links between a parent-child pair.
--
-- /Note:/ Consider using 'includeAllLinksToEachParent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopIncludeAllLinksToEachParent :: Lens.Lens' ListObjectParents (Lude.Maybe Lude.Bool)
lopIncludeAllLinksToEachParent = Lens.lens (includeAllLinksToEachParent :: ListObjectParents -> Lude.Maybe Lude.Bool) (\s a -> s {includeAllLinksToEachParent = a} :: ListObjectParents)
{-# DEPRECATED lopIncludeAllLinksToEachParent "Use generic-lens or generic-optics with 'includeAllLinksToEachParent' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopNextToken :: Lens.Lens' ListObjectParents (Lude.Maybe Lude.Text)
lopNextToken = Lens.lens (nextToken :: ListObjectParents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectParents)
{-# DEPRECATED lopNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopMaxResults :: Lens.Lens' ListObjectParents (Lude.Maybe Lude.Natural)
lopMaxResults = Lens.lens (maxResults :: ListObjectParents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListObjectParents)
{-# DEPRECATED lopMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopDirectoryARN :: Lens.Lens' ListObjectParents Lude.Text
lopDirectoryARN = Lens.lens (directoryARN :: ListObjectParents -> Lude.Text) (\s a -> s {directoryARN = a} :: ListObjectParents)
{-# DEPRECATED lopDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The reference that identifies the object for which parent objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopObjectReference :: Lens.Lens' ListObjectParents ObjectReference
lopObjectReference = Lens.lens (objectReference :: ListObjectParents -> ObjectReference) (\s a -> s {objectReference = a} :: ListObjectParents)
{-# DEPRECATED lopObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest ListObjectParents where
  type Rs ListObjectParents = ListObjectParentsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListObjectParentsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Parents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ParentLinks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectParents where
  toHeaders ListObjectParents' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON ListObjectParents where
  toJSON ListObjectParents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeAllLinksToEachParent" Lude..=)
              Lude.<$> includeAllLinksToEachParent,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath ListObjectParents where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/parent"

instance Lude.ToQuery ListObjectParents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListObjectParentsResponse' smart constructor.
data ListObjectParentsResponse = ListObjectParentsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    parents ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    parentLinks ::
      Lude.Maybe
        [ObjectIdentifierAndLinkNameTuple],
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

-- | Creates a value of 'ListObjectParentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'parentLinks' - Returns a list of parent reference and LinkName Tuples.
-- * 'parents' - The parent structure, which is a map with key as the @ObjectIdentifier@ and LinkName as the value.
-- * 'responseStatus' - The response status code.
mkListObjectParentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectParentsResponse
mkListObjectParentsResponse pResponseStatus_ =
  ListObjectParentsResponse'
    { nextToken = Lude.Nothing,
      parents = Lude.Nothing,
      parentLinks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListObjectParentsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListObjectParentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectParentsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parent structure, which is a map with key as the @ObjectIdentifier@ and LinkName as the value.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsParents :: Lens.Lens' ListObjectParentsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
lrsParents = Lens.lens (parents :: ListObjectParentsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parents = a} :: ListObjectParentsResponse)
{-# DEPRECATED lrsParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | Returns a list of parent reference and LinkName Tuples.
--
-- /Note:/ Consider using 'parentLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsParentLinks :: Lens.Lens' ListObjectParentsResponse (Lude.Maybe [ObjectIdentifierAndLinkNameTuple])
lrsParentLinks = Lens.lens (parentLinks :: ListObjectParentsResponse -> Lude.Maybe [ObjectIdentifierAndLinkNameTuple]) (\s a -> s {parentLinks = a} :: ListObjectParentsResponse)
{-# DEPRECATED lrsParentLinks "Use generic-lens or generic-optics with 'parentLinks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListObjectParentsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListObjectParentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectParentsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
