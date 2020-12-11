{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectChildren
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of child objects that are associated with a given object.
module Network.AWS.CloudDirectory.ListObjectChildren
  ( -- * Creating a request
    ListObjectChildren (..),
    mkListObjectChildren,

    -- ** Request lenses
    locConsistencyLevel,
    locNextToken,
    locMaxResults,
    locDirectoryARN,
    locObjectReference,

    -- * Destructuring the response
    ListObjectChildrenResponse (..),
    mkListObjectChildrenResponse,

    -- ** Response lenses
    locrsChildren,
    locrsNextToken,
    locrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListObjectChildren' smart constructor.
data ListObjectChildren = ListObjectChildren'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
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

-- | Creates a value of 'ListObjectChildren' with the minimum fields required to make a request.
--
-- * 'consistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - The reference that identifies the object for which child objects are being listed.
mkListObjectChildren ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectChildren
mkListObjectChildren pDirectoryARN_ pObjectReference_ =
  ListObjectChildren'
    { consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      objectReference = pObjectReference_
    }

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locConsistencyLevel :: Lens.Lens' ListObjectChildren (Lude.Maybe ConsistencyLevel)
locConsistencyLevel = Lens.lens (consistencyLevel :: ListObjectChildren -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListObjectChildren)
{-# DEPRECATED locConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locNextToken :: Lens.Lens' ListObjectChildren (Lude.Maybe Lude.Text)
locNextToken = Lens.lens (nextToken :: ListObjectChildren -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectChildren)
{-# DEPRECATED locNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locMaxResults :: Lens.Lens' ListObjectChildren (Lude.Maybe Lude.Natural)
locMaxResults = Lens.lens (maxResults :: ListObjectChildren -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListObjectChildren)
{-# DEPRECATED locMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locDirectoryARN :: Lens.Lens' ListObjectChildren Lude.Text
locDirectoryARN = Lens.lens (directoryARN :: ListObjectChildren -> Lude.Text) (\s a -> s {directoryARN = a} :: ListObjectChildren)
{-# DEPRECATED locDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The reference that identifies the object for which child objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locObjectReference :: Lens.Lens' ListObjectChildren ObjectReference
locObjectReference = Lens.lens (objectReference :: ListObjectChildren -> ObjectReference) (\s a -> s {objectReference = a} :: ListObjectChildren)
{-# DEPRECATED locObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest ListObjectChildren where
  type Rs ListObjectChildren = ListObjectChildrenResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListObjectChildrenResponse'
            Lude.<$> (x Lude..?> "Children" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectChildren where
  toHeaders ListObjectChildren' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON ListObjectChildren where
  toJSON ListObjectChildren' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath ListObjectChildren where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/children"

instance Lude.ToQuery ListObjectChildren where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListObjectChildrenResponse' smart constructor.
data ListObjectChildrenResponse = ListObjectChildrenResponse'
  { children ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListObjectChildrenResponse' with the minimum fields required to make a request.
--
-- * 'children' - Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListObjectChildrenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectChildrenResponse
mkListObjectChildrenResponse pResponseStatus_ =
  ListObjectChildrenResponse'
    { children = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- /Note:/ Consider using 'children' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrsChildren :: Lens.Lens' ListObjectChildrenResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
locrsChildren = Lens.lens (children :: ListObjectChildrenResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {children = a} :: ListObjectChildrenResponse)
{-# DEPRECATED locrsChildren "Use generic-lens or generic-optics with 'children' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrsNextToken :: Lens.Lens' ListObjectChildrenResponse (Lude.Maybe Lude.Text)
locrsNextToken = Lens.lens (nextToken :: ListObjectChildrenResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectChildrenResponse)
{-# DEPRECATED locrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrsResponseStatus :: Lens.Lens' ListObjectChildrenResponse Lude.Int
locrsResponseStatus = Lens.lens (responseStatus :: ListObjectChildrenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectChildrenResponse)
{-# DEPRECATED locrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
