{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns policies attached to an object in pagination fashion.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectPolicies
  ( -- * Creating a request
    ListObjectPolicies (..),
    mkListObjectPolicies,

    -- ** Request lenses
    lDirectoryARN,
    lConsistencyLevel,
    lNextToken,
    lObjectReference,
    lMaxResults,

    -- * Destructuring the response
    ListObjectPoliciesResponse (..),
    mkListObjectPoliciesResponse,

    -- ** Response lenses
    loprsNextToken,
    loprsAttachedPolicyIds,
    loprsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListObjectPolicies' smart constructor.
data ListObjectPolicies = ListObjectPolicies'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Lude.Maybe ConsistencyLevel,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Reference that identifies the object for which policies will be listed.
    objectReference :: ObjectReference,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectPolicies' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
-- * 'consistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - Reference that identifies the object for which policies will be listed.
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
mkListObjectPolicies ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectPolicies
mkListObjectPolicies pDirectoryARN_ pObjectReference_ =
  ListObjectPolicies'
    { directoryARN = pDirectoryARN_,
      consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      objectReference = pObjectReference_,
      maxResults = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDirectoryARN :: Lens.Lens' ListObjectPolicies Lude.Text
lDirectoryARN = Lens.lens (directoryARN :: ListObjectPolicies -> Lude.Text) (\s a -> s {directoryARN = a} :: ListObjectPolicies)
{-# DEPRECATED lDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lConsistencyLevel :: Lens.Lens' ListObjectPolicies (Lude.Maybe ConsistencyLevel)
lConsistencyLevel = Lens.lens (consistencyLevel :: ListObjectPolicies -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListObjectPolicies)
{-# DEPRECATED lConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListObjectPolicies (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListObjectPolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectPolicies)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Reference that identifies the object for which policies will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lObjectReference :: Lens.Lens' ListObjectPolicies ObjectReference
lObjectReference = Lens.lens (objectReference :: ListObjectPolicies -> ObjectReference) (\s a -> s {objectReference = a} :: ListObjectPolicies)
{-# DEPRECATED lObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListObjectPolicies (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListObjectPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListObjectPolicies)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListObjectPolicies where
  page rq rs
    | Page.stop (rs Lens.^. loprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loprsAttachedPolicyIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. loprsNextToken

instance Lude.AWSRequest ListObjectPolicies where
  type Rs ListObjectPolicies = ListObjectPoliciesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListObjectPoliciesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AttachedPolicyIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectPolicies where
  toHeaders ListObjectPolicies' {..} =
    Lude.mconcat
      [ "x-amz-data-partition" Lude.=# directoryARN,
        "x-amz-consistency-level" Lude.=# consistencyLevel
      ]

instance Lude.ToJSON ListObjectPolicies where
  toJSON ListObjectPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ObjectReference" Lude..= objectReference),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListObjectPolicies where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/policy"

instance Lude.ToQuery ListObjectPolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListObjectPoliciesResponse' smart constructor.
data ListObjectPoliciesResponse = ListObjectPoliciesResponse'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
    attachedPolicyIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'attachedPolicyIds' - A list of policy @ObjectIdentifiers@ , that are attached to the object.
-- * 'responseStatus' - The response status code.
mkListObjectPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectPoliciesResponse
mkListObjectPoliciesResponse pResponseStatus_ =
  ListObjectPoliciesResponse'
    { nextToken = Lude.Nothing,
      attachedPolicyIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprsNextToken :: Lens.Lens' ListObjectPoliciesResponse (Lude.Maybe Lude.Text)
loprsNextToken = Lens.lens (nextToken :: ListObjectPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectPoliciesResponse)
{-# DEPRECATED loprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
--
-- /Note:/ Consider using 'attachedPolicyIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprsAttachedPolicyIds :: Lens.Lens' ListObjectPoliciesResponse (Lude.Maybe [Lude.Text])
loprsAttachedPolicyIds = Lens.lens (attachedPolicyIds :: ListObjectPoliciesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {attachedPolicyIds = a} :: ListObjectPoliciesResponse)
{-# DEPRECATED loprsAttachedPolicyIds "Use generic-lens or generic-optics with 'attachedPolicyIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprsResponseStatus :: Lens.Lens' ListObjectPoliciesResponse Lude.Int
loprsResponseStatus = Lens.lens (responseStatus :: ListObjectPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectPoliciesResponse)
{-# DEPRECATED loprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
