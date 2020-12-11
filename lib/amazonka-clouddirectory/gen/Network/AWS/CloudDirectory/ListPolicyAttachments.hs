{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListPolicyAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPolicyAttachments
  ( -- * Creating a request
    ListPolicyAttachments (..),
    mkListPolicyAttachments,

    -- ** Request lenses
    lpaConsistencyLevel,
    lpaNextToken,
    lpaMaxResults,
    lpaDirectoryARN,
    lpaPolicyReference,

    -- * Destructuring the response
    ListPolicyAttachmentsResponse (..),
    mkListPolicyAttachmentsResponse,

    -- ** Response lenses
    lparsObjectIdentifiers,
    lparsNextToken,
    lparsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPolicyAttachments' smart constructor.
data ListPolicyAttachments = ListPolicyAttachments'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    directoryARN :: Lude.Text,
    policyReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicyAttachments' with the minimum fields required to make a request.
--
-- * 'consistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The pagination token.
-- * 'policyReference' - The reference that identifies the policy object.
mkListPolicyAttachments ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'policyReference'
  ObjectReference ->
  ListPolicyAttachments
mkListPolicyAttachments pDirectoryARN_ pPolicyReference_ =
  ListPolicyAttachments'
    { consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      policyReference = pPolicyReference_
    }

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaConsistencyLevel :: Lens.Lens' ListPolicyAttachments (Lude.Maybe ConsistencyLevel)
lpaConsistencyLevel = Lens.lens (consistencyLevel :: ListPolicyAttachments -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListPolicyAttachments)
{-# DEPRECATED lpaConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaNextToken :: Lens.Lens' ListPolicyAttachments (Lude.Maybe Lude.Text)
lpaNextToken = Lens.lens (nextToken :: ListPolicyAttachments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPolicyAttachments)
{-# DEPRECATED lpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaMaxResults :: Lens.Lens' ListPolicyAttachments (Lude.Maybe Lude.Natural)
lpaMaxResults = Lens.lens (maxResults :: ListPolicyAttachments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPolicyAttachments)
{-# DEPRECATED lpaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaDirectoryARN :: Lens.Lens' ListPolicyAttachments Lude.Text
lpaDirectoryARN = Lens.lens (directoryARN :: ListPolicyAttachments -> Lude.Text) (\s a -> s {directoryARN = a} :: ListPolicyAttachments)
{-# DEPRECATED lpaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaPolicyReference :: Lens.Lens' ListPolicyAttachments ObjectReference
lpaPolicyReference = Lens.lens (policyReference :: ListPolicyAttachments -> ObjectReference) (\s a -> s {policyReference = a} :: ListPolicyAttachments)
{-# DEPRECATED lpaPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

instance Page.AWSPager ListPolicyAttachments where
  page rq rs
    | Page.stop (rs Lens.^. lparsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lparsObjectIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpaNextToken Lens..~ rs Lens.^. lparsNextToken

instance Lude.AWSRequest ListPolicyAttachments where
  type Rs ListPolicyAttachments = ListPolicyAttachmentsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPolicyAttachmentsResponse'
            Lude.<$> (x Lude..?> "ObjectIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicyAttachments where
  toHeaders ListPolicyAttachments' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON ListPolicyAttachments where
  toJSON ListPolicyAttachments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("PolicyReference" Lude..= policyReference)
          ]
      )

instance Lude.ToPath ListPolicyAttachments where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/policy/attachment"

instance Lude.ToQuery ListPolicyAttachments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPolicyAttachmentsResponse' smart constructor.
data ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse'
  { objectIdentifiers ::
      Lude.Maybe [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListPolicyAttachmentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'objectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
-- * 'responseStatus' - The response status code.
mkListPolicyAttachmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPolicyAttachmentsResponse
mkListPolicyAttachmentsResponse pResponseStatus_ =
  ListPolicyAttachmentsResponse'
    { objectIdentifiers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparsObjectIdentifiers :: Lens.Lens' ListPolicyAttachmentsResponse (Lude.Maybe [Lude.Text])
lparsObjectIdentifiers = Lens.lens (objectIdentifiers :: ListPolicyAttachmentsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {objectIdentifiers = a} :: ListPolicyAttachmentsResponse)
{-# DEPRECATED lparsObjectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparsNextToken :: Lens.Lens' ListPolicyAttachmentsResponse (Lude.Maybe Lude.Text)
lparsNextToken = Lens.lens (nextToken :: ListPolicyAttachmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPolicyAttachmentsResponse)
{-# DEPRECATED lparsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparsResponseStatus :: Lens.Lens' ListPolicyAttachmentsResponse Lude.Int
lparsResponseStatus = Lens.lens (responseStatus :: ListPolicyAttachmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPolicyAttachmentsResponse)
{-# DEPRECATED lparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
