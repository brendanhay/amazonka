{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.LookupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.LookupPolicy
  ( -- * Creating a request
    LookupPolicy (..),
    mkLookupPolicy,

    -- ** Request lenses
    lpNextToken,
    lpMaxResults,
    lpDirectoryARN,
    lpObjectReference,

    -- * Destructuring the response
    LookupPolicyResponse (..),
    mkLookupPolicyResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsPolicyToPathList,
    lprsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkLookupPolicy' smart constructor.
data LookupPolicy = LookupPolicy'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LookupPolicy' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The token to request the next page of results.
-- * 'objectReference' - Reference that identifies the object whose policies will be looked up.
mkLookupPolicy ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  LookupPolicy
mkLookupPolicy pDirectoryARN_ pObjectReference_ =
  LookupPolicy'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      objectReference = pObjectReference_
    }

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' LookupPolicy (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: LookupPolicy -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: LookupPolicy)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' LookupPolicy (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: LookupPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: LookupPolicy)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpDirectoryARN :: Lens.Lens' LookupPolicy Lude.Text
lpDirectoryARN = Lens.lens (directoryARN :: LookupPolicy -> Lude.Text) (\s a -> s {directoryARN = a} :: LookupPolicy)
{-# DEPRECATED lpDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Reference that identifies the object whose policies will be looked up.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpObjectReference :: Lens.Lens' LookupPolicy ObjectReference
lpObjectReference = Lens.lens (objectReference :: LookupPolicy -> ObjectReference) (\s a -> s {objectReference = a} :: LookupPolicy)
{-# DEPRECATED lpObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Page.AWSPager LookupPolicy where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPolicyToPathList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest LookupPolicy where
  type Rs LookupPolicy = LookupPolicyResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          LookupPolicyResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "PolicyToPathList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders LookupPolicy where
  toHeaders LookupPolicy' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON LookupPolicy where
  toJSON LookupPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath LookupPolicy where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/policy/lookup"

instance Lude.ToQuery LookupPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkLookupPolicyResponse' smart constructor.
data LookupPolicyResponse = LookupPolicyResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    policyToPathList :: Lude.Maybe [PolicyToPath],
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

-- | Creates a value of 'LookupPolicyResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'policyToPathList' - Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
-- * 'responseStatus' - The response status code.
mkLookupPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  LookupPolicyResponse
mkLookupPolicyResponse pResponseStatus_ =
  LookupPolicyResponse'
    { nextToken = Lude.Nothing,
      policyToPathList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' LookupPolicyResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: LookupPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: LookupPolicyResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'policyToPathList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPolicyToPathList :: Lens.Lens' LookupPolicyResponse (Lude.Maybe [PolicyToPath])
lprsPolicyToPathList = Lens.lens (policyToPathList :: LookupPolicyResponse -> Lude.Maybe [PolicyToPath]) (\s a -> s {policyToPathList = a} :: LookupPolicyResponse)
{-# DEPRECATED lprsPolicyToPathList "Use generic-lens or generic-optics with 'policyToPathList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' LookupPolicyResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: LookupPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: LookupPolicyResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
