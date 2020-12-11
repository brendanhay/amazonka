{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListKeyPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the names of the key policies that are attached to a customer master key (CMK). This operation is designed to get policy names that you can use in a 'GetKeyPolicy' operation. However, the only valid policy name is @default@ . You cannot perform this operation on a CMK in a different AWS account.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListKeyPolicies
  ( -- * Creating a request
    ListKeyPolicies (..),
    mkListKeyPolicies,

    -- ** Request lenses
    lkpMarker,
    lkpLimit,
    lkpKeyId,

    -- * Destructuring the response
    ListKeyPoliciesResponse (..),
    mkListKeyPoliciesResponse,

    -- ** Response lenses
    lkprsPolicyNames,
    lkprsTruncated,
    lkprsNextMarker,
    lkprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListKeyPolicies' smart constructor.
data ListKeyPolicies = ListKeyPolicies'
  { marker ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    keyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListKeyPolicies' with the minimum fields required to make a request.
--
-- * 'keyId' - A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
-- * 'limit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
-- Only one policy can be attached to a key.
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
mkListKeyPolicies ::
  -- | 'keyId'
  Lude.Text ->
  ListKeyPolicies
mkListKeyPolicies pKeyId_ =
  ListKeyPolicies'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      keyId = pKeyId_
    }

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpMarker :: Lens.Lens' ListKeyPolicies (Lude.Maybe Lude.Text)
lkpMarker = Lens.lens (marker :: ListKeyPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListKeyPolicies)
{-# DEPRECATED lkpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
-- Only one policy can be attached to a key.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpLimit :: Lens.Lens' ListKeyPolicies (Lude.Maybe Lude.Natural)
lkpLimit = Lens.lens (limit :: ListKeyPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListKeyPolicies)
{-# DEPRECATED lkpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpKeyId :: Lens.Lens' ListKeyPolicies Lude.Text
lkpKeyId = Lens.lens (keyId :: ListKeyPolicies -> Lude.Text) (\s a -> s {keyId = a} :: ListKeyPolicies)
{-# DEPRECATED lkpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Page.AWSPager ListKeyPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lkprsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lkprsNextMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lkpMarker Lens..~ rs Lens.^. lkprsNextMarker

instance Lude.AWSRequest ListKeyPolicies where
  type Rs ListKeyPolicies = ListKeyPoliciesResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListKeyPoliciesResponse'
            Lude.<$> (x Lude..?> "PolicyNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Truncated")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListKeyPolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ListKeyPolicies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListKeyPolicies where
  toJSON ListKeyPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("KeyId" Lude..= keyId)
          ]
      )

instance Lude.ToPath ListKeyPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListKeyPolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListKeyPoliciesResponse' smart constructor.
data ListKeyPoliciesResponse = ListKeyPoliciesResponse'
  { policyNames ::
      Lude.Maybe [Lude.Text],
    truncated :: Lude.Maybe Lude.Bool,
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListKeyPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
-- * 'policyNames' - A list of key policy names. The only valid value is @default@ .
-- * 'responseStatus' - The response status code.
-- * 'truncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
mkListKeyPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListKeyPoliciesResponse
mkListKeyPoliciesResponse pResponseStatus_ =
  ListKeyPoliciesResponse'
    { policyNames = Lude.Nothing,
      truncated = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of key policy names. The only valid value is @default@ .
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprsPolicyNames :: Lens.Lens' ListKeyPoliciesResponse (Lude.Maybe [Lude.Text])
lkprsPolicyNames = Lens.lens (policyNames :: ListKeyPoliciesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNames = a} :: ListKeyPoliciesResponse)
{-# DEPRECATED lkprsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprsTruncated :: Lens.Lens' ListKeyPoliciesResponse (Lude.Maybe Lude.Bool)
lkprsTruncated = Lens.lens (truncated :: ListKeyPoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: ListKeyPoliciesResponse)
{-# DEPRECATED lkprsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprsNextMarker :: Lens.Lens' ListKeyPoliciesResponse (Lude.Maybe Lude.Text)
lkprsNextMarker = Lens.lens (nextMarker :: ListKeyPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListKeyPoliciesResponse)
{-# DEPRECATED lkprsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprsResponseStatus :: Lens.Lens' ListKeyPoliciesResponse Lude.Int
lkprsResponseStatus = Lens.lens (responseStatus :: ListKeyPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListKeyPoliciesResponse)
{-# DEPRECATED lkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
