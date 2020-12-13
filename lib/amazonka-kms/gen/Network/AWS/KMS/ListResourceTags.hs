{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListResourceTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all tags for the specified customer master key (CMK).
--
-- You cannot perform this operation on a CMK in a different AWS account.
module Network.AWS.KMS.ListResourceTags
  ( -- * Creating a request
    ListResourceTags (..),
    mkListResourceTags,

    -- ** Request lenses
    lrtKeyId,
    lrtMarker,
    lrtLimit,

    -- * Destructuring the response
    ListResourceTagsResponse (..),
    mkListResourceTagsResponse,

    -- ** Response lenses
    lrtrsTruncated,
    lrtrsNextMarker,
    lrtrsTags,
    lrtrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResourceTags' smart constructor.
data ListResourceTags = ListResourceTags'
  { -- | A unique identifier for the customer master key (CMK).
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
    keyId :: Lude.Text,
    -- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
    --
    -- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceTags' with the minimum fields required to make a request.
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
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
-- * 'limit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
mkListResourceTags ::
  -- | 'keyId'
  Lude.Text ->
  ListResourceTags
mkListResourceTags pKeyId_ =
  ListResourceTags'
    { keyId = pKeyId_,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

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
lrtKeyId :: Lens.Lens' ListResourceTags Lude.Text
lrtKeyId = Lens.lens (keyId :: ListResourceTags -> Lude.Text) (\s a -> s {keyId = a} :: ListResourceTags)
{-# DEPRECATED lrtKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMarker :: Lens.Lens' ListResourceTags (Lude.Maybe Lude.Text)
lrtMarker = Lens.lens (marker :: ListResourceTags -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListResourceTags)
{-# DEPRECATED lrtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtLimit :: Lens.Lens' ListResourceTags (Lude.Maybe Lude.Natural)
lrtLimit = Lens.lens (limit :: ListResourceTags -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListResourceTags)
{-# DEPRECATED lrtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListResourceTags where
  type Rs ListResourceTags = ListResourceTagsResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceTagsResponse'
            Lude.<$> (x Lude..?> "Truncated")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourceTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ListResourceTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourceTags where
  toJSON ListResourceTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListResourceTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourceTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourceTagsResponse' smart constructor.
data ListResourceTagsResponse = ListResourceTagsResponse'
  { -- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
    truncated :: Lude.Maybe Lude.Bool,
    -- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
    --
    -- Do not assume or infer any information from this value.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A list of tags. Each tag consists of a tag key and a tag value.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceTagsResponse' with the minimum fields required to make a request.
--
-- * 'truncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
-- * 'nextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
-- * 'tags' - A list of tags. Each tag consists of a tag key and a tag value.
-- * 'responseStatus' - The response status code.
mkListResourceTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceTagsResponse
mkListResourceTagsResponse pResponseStatus_ =
  ListResourceTagsResponse'
    { truncated = Lude.Nothing,
      nextMarker = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsTruncated :: Lens.Lens' ListResourceTagsResponse (Lude.Maybe Lude.Bool)
lrtrsTruncated = Lens.lens (truncated :: ListResourceTagsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: ListResourceTagsResponse)
{-# DEPRECATED lrtrsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsNextMarker :: Lens.Lens' ListResourceTagsResponse (Lude.Maybe Lude.Text)
lrtrsNextMarker = Lens.lens (nextMarker :: ListResourceTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListResourceTagsResponse)
{-# DEPRECATED lrtrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A list of tags. Each tag consists of a tag key and a tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsTags :: Lens.Lens' ListResourceTagsResponse (Lude.Maybe [Tag])
lrtrsTags = Lens.lens (tags :: ListResourceTagsResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ListResourceTagsResponse)
{-# DEPRECATED lrtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsResponseStatus :: Lens.Lens' ListResourceTagsResponse Lude.Int
lrtrsResponseStatus = Lens.lens (responseStatus :: ListResourceTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceTagsResponse)
{-# DEPRECATED lrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
