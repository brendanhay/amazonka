{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListGrants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all grants for the specified customer master key (CMK).
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListGrants
  ( -- * Creating a request
    ListGrants (..),
    mkListGrants,

    -- ** Request lenses
    lgMarker,
    lgLimit,
    lgKeyId,

    -- * Destructuring the response
    ListGrantsResponse (..),
    mkListGrantsResponse,

    -- ** Response lenses
    lgTruncated,
    lgGrants,
    lgNextMarker,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGrants' smart constructor.
data ListGrants = ListGrants'
  { marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListGrants' with the minimum fields required to make a request.
--
-- * 'keyId' - A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
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
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
mkListGrants ::
  -- | 'keyId'
  Lude.Text ->
  ListGrants
mkListGrants pKeyId_ =
  ListGrants'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      keyId = pKeyId_
    }

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMarker :: Lens.Lens' ListGrants (Lude.Maybe Lude.Text)
lgMarker = Lens.lens (marker :: ListGrants -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGrants)
{-# DEPRECATED lgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLimit :: Lens.Lens' ListGrants (Lude.Maybe Lude.Natural)
lgLimit = Lens.lens (limit :: ListGrants -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGrants)
{-# DEPRECATED lgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
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
lgKeyId :: Lens.Lens' ListGrants Lude.Text
lgKeyId = Lens.lens (keyId :: ListGrants -> Lude.Text) (\s a -> s {keyId = a} :: ListGrants)
{-# DEPRECATED lgKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Page.AWSPager ListGrants where
  page rq rs
    | Page.stop (rs Lens.^. lgTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lgNextMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lgMarker Lens..~ rs Lens.^. lgNextMarker

instance Lude.AWSRequest ListGrants where
  type Rs ListGrants = ListGrantsResponse
  request = Req.postJSON kmsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders ListGrants where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ListGrants" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGrants where
  toJSON ListGrants' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("KeyId" Lude..= keyId)
          ]
      )

instance Lude.ToPath ListGrants where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGrants where
  toQuery = Lude.const Lude.mempty
