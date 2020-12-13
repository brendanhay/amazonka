{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.ListPublicKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all public keys whose private keys were used to sign the digest files within the specified time range. The public key is needed to validate digest files that were signed with its corresponding private key.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListPublicKeys
  ( -- * Creating a request
    ListPublicKeys (..),
    mkListPublicKeys,

    -- ** Request lenses
    lpkStartTime,
    lpkNextToken,
    lpkEndTime,

    -- * Destructuring the response
    ListPublicKeysResponse (..),
    mkListPublicKeysResponse,

    -- ** Response lenses
    lpkrsPublicKeyList,
    lpkrsNextToken,
    lpkrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests the public keys for a specified time range.
--
-- /See:/ 'mkListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { -- | Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Reserved for future use.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublicKeys' with the minimum fields required to make a request.
--
-- * 'startTime' - Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
-- * 'nextToken' - Reserved for future use.
-- * 'endTime' - Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
mkListPublicKeys ::
  ListPublicKeys
mkListPublicKeys =
  ListPublicKeys'
    { startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkStartTime :: Lens.Lens' ListPublicKeys (Lude.Maybe Lude.Timestamp)
lpkStartTime = Lens.lens (startTime :: ListPublicKeys -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ListPublicKeys)
{-# DEPRECATED lpkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkNextToken :: Lens.Lens' ListPublicKeys (Lude.Maybe Lude.Text)
lpkNextToken = Lens.lens (nextToken :: ListPublicKeys -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPublicKeys)
{-# DEPRECATED lpkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkEndTime :: Lens.Lens' ListPublicKeys (Lude.Maybe Lude.Timestamp)
lpkEndTime = Lens.lens (endTime :: ListPublicKeys -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: ListPublicKeys)
{-# DEPRECATED lpkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Page.AWSPager ListPublicKeys where
  page rq rs
    | Page.stop (rs Lens.^. lpkrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpkrsPublicKeyList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpkNextToken Lens..~ rs Lens.^. lpkrsNextToken

instance Lude.AWSRequest ListPublicKeys where
  type Rs ListPublicKeys = ListPublicKeysResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPublicKeysResponse'
            Lude.<$> (x Lude..?> "PublicKeyList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPublicKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListPublicKeys" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPublicKeys where
  toJSON ListPublicKeys' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EndTime" Lude..=) Lude.<$> endTime
          ]
      )

instance Lude.ToPath ListPublicKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPublicKeys where
  toQuery = Lude.const Lude.mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { -- | Contains an array of PublicKey objects.
    publicKeyList :: Lude.Maybe [PublicKey],
    -- | Reserved for future use.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublicKeysResponse' with the minimum fields required to make a request.
--
-- * 'publicKeyList' - Contains an array of PublicKey objects.
-- * 'nextToken' - Reserved for future use.
-- * 'responseStatus' - The response status code.
mkListPublicKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPublicKeysResponse
mkListPublicKeysResponse pResponseStatus_ =
  ListPublicKeysResponse'
    { publicKeyList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains an array of PublicKey objects.
--
-- /Note:/ Consider using 'publicKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrsPublicKeyList :: Lens.Lens' ListPublicKeysResponse (Lude.Maybe [PublicKey])
lpkrsPublicKeyList = Lens.lens (publicKeyList :: ListPublicKeysResponse -> Lude.Maybe [PublicKey]) (\s a -> s {publicKeyList = a} :: ListPublicKeysResponse)
{-# DEPRECATED lpkrsPublicKeyList "Use generic-lens or generic-optics with 'publicKeyList' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrsNextToken :: Lens.Lens' ListPublicKeysResponse (Lude.Maybe Lude.Text)
lpkrsNextToken = Lens.lens (nextToken :: ListPublicKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPublicKeysResponse)
{-# DEPRECATED lpkrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrsResponseStatus :: Lens.Lens' ListPublicKeysResponse Lude.Int
lpkrsResponseStatus = Lens.lens (responseStatus :: ListPublicKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPublicKeysResponse)
{-# DEPRECATED lpkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
