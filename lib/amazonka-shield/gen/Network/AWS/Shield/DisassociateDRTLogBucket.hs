{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateDRTLogBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team's (DRT) access to the specified Amazon S3 bucket containing your AWS WAF logs.
--
-- To make a @DisassociateDRTLogBucket@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> . However, if you are not subscribed to one of these support plans, but had been previously and had granted the DRT access to your account, you can submit a @DisassociateDRTLogBucket@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTLogBucket
  ( -- * Creating a request
    DisassociateDRTLogBucket (..),
    mkDisassociateDRTLogBucket,

    -- ** Request lenses
    ddrtlbLogBucket,

    -- * Destructuring the response
    DisassociateDRTLogBucketResponse (..),
    mkDisassociateDRTLogBucketResponse,

    -- ** Response lenses
    ddrtlbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDisassociateDRTLogBucket' smart constructor.
newtype DisassociateDRTLogBucket = DisassociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains your AWS WAF logs.
    logBucket :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDRTLogBucket' with the minimum fields required to make a request.
--
-- * 'logBucket' - The Amazon S3 bucket that contains your AWS WAF logs.
mkDisassociateDRTLogBucket ::
  -- | 'logBucket'
  Lude.Text ->
  DisassociateDRTLogBucket
mkDisassociateDRTLogBucket pLogBucket_ =
  DisassociateDRTLogBucket' {logBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
--
-- /Note:/ Consider using 'logBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtlbLogBucket :: Lens.Lens' DisassociateDRTLogBucket Lude.Text
ddrtlbLogBucket = Lens.lens (logBucket :: DisassociateDRTLogBucket -> Lude.Text) (\s a -> s {logBucket = a} :: DisassociateDRTLogBucket)
{-# DEPRECATED ddrtlbLogBucket "Use generic-lens or generic-optics with 'logBucket' instead." #-}

instance Lude.AWSRequest DisassociateDRTLogBucket where
  type Rs DisassociateDRTLogBucket = DisassociateDRTLogBucketResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateDRTLogBucketResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateDRTLogBucket where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DisassociateDRTLogBucket" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateDRTLogBucket where
  toJSON DisassociateDRTLogBucket' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("LogBucket" Lude..= logBucket)])

instance Lude.ToPath DisassociateDRTLogBucket where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateDRTLogBucket where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateDRTLogBucketResponse' smart constructor.
newtype DisassociateDRTLogBucketResponse = DisassociateDRTLogBucketResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDRTLogBucketResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateDRTLogBucketResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateDRTLogBucketResponse
mkDisassociateDRTLogBucketResponse pResponseStatus_ =
  DisassociateDRTLogBucketResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtlbrsResponseStatus :: Lens.Lens' DisassociateDRTLogBucketResponse Lude.Int
ddrtlbrsResponseStatus = Lens.lens (responseStatus :: DisassociateDRTLogBucketResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateDRTLogBucketResponse)
{-# DEPRECATED ddrtlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
