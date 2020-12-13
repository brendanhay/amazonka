{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateDRTLogBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to access the specified Amazon S3 bucket containing your AWS WAF logs. You can associate up to 10 Amazon S3 buckets with your subscription.
--
-- To use the services of the DRT and make an @AssociateDRTLogBucket@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> .
module Network.AWS.Shield.AssociateDRTLogBucket
  ( -- * Creating a request
    AssociateDRTLogBucket (..),
    mkAssociateDRTLogBucket,

    -- ** Request lenses
    adrtlbLogBucket,

    -- * Destructuring the response
    AssociateDRTLogBucketResponse (..),
    mkAssociateDRTLogBucketResponse,

    -- ** Response lenses
    adrtlbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkAssociateDRTLogBucket' smart constructor.
newtype AssociateDRTLogBucket = AssociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains your AWS WAF logs.
    logBucket :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDRTLogBucket' with the minimum fields required to make a request.
--
-- * 'logBucket' - The Amazon S3 bucket that contains your AWS WAF logs.
mkAssociateDRTLogBucket ::
  -- | 'logBucket'
  Lude.Text ->
  AssociateDRTLogBucket
mkAssociateDRTLogBucket pLogBucket_ =
  AssociateDRTLogBucket' {logBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
--
-- /Note:/ Consider using 'logBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtlbLogBucket :: Lens.Lens' AssociateDRTLogBucket Lude.Text
adrtlbLogBucket = Lens.lens (logBucket :: AssociateDRTLogBucket -> Lude.Text) (\s a -> s {logBucket = a} :: AssociateDRTLogBucket)
{-# DEPRECATED adrtlbLogBucket "Use generic-lens or generic-optics with 'logBucket' instead." #-}

instance Lude.AWSRequest AssociateDRTLogBucket where
  type Rs AssociateDRTLogBucket = AssociateDRTLogBucketResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateDRTLogBucketResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateDRTLogBucket where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.AssociateDRTLogBucket" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateDRTLogBucket where
  toJSON AssociateDRTLogBucket' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("LogBucket" Lude..= logBucket)])

instance Lude.ToPath AssociateDRTLogBucket where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDRTLogBucket where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateDRTLogBucketResponse' smart constructor.
newtype AssociateDRTLogBucketResponse = AssociateDRTLogBucketResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDRTLogBucketResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateDRTLogBucketResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateDRTLogBucketResponse
mkAssociateDRTLogBucketResponse pResponseStatus_ =
  AssociateDRTLogBucketResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtlbrsResponseStatus :: Lens.Lens' AssociateDRTLogBucketResponse Lude.Int
adrtlbrsResponseStatus = Lens.lens (responseStatus :: AssociateDRTLogBucketResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateDRTLogBucketResponse)
{-# DEPRECATED adrtlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
