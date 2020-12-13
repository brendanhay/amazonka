{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeDRTAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current role and list of Amazon S3 log buckets used by the DDoS Response Team (DRT) to access your AWS account while assisting with attack mitigation.
module Network.AWS.Shield.DescribeDRTAccess
  ( -- * Creating a request
    DescribeDRTAccess (..),
    mkDescribeDRTAccess,

    -- * Destructuring the response
    DescribeDRTAccessResponse (..),
    mkDescribeDRTAccessResponse,

    -- ** Response lenses
    ddrtarsLogBucketList,
    ddrtarsRoleARN,
    ddrtarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeDRTAccess' smart constructor.
data DescribeDRTAccess = DescribeDRTAccess'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDRTAccess' with the minimum fields required to make a request.
mkDescribeDRTAccess ::
  DescribeDRTAccess
mkDescribeDRTAccess = DescribeDRTAccess'

instance Lude.AWSRequest DescribeDRTAccess where
  type Rs DescribeDRTAccess = DescribeDRTAccessResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDRTAccessResponse'
            Lude.<$> (x Lude..?> "LogBucketList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDRTAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DescribeDRTAccess" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDRTAccess where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeDRTAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDRTAccess where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDRTAccessResponse' smart constructor.
data DescribeDRTAccessResponse = DescribeDRTAccessResponse'
  { -- | The list of Amazon S3 buckets accessed by the DRT.
    logBucketList :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDRTAccessResponse' with the minimum fields required to make a request.
--
-- * 'logBucketList' - The list of Amazon S3 buckets accessed by the DRT.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
-- * 'responseStatus' - The response status code.
mkDescribeDRTAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDRTAccessResponse
mkDescribeDRTAccessResponse pResponseStatus_ =
  DescribeDRTAccessResponse'
    { logBucketList = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of Amazon S3 buckets accessed by the DRT.
--
-- /Note:/ Consider using 'logBucketList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtarsLogBucketList :: Lens.Lens' DescribeDRTAccessResponse (Lude.Maybe [Lude.Text])
ddrtarsLogBucketList = Lens.lens (logBucketList :: DescribeDRTAccessResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {logBucketList = a} :: DescribeDRTAccessResponse)
{-# DEPRECATED ddrtarsLogBucketList "Use generic-lens or generic-optics with 'logBucketList' instead." #-}

-- | The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtarsRoleARN :: Lens.Lens' DescribeDRTAccessResponse (Lude.Maybe Lude.Text)
ddrtarsRoleARN = Lens.lens (roleARN :: DescribeDRTAccessResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeDRTAccessResponse)
{-# DEPRECATED ddrtarsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtarsResponseStatus :: Lens.Lens' DescribeDRTAccessResponse Lude.Int
ddrtarsResponseStatus = Lens.lens (responseStatus :: DescribeDRTAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDRTAccessResponse)
{-# DEPRECATED ddrtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
