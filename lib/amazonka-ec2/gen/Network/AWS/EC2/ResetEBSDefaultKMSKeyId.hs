{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetEBSDefaultKMSKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the default customer master key (CMK) for EBS encryption for your account in this Region to the AWS managed CMK for EBS.
--
-- After resetting the default CMK to the AWS managed CMK, you can continue to encrypt by a customer managed CMK by specifying it when you create the volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ResetEBSDefaultKMSKeyId
  ( -- * Creating a request
    ResetEBSDefaultKMSKeyId (..),
    mkResetEBSDefaultKMSKeyId,

    -- ** Request lenses
    redkkiDryRun,

    -- * Destructuring the response
    ResetEBSDefaultKMSKeyIdResponse (..),
    mkResetEBSDefaultKMSKeyIdResponse,

    -- ** Response lenses
    redkkirsKMSKeyId,
    redkkirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetEBSDefaultKMSKeyId' smart constructor.
newtype ResetEBSDefaultKMSKeyId = ResetEBSDefaultKMSKeyId'
  { dryRun ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetEBSDefaultKMSKeyId' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkResetEBSDefaultKMSKeyId ::
  ResetEBSDefaultKMSKeyId
mkResetEBSDefaultKMSKeyId =
  ResetEBSDefaultKMSKeyId' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
redkkiDryRun :: Lens.Lens' ResetEBSDefaultKMSKeyId (Lude.Maybe Lude.Bool)
redkkiDryRun = Lens.lens (dryRun :: ResetEBSDefaultKMSKeyId -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ResetEBSDefaultKMSKeyId)
{-# DEPRECATED redkkiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ResetEBSDefaultKMSKeyId where
  type Rs ResetEBSDefaultKMSKeyId = ResetEBSDefaultKMSKeyIdResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ResetEBSDefaultKMSKeyIdResponse'
            Lude.<$> (x Lude..@? "kmsKeyId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetEBSDefaultKMSKeyId where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetEBSDefaultKMSKeyId where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetEBSDefaultKMSKeyId where
  toQuery ResetEBSDefaultKMSKeyId' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResetEbsDefaultKmsKeyId" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkResetEBSDefaultKMSKeyIdResponse' smart constructor.
data ResetEBSDefaultKMSKeyIdResponse = ResetEBSDefaultKMSKeyIdResponse'
  { kmsKeyId ::
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

-- | Creates a value of 'ResetEBSDefaultKMSKeyIdResponse' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the default CMK for EBS encryption by default.
-- * 'responseStatus' - The response status code.
mkResetEBSDefaultKMSKeyIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetEBSDefaultKMSKeyIdResponse
mkResetEBSDefaultKMSKeyIdResponse pResponseStatus_ =
  ResetEBSDefaultKMSKeyIdResponse'
    { kmsKeyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for EBS encryption by default.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
redkkirsKMSKeyId :: Lens.Lens' ResetEBSDefaultKMSKeyIdResponse (Lude.Maybe Lude.Text)
redkkirsKMSKeyId = Lens.lens (kmsKeyId :: ResetEBSDefaultKMSKeyIdResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ResetEBSDefaultKMSKeyIdResponse)
{-# DEPRECATED redkkirsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
redkkirsResponseStatus :: Lens.Lens' ResetEBSDefaultKMSKeyIdResponse Lude.Int
redkkirsResponseStatus = Lens.lens (responseStatus :: ResetEBSDefaultKMSKeyIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetEBSDefaultKMSKeyIdResponse)
{-# DEPRECATED redkkirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
