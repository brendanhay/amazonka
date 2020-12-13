{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetEBSDefaultKMSKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default customer master key (CMK) for EBS encryption by default for your account in this Region. You can change the default CMK for encryption by default using 'ModifyEbsDefaultKmsKeyId' or 'ResetEbsDefaultKmsKeyId' .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetEBSDefaultKMSKeyId
  ( -- * Creating a request
    GetEBSDefaultKMSKeyId (..),
    mkGetEBSDefaultKMSKeyId,

    -- ** Request lenses
    gedkkiDryRun,

    -- * Destructuring the response
    GetEBSDefaultKMSKeyIdResponse (..),
    mkGetEBSDefaultKMSKeyIdResponse,

    -- ** Response lenses
    gedkkirsKMSKeyId,
    gedkkirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEBSDefaultKMSKeyId' smart constructor.
newtype GetEBSDefaultKMSKeyId = GetEBSDefaultKMSKeyId'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEBSDefaultKMSKeyId' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetEBSDefaultKMSKeyId ::
  GetEBSDefaultKMSKeyId
mkGetEBSDefaultKMSKeyId =
  GetEBSDefaultKMSKeyId' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gedkkiDryRun :: Lens.Lens' GetEBSDefaultKMSKeyId (Lude.Maybe Lude.Bool)
gedkkiDryRun = Lens.lens (dryRun :: GetEBSDefaultKMSKeyId -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetEBSDefaultKMSKeyId)
{-# DEPRECATED gedkkiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetEBSDefaultKMSKeyId where
  type Rs GetEBSDefaultKMSKeyId = GetEBSDefaultKMSKeyIdResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetEBSDefaultKMSKeyIdResponse'
            Lude.<$> (x Lude..@? "kmsKeyId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEBSDefaultKMSKeyId where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetEBSDefaultKMSKeyId where
  toPath = Lude.const "/"

instance Lude.ToQuery GetEBSDefaultKMSKeyId where
  toQuery GetEBSDefaultKMSKeyId' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetEbsDefaultKmsKeyId" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetEBSDefaultKMSKeyIdResponse' smart constructor.
data GetEBSDefaultKMSKeyIdResponse = GetEBSDefaultKMSKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default CMK for encryption by default.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEBSDefaultKMSKeyIdResponse' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the default CMK for encryption by default.
-- * 'responseStatus' - The response status code.
mkGetEBSDefaultKMSKeyIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEBSDefaultKMSKeyIdResponse
mkGetEBSDefaultKMSKeyIdResponse pResponseStatus_ =
  GetEBSDefaultKMSKeyIdResponse'
    { kmsKeyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by default.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gedkkirsKMSKeyId :: Lens.Lens' GetEBSDefaultKMSKeyIdResponse (Lude.Maybe Lude.Text)
gedkkirsKMSKeyId = Lens.lens (kmsKeyId :: GetEBSDefaultKMSKeyIdResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: GetEBSDefaultKMSKeyIdResponse)
{-# DEPRECATED gedkkirsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gedkkirsResponseStatus :: Lens.Lens' GetEBSDefaultKMSKeyIdResponse Lude.Int
gedkkirsResponseStatus = Lens.lens (responseStatus :: GetEBSDefaultKMSKeyIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEBSDefaultKMSKeyIdResponse)
{-# DEPRECATED gedkkirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
