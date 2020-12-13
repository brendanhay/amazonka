{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetEBSEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether EBS encryption by default is enabled for your account in the current Region.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetEBSEncryptionByDefault
  ( -- * Creating a request
    GetEBSEncryptionByDefault (..),
    mkGetEBSEncryptionByDefault,

    -- ** Request lenses
    geebdDryRun,

    -- * Destructuring the response
    GetEBSEncryptionByDefaultResponse (..),
    mkGetEBSEncryptionByDefaultResponse,

    -- ** Response lenses
    geebdrsEBSEncryptionByDefault,
    geebdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEBSEncryptionByDefault' smart constructor.
newtype GetEBSEncryptionByDefault = GetEBSEncryptionByDefault'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEBSEncryptionByDefault' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetEBSEncryptionByDefault ::
  GetEBSEncryptionByDefault
mkGetEBSEncryptionByDefault =
  GetEBSEncryptionByDefault' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geebdDryRun :: Lens.Lens' GetEBSEncryptionByDefault (Lude.Maybe Lude.Bool)
geebdDryRun = Lens.lens (dryRun :: GetEBSEncryptionByDefault -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetEBSEncryptionByDefault)
{-# DEPRECATED geebdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetEBSEncryptionByDefault where
  type
    Rs GetEBSEncryptionByDefault =
      GetEBSEncryptionByDefaultResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetEBSEncryptionByDefaultResponse'
            Lude.<$> (x Lude..@? "ebsEncryptionByDefault")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEBSEncryptionByDefault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetEBSEncryptionByDefault where
  toPath = Lude.const "/"

instance Lude.ToQuery GetEBSEncryptionByDefault where
  toQuery GetEBSEncryptionByDefault' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetEbsEncryptionByDefault" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetEBSEncryptionByDefaultResponse' smart constructor.
data GetEBSEncryptionByDefaultResponse = GetEBSEncryptionByDefaultResponse'
  { -- | Indicates whether encryption by default is enabled.
    ebsEncryptionByDefault :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEBSEncryptionByDefaultResponse' with the minimum fields required to make a request.
--
-- * 'ebsEncryptionByDefault' - Indicates whether encryption by default is enabled.
-- * 'responseStatus' - The response status code.
mkGetEBSEncryptionByDefaultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEBSEncryptionByDefaultResponse
mkGetEBSEncryptionByDefaultResponse pResponseStatus_ =
  GetEBSEncryptionByDefaultResponse'
    { ebsEncryptionByDefault =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether encryption by default is enabled.
--
-- /Note:/ Consider using 'ebsEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geebdrsEBSEncryptionByDefault :: Lens.Lens' GetEBSEncryptionByDefaultResponse (Lude.Maybe Lude.Bool)
geebdrsEBSEncryptionByDefault = Lens.lens (ebsEncryptionByDefault :: GetEBSEncryptionByDefaultResponse -> Lude.Maybe Lude.Bool) (\s a -> s {ebsEncryptionByDefault = a} :: GetEBSEncryptionByDefaultResponse)
{-# DEPRECATED geebdrsEBSEncryptionByDefault "Use generic-lens or generic-optics with 'ebsEncryptionByDefault' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geebdrsResponseStatus :: Lens.Lens' GetEBSEncryptionByDefaultResponse Lude.Int
geebdrsResponseStatus = Lens.lens (responseStatus :: GetEBSEncryptionByDefaultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEBSEncryptionByDefaultResponse)
{-# DEPRECATED geebdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
