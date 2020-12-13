{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableEBSEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables EBS encryption by default for your account in the current Region.
--
-- After you disable encryption by default, you can still create encrypted volumes by enabling encryption when you create each volume.
-- Disabling encryption by default does not change the encryption status of your existing volumes.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DisableEBSEncryptionByDefault
  ( -- * Creating a request
    DisableEBSEncryptionByDefault (..),
    mkDisableEBSEncryptionByDefault,

    -- ** Request lenses
    deebdDryRun,

    -- * Destructuring the response
    DisableEBSEncryptionByDefaultResponse (..),
    mkDisableEBSEncryptionByDefaultResponse,

    -- ** Response lenses
    deebdrsEBSEncryptionByDefault,
    deebdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableEBSEncryptionByDefault' smart constructor.
newtype DisableEBSEncryptionByDefault = DisableEBSEncryptionByDefault'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableEBSEncryptionByDefault' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisableEBSEncryptionByDefault ::
  DisableEBSEncryptionByDefault
mkDisableEBSEncryptionByDefault =
  DisableEBSEncryptionByDefault' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deebdDryRun :: Lens.Lens' DisableEBSEncryptionByDefault (Lude.Maybe Lude.Bool)
deebdDryRun = Lens.lens (dryRun :: DisableEBSEncryptionByDefault -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisableEBSEncryptionByDefault)
{-# DEPRECATED deebdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisableEBSEncryptionByDefault where
  type
    Rs DisableEBSEncryptionByDefault =
      DisableEBSEncryptionByDefaultResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisableEBSEncryptionByDefaultResponse'
            Lude.<$> (x Lude..@? "ebsEncryptionByDefault")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableEBSEncryptionByDefault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableEBSEncryptionByDefault where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableEBSEncryptionByDefault where
  toQuery DisableEBSEncryptionByDefault' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisableEbsEncryptionByDefault" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDisableEBSEncryptionByDefaultResponse' smart constructor.
data DisableEBSEncryptionByDefaultResponse = DisableEBSEncryptionByDefaultResponse'
  { -- | The updated status of encryption by default.
    ebsEncryptionByDefault :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableEBSEncryptionByDefaultResponse' with the minimum fields required to make a request.
--
-- * 'ebsEncryptionByDefault' - The updated status of encryption by default.
-- * 'responseStatus' - The response status code.
mkDisableEBSEncryptionByDefaultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableEBSEncryptionByDefaultResponse
mkDisableEBSEncryptionByDefaultResponse pResponseStatus_ =
  DisableEBSEncryptionByDefaultResponse'
    { ebsEncryptionByDefault =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated status of encryption by default.
--
-- /Note:/ Consider using 'ebsEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deebdrsEBSEncryptionByDefault :: Lens.Lens' DisableEBSEncryptionByDefaultResponse (Lude.Maybe Lude.Bool)
deebdrsEBSEncryptionByDefault = Lens.lens (ebsEncryptionByDefault :: DisableEBSEncryptionByDefaultResponse -> Lude.Maybe Lude.Bool) (\s a -> s {ebsEncryptionByDefault = a} :: DisableEBSEncryptionByDefaultResponse)
{-# DEPRECATED deebdrsEBSEncryptionByDefault "Use generic-lens or generic-optics with 'ebsEncryptionByDefault' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deebdrsResponseStatus :: Lens.Lens' DisableEBSEncryptionByDefaultResponse Lude.Int
deebdrsResponseStatus = Lens.lens (responseStatus :: DisableEBSEncryptionByDefaultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableEBSEncryptionByDefaultResponse)
{-# DEPRECATED deebdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
