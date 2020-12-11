{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableEBSEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables EBS encryption by default for your account in the current Region.
--
-- After you enable encryption by default, the EBS volumes that you create are are always encrypted, either using the default CMK or the CMK that you specified when you created each volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- You can specify the default CMK for encryption by default using 'ModifyEbsDefaultKmsKeyId' or 'ResetEbsDefaultKmsKeyId' .
-- Enabling encryption by default has no effect on the encryption status of your existing volumes.
-- After you enable encryption by default, you can no longer launch instances using instance types that do not support encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
module Network.AWS.EC2.EnableEBSEncryptionByDefault
  ( -- * Creating a request
    EnableEBSEncryptionByDefault (..),
    mkEnableEBSEncryptionByDefault,

    -- ** Request lenses
    eeebdDryRun,

    -- * Destructuring the response
    EnableEBSEncryptionByDefaultResponse (..),
    mkEnableEBSEncryptionByDefaultResponse,

    -- ** Response lenses
    eeebdrsEBSEncryptionByDefault,
    eeebdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableEBSEncryptionByDefault' smart constructor.
newtype EnableEBSEncryptionByDefault = EnableEBSEncryptionByDefault'
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

-- | Creates a value of 'EnableEBSEncryptionByDefault' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkEnableEBSEncryptionByDefault ::
  EnableEBSEncryptionByDefault
mkEnableEBSEncryptionByDefault =
  EnableEBSEncryptionByDefault' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeebdDryRun :: Lens.Lens' EnableEBSEncryptionByDefault (Lude.Maybe Lude.Bool)
eeebdDryRun = Lens.lens (dryRun :: EnableEBSEncryptionByDefault -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: EnableEBSEncryptionByDefault)
{-# DEPRECATED eeebdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest EnableEBSEncryptionByDefault where
  type
    Rs EnableEBSEncryptionByDefault =
      EnableEBSEncryptionByDefaultResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          EnableEBSEncryptionByDefaultResponse'
            Lude.<$> (x Lude..@? "ebsEncryptionByDefault")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableEBSEncryptionByDefault where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableEBSEncryptionByDefault where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableEBSEncryptionByDefault where
  toQuery EnableEBSEncryptionByDefault' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("EnableEbsEncryptionByDefault" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkEnableEBSEncryptionByDefaultResponse' smart constructor.
data EnableEBSEncryptionByDefaultResponse = EnableEBSEncryptionByDefaultResponse'
  { ebsEncryptionByDefault ::
      Lude.Maybe
        Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableEBSEncryptionByDefaultResponse' with the minimum fields required to make a request.
--
-- * 'ebsEncryptionByDefault' - The updated status of encryption by default.
-- * 'responseStatus' - The response status code.
mkEnableEBSEncryptionByDefaultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableEBSEncryptionByDefaultResponse
mkEnableEBSEncryptionByDefaultResponse pResponseStatus_ =
  EnableEBSEncryptionByDefaultResponse'
    { ebsEncryptionByDefault =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated status of encryption by default.
--
-- /Note:/ Consider using 'ebsEncryptionByDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeebdrsEBSEncryptionByDefault :: Lens.Lens' EnableEBSEncryptionByDefaultResponse (Lude.Maybe Lude.Bool)
eeebdrsEBSEncryptionByDefault = Lens.lens (ebsEncryptionByDefault :: EnableEBSEncryptionByDefaultResponse -> Lude.Maybe Lude.Bool) (\s a -> s {ebsEncryptionByDefault = a} :: EnableEBSEncryptionByDefaultResponse)
{-# DEPRECATED eeebdrsEBSEncryptionByDefault "Use generic-lens or generic-optics with 'ebsEncryptionByDefault' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeebdrsResponseStatus :: Lens.Lens' EnableEBSEncryptionByDefaultResponse Lude.Int
eeebdrsResponseStatus = Lens.lens (responseStatus :: EnableEBSEncryptionByDefaultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableEBSEncryptionByDefaultResponse)
{-# DEPRECATED eeebdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
