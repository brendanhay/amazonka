{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the encrypted administrator password for a running Windows instance.
--
-- The Windows password is generated at boot by the @EC2Config@ service or @EC2Launch@ scripts (Windows Server 2016 and later). This usually only happens the first time an instance is launched. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/UsingConfig_WinAMI.html EC2Config> and <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2launch.html EC2Launch> in the Amazon Elastic Compute Cloud User Guide.
-- For the @EC2Config@ service, the password is not generated for rebundled AMIs unless @Ec2SetPassword@ is enabled before bundling.
-- The password is encrypted using the key pair that you specified when you launched the instance. You must provide the corresponding key pair file.
-- When you launch an instance, password generation and encryption may take a few minutes. If you try to retrieve the password before it's available, the output returns an empty string. We recommend that you wait up to 15 minutes after launching an instance before trying to retrieve the generated password.
module Network.AWS.EC2.GetPasswordData
  ( -- * Creating a request
    GetPasswordData (..),
    mkGetPasswordData,

    -- ** Request lenses
    gpdDryRun,
    gpdInstanceId,

    -- * Destructuring the response
    GetPasswordDataResponse (..),
    mkGetPasswordDataResponse,

    -- ** Response lenses
    gpdrsResponseStatus,
    gpdrsInstanceId,
    gpdrsPasswordData,
    gpdrsTimestamp,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPasswordData' smart constructor.
data GetPasswordData = GetPasswordData'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPasswordData' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the Windows instance.
mkGetPasswordData ::
  -- | 'instanceId'
  Lude.Text ->
  GetPasswordData
mkGetPasswordData pInstanceId_ =
  GetPasswordData'
    { dryRun = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdDryRun :: Lens.Lens' GetPasswordData (Lude.Maybe Lude.Bool)
gpdDryRun = Lens.lens (dryRun :: GetPasswordData -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetPasswordData)
{-# DEPRECATED gpdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Windows instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdInstanceId :: Lens.Lens' GetPasswordData Lude.Text
gpdInstanceId = Lens.lens (instanceId :: GetPasswordData -> Lude.Text) (\s a -> s {instanceId = a} :: GetPasswordData)
{-# DEPRECATED gpdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest GetPasswordData where
  type Rs GetPasswordData = GetPasswordDataResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetPasswordDataResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "instanceId")
            Lude.<*> (x Lude..@ "passwordData")
            Lude.<*> (x Lude..@ "timestamp")
      )

instance Lude.ToHeaders GetPasswordData where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPasswordData where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPasswordData where
  toQuery GetPasswordData' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetPasswordData" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId
      ]

-- | /See:/ 'mkGetPasswordDataResponse' smart constructor.
data GetPasswordDataResponse = GetPasswordDataResponse'
  { responseStatus ::
      Lude.Int,
    instanceId :: Lude.Text,
    passwordData :: Lude.Text,
    timestamp :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPasswordDataResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the Windows instance.
-- * 'passwordData' - The password of the instance. Returns an empty string if the password is not available.
-- * 'responseStatus' - The response status code.
-- * 'timestamp' - The time the data was last updated.
mkGetPasswordDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'instanceId'
  Lude.Text ->
  -- | 'passwordData'
  Lude.Text ->
  -- | 'timestamp'
  Lude.ISO8601 ->
  GetPasswordDataResponse
mkGetPasswordDataResponse
  pResponseStatus_
  pInstanceId_
  pPasswordData_
  pTimestamp_ =
    GetPasswordDataResponse'
      { responseStatus = pResponseStatus_,
        instanceId = pInstanceId_,
        passwordData = pPasswordData_,
        timestamp = pTimestamp_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsResponseStatus :: Lens.Lens' GetPasswordDataResponse Lude.Int
gpdrsResponseStatus = Lens.lens (responseStatus :: GetPasswordDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPasswordDataResponse)
{-# DEPRECATED gpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the Windows instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsInstanceId :: Lens.Lens' GetPasswordDataResponse Lude.Text
gpdrsInstanceId = Lens.lens (instanceId :: GetPasswordDataResponse -> Lude.Text) (\s a -> s {instanceId = a} :: GetPasswordDataResponse)
{-# DEPRECATED gpdrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The password of the instance. Returns an empty string if the password is not available.
--
-- /Note:/ Consider using 'passwordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsPasswordData :: Lens.Lens' GetPasswordDataResponse Lude.Text
gpdrsPasswordData = Lens.lens (passwordData :: GetPasswordDataResponse -> Lude.Text) (\s a -> s {passwordData = a} :: GetPasswordDataResponse)
{-# DEPRECATED gpdrsPasswordData "Use generic-lens or generic-optics with 'passwordData' instead." #-}

-- | The time the data was last updated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrsTimestamp :: Lens.Lens' GetPasswordDataResponse Lude.ISO8601
gpdrsTimestamp = Lens.lens (timestamp :: GetPasswordDataResponse -> Lude.ISO8601) (\s a -> s {timestamp = a} :: GetPasswordDataResponse)
{-# DEPRECATED gpdrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}
