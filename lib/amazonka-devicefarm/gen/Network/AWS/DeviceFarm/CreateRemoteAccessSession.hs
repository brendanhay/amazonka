{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies and starts a remote access session.
module Network.AWS.DeviceFarm.CreateRemoteAccessSession
  ( -- * Creating a request
    CreateRemoteAccessSession (..),
    mkCreateRemoteAccessSession,

    -- ** Request lenses
    crasClientId,
    crasSkipAppResign,
    crasInstanceARN,
    crasRemoteRecordEnabled,
    crasDeviceARN,
    crasRemoteRecordAppARN,
    crasSshPublicKey,
    crasName,
    crasProjectARN,
    crasRemoteDebugEnabled,
    crasConfiguration,
    crasInteractionMode,

    -- * Destructuring the response
    CreateRemoteAccessSessionResponse (..),
    mkCreateRemoteAccessSessionResponse,

    -- ** Response lenses
    crasrsRemoteAccessSession,
    crasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates and submits a request to start a remote access session.
--
-- /See:/ 'mkCreateRemoteAccessSession' smart constructor.
data CreateRemoteAccessSession = CreateRemoteAccessSession'
  { -- | Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    clientId :: Lude.Maybe Lude.Text,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
    skipAppResign :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
    instanceARN :: Lude.Maybe Lude.Text,
    -- | Set to @true@ to enable remote recording for the remote access session.
    remoteRecordEnabled :: Lude.Maybe Lude.Bool,
    -- | The ARN of the device for which you want to create a remote access session.
    deviceARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
    remoteRecordAppARN :: Lude.Maybe Lude.Text,
    -- | Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The name of the remote access session to create.
    name :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
    projectARN :: Lude.Text,
    -- | Set to @true@ if you want to access devices remotely for debugging in your remote access session.
    --
    -- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
    remoteDebugEnabled :: Lude.Maybe Lude.Bool,
    -- | The configuration information for the remote access session request.
    configuration :: Lude.Maybe CreateRemoteAccessSessionConfiguration,
    -- | The interaction mode of the remote access session. Valid values are:
    --
    --
    --     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.
    --
    --
    --     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.
    --
    --
    --     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
    interactionMode :: Lude.Maybe InteractionMode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRemoteAccessSession' with the minimum fields required to make a request.
--
-- * 'clientId' - Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'skipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
-- * 'instanceARN' - The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
-- * 'remoteRecordEnabled' - Set to @true@ to enable remote recording for the remote access session.
-- * 'deviceARN' - The ARN of the device for which you want to create a remote access session.
-- * 'remoteRecordAppARN' - The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
-- * 'sshPublicKey' - Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'name' - The name of the remote access session to create.
-- * 'projectARN' - The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
-- * 'remoteDebugEnabled' - Set to @true@ if you want to access devices remotely for debugging in your remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
-- * 'configuration' - The configuration information for the remote access session request.
-- * 'interactionMode' - The interaction mode of the remote access session. Valid values are:
--
--
--     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.
--
--
--     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.
--
--
--     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
mkCreateRemoteAccessSession ::
  -- | 'deviceARN'
  Lude.Text ->
  -- | 'projectARN'
  Lude.Text ->
  CreateRemoteAccessSession
mkCreateRemoteAccessSession pDeviceARN_ pProjectARN_ =
  CreateRemoteAccessSession'
    { clientId = Lude.Nothing,
      skipAppResign = Lude.Nothing,
      instanceARN = Lude.Nothing,
      remoteRecordEnabled = Lude.Nothing,
      deviceARN = pDeviceARN_,
      remoteRecordAppARN = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      name = Lude.Nothing,
      projectARN = pProjectARN_,
      remoteDebugEnabled = Lude.Nothing,
      configuration = Lude.Nothing,
      interactionMode = Lude.Nothing
    }

-- | Unique identifier for the client. If you want access to multiple devices on the same client, you should pass the same @clientId@ value in each call to @CreateRemoteAccessSession@ . This identifier is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasClientId :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Text)
crasClientId = Lens.lens (clientId :: CreateRemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during tests, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasSkipAppResign :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Bool)
crasSkipAppResign = Lens.lens (skipAppResign :: CreateRemoteAccessSession -> Lude.Maybe Lude.Bool) (\s a -> s {skipAppResign = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | The Amazon Resource Name (ARN) of the device instance for which you want to create a remote access session.
--
-- /Note:/ Consider using 'instanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasInstanceARN :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Text)
crasInstanceARN = Lens.lens (instanceARN :: CreateRemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {instanceARN = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasInstanceARN "Use generic-lens or generic-optics with 'instanceARN' instead." #-}

-- | Set to @true@ to enable remote recording for the remote access session.
--
-- /Note:/ Consider using 'remoteRecordEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteRecordEnabled :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Bool)
crasRemoteRecordEnabled = Lens.lens (remoteRecordEnabled :: CreateRemoteAccessSession -> Lude.Maybe Lude.Bool) (\s a -> s {remoteRecordEnabled = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasRemoteRecordEnabled "Use generic-lens or generic-optics with 'remoteRecordEnabled' instead." #-}

-- | The ARN of the device for which you want to create a remote access session.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasDeviceARN :: Lens.Lens' CreateRemoteAccessSession Lude.Text
crasDeviceARN = Lens.lens (deviceARN :: CreateRemoteAccessSession -> Lude.Text) (\s a -> s {deviceARN = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | The Amazon Resource Name (ARN) for the app to be recorded in the remote access session.
--
-- /Note:/ Consider using 'remoteRecordAppARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteRecordAppARN :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Text)
crasRemoteRecordAppARN = Lens.lens (remoteRecordAppARN :: CreateRemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {remoteRecordAppARN = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasRemoteRecordAppARN "Use generic-lens or generic-optics with 'remoteRecordAppARN' instead." #-}

-- | Ignored. The public key of the @ssh@ key pair you want to use for connecting to remote devices in your remote debugging session. This key is required only if @remoteDebugEnabled@ is set to @true@ .
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasSshPublicKey :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Text)
crasSshPublicKey = Lens.lens (sshPublicKey :: CreateRemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The name of the remote access session to create.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasName :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Text)
crasName = Lens.lens (name :: CreateRemoteAccessSession -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the project for which you want to create a remote access session.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasProjectARN :: Lens.Lens' CreateRemoteAccessSession Lude.Text
crasProjectARN = Lens.lens (projectARN :: CreateRemoteAccessSession -> Lude.Text) (\s a -> s {projectARN = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | Set to @true@ if you want to access devices remotely for debugging in your remote access session.
--
-- Remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> .
--
-- /Note:/ Consider using 'remoteDebugEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasRemoteDebugEnabled :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe Lude.Bool)
crasRemoteDebugEnabled = Lens.lens (remoteDebugEnabled :: CreateRemoteAccessSession -> Lude.Maybe Lude.Bool) (\s a -> s {remoteDebugEnabled = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasRemoteDebugEnabled "Use generic-lens or generic-optics with 'remoteDebugEnabled' instead." #-}

-- | The configuration information for the remote access session request.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasConfiguration :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe CreateRemoteAccessSessionConfiguration)
crasConfiguration = Lens.lens (configuration :: CreateRemoteAccessSession -> Lude.Maybe CreateRemoteAccessSessionConfiguration) (\s a -> s {configuration = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The interaction mode of the remote access session. Valid values are:
--
--
--     * INTERACTIVE: You can interact with the iOS device by viewing, touching, and rotating the screen. You cannot run XCUITest framework-based tests in this mode.
--
--
--     * NO_VIDEO: You are connected to the device, but cannot interact with it or view the screen. This mode has the fastest test execution speed. You can run XCUITest framework-based tests in this mode.
--
--
--     * VIDEO_ONLY: You can view the screen, but cannot touch or rotate it. You can run XCUITest framework-based tests and watch the screen in this mode.
--
--
--
-- /Note:/ Consider using 'interactionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasInteractionMode :: Lens.Lens' CreateRemoteAccessSession (Lude.Maybe InteractionMode)
crasInteractionMode = Lens.lens (interactionMode :: CreateRemoteAccessSession -> Lude.Maybe InteractionMode) (\s a -> s {interactionMode = a} :: CreateRemoteAccessSession)
{-# DEPRECATED crasInteractionMode "Use generic-lens or generic-optics with 'interactionMode' instead." #-}

instance Lude.AWSRequest CreateRemoteAccessSession where
  type
    Rs CreateRemoteAccessSession =
      CreateRemoteAccessSessionResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRemoteAccessSessionResponse'
            Lude.<$> (x Lude..?> "remoteAccessSession")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRemoteAccessSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.CreateRemoteAccessSession" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRemoteAccessSession where
  toJSON CreateRemoteAccessSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientId" Lude..=) Lude.<$> clientId,
            ("skipAppResign" Lude..=) Lude.<$> skipAppResign,
            ("instanceArn" Lude..=) Lude.<$> instanceARN,
            ("remoteRecordEnabled" Lude..=) Lude.<$> remoteRecordEnabled,
            Lude.Just ("deviceArn" Lude..= deviceARN),
            ("remoteRecordAppArn" Lude..=) Lude.<$> remoteRecordAppARN,
            ("sshPublicKey" Lude..=) Lude.<$> sshPublicKey,
            ("name" Lude..=) Lude.<$> name,
            Lude.Just ("projectArn" Lude..= projectARN),
            ("remoteDebugEnabled" Lude..=) Lude.<$> remoteDebugEnabled,
            ("configuration" Lude..=) Lude.<$> configuration,
            ("interactionMode" Lude..=) Lude.<$> interactionMode
          ]
      )

instance Lude.ToPath CreateRemoteAccessSession where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRemoteAccessSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the server response from a request to create a remote access session.
--
-- /See:/ 'mkCreateRemoteAccessSessionResponse' smart constructor.
data CreateRemoteAccessSessionResponse = CreateRemoteAccessSessionResponse'
  { -- | A container that describes the remote access session when the request to create a remote access session is sent.
    remoteAccessSession :: Lude.Maybe RemoteAccessSession,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- * 'remoteAccessSession' - A container that describes the remote access session when the request to create a remote access session is sent.
-- * 'responseStatus' - The response status code.
mkCreateRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRemoteAccessSessionResponse
mkCreateRemoteAccessSessionResponse pResponseStatus_ =
  CreateRemoteAccessSessionResponse'
    { remoteAccessSession =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A container that describes the remote access session when the request to create a remote access session is sent.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasrsRemoteAccessSession :: Lens.Lens' CreateRemoteAccessSessionResponse (Lude.Maybe RemoteAccessSession)
crasrsRemoteAccessSession = Lens.lens (remoteAccessSession :: CreateRemoteAccessSessionResponse -> Lude.Maybe RemoteAccessSession) (\s a -> s {remoteAccessSession = a} :: CreateRemoteAccessSessionResponse)
{-# DEPRECATED crasrsRemoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crasrsResponseStatus :: Lens.Lens' CreateRemoteAccessSessionResponse Lude.Int
crasrsResponseStatus = Lens.lens (responseStatus :: CreateRemoteAccessSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRemoteAccessSessionResponse)
{-# DEPRECATED crasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
