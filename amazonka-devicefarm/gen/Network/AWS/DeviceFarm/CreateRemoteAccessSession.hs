{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateRemoteAccessSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies and starts a remote access session.
module Network.AWS.DeviceFarm.CreateRemoteAccessSession
  ( -- * Creating a Request
    CreateRemoteAccessSession (..),
    newCreateRemoteAccessSession,

    -- * Request Lenses
    createRemoteAccessSession_clientId,
    createRemoteAccessSession_interactionMode,
    createRemoteAccessSession_configuration,
    createRemoteAccessSession_name,
    createRemoteAccessSession_instanceArn,
    createRemoteAccessSession_remoteRecordEnabled,
    createRemoteAccessSession_skipAppResign,
    createRemoteAccessSession_sshPublicKey,
    createRemoteAccessSession_remoteDebugEnabled,
    createRemoteAccessSession_remoteRecordAppArn,
    createRemoteAccessSession_projectArn,
    createRemoteAccessSession_deviceArn,

    -- * Destructuring the Response
    CreateRemoteAccessSessionResponse (..),
    newCreateRemoteAccessSessionResponse,

    -- * Response Lenses
    createRemoteAccessSessionResponse_remoteAccessSession,
    createRemoteAccessSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates and submits a request to start a remote access session.
--
-- /See:/ 'newCreateRemoteAccessSession' smart constructor.
data CreateRemoteAccessSession = CreateRemoteAccessSession'
  { -- | Unique identifier for the client. If you want access to multiple devices
    -- on the same client, you should pass the same @clientId@ value in each
    -- call to @CreateRemoteAccessSession@. This identifier is required only if
    -- @remoteDebugEnabled@ is set to @true@.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    clientId :: Core.Maybe Core.Text,
    -- | The interaction mode of the remote access session. Valid values are:
    --
    -- -   INTERACTIVE: You can interact with the iOS device by viewing,
    --     touching, and rotating the screen. You cannot run XCUITest
    --     framework-based tests in this mode.
    --
    -- -   NO_VIDEO: You are connected to the device, but cannot interact with
    --     it or view the screen. This mode has the fastest test execution
    --     speed. You can run XCUITest framework-based tests in this mode.
    --
    -- -   VIDEO_ONLY: You can view the screen, but cannot touch or rotate it.
    --     You can run XCUITest framework-based tests and watch the screen in
    --     this mode.
    interactionMode :: Core.Maybe InteractionMode,
    -- | The configuration information for the remote access session request.
    configuration :: Core.Maybe CreateRemoteAccessSessionConfiguration,
    -- | The name of the remote access session to create.
    name :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the device instance for which you want
    -- to create a remote access session.
    instanceArn :: Core.Maybe Core.Text,
    -- | Set to @true@ to enable remote recording for the remote access session.
    remoteRecordEnabled :: Core.Maybe Core.Bool,
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information on how Device Farm modifies your uploads during
    -- tests, see
    -- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
    skipAppResign :: Core.Maybe Core.Bool,
    -- | Ignored. The public key of the @ssh@ key pair you want to use for
    -- connecting to remote devices in your remote debugging session. This key
    -- is required only if @remoteDebugEnabled@ is set to @true@.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    sshPublicKey :: Core.Maybe Core.Text,
    -- | Set to @true@ if you want to access devices remotely for debugging in
    -- your remote access session.
    --
    -- Remote debugging is
    -- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
    remoteDebugEnabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the app to be recorded in the remote
    -- access session.
    remoteRecordAppArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the project for which you want to
    -- create a remote access session.
    projectArn :: Core.Text,
    -- | The ARN of the device for which you want to create a remote access
    -- session.
    deviceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRemoteAccessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'createRemoteAccessSession_clientId' - Unique identifier for the client. If you want access to multiple devices
-- on the same client, you should pass the same @clientId@ value in each
-- call to @CreateRemoteAccessSession@. This identifier is required only if
-- @remoteDebugEnabled@ is set to @true@.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'interactionMode', 'createRemoteAccessSession_interactionMode' - The interaction mode of the remote access session. Valid values are:
--
-- -   INTERACTIVE: You can interact with the iOS device by viewing,
--     touching, and rotating the screen. You cannot run XCUITest
--     framework-based tests in this mode.
--
-- -   NO_VIDEO: You are connected to the device, but cannot interact with
--     it or view the screen. This mode has the fastest test execution
--     speed. You can run XCUITest framework-based tests in this mode.
--
-- -   VIDEO_ONLY: You can view the screen, but cannot touch or rotate it.
--     You can run XCUITest framework-based tests and watch the screen in
--     this mode.
--
-- 'configuration', 'createRemoteAccessSession_configuration' - The configuration information for the remote access session request.
--
-- 'name', 'createRemoteAccessSession_name' - The name of the remote access session to create.
--
-- 'instanceArn', 'createRemoteAccessSession_instanceArn' - The Amazon Resource Name (ARN) of the device instance for which you want
-- to create a remote access session.
--
-- 'remoteRecordEnabled', 'createRemoteAccessSession_remoteRecordEnabled' - Set to @true@ to enable remote recording for the remote access session.
--
-- 'skipAppResign', 'createRemoteAccessSession_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during
-- tests, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
--
-- 'sshPublicKey', 'createRemoteAccessSession_sshPublicKey' - Ignored. The public key of the @ssh@ key pair you want to use for
-- connecting to remote devices in your remote debugging session. This key
-- is required only if @remoteDebugEnabled@ is set to @true@.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'remoteDebugEnabled', 'createRemoteAccessSession_remoteDebugEnabled' - Set to @true@ if you want to access devices remotely for debugging in
-- your remote access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
--
-- 'remoteRecordAppArn', 'createRemoteAccessSession_remoteRecordAppArn' - The Amazon Resource Name (ARN) for the app to be recorded in the remote
-- access session.
--
-- 'projectArn', 'createRemoteAccessSession_projectArn' - The Amazon Resource Name (ARN) of the project for which you want to
-- create a remote access session.
--
-- 'deviceArn', 'createRemoteAccessSession_deviceArn' - The ARN of the device for which you want to create a remote access
-- session.
newCreateRemoteAccessSession ::
  -- | 'projectArn'
  Core.Text ->
  -- | 'deviceArn'
  Core.Text ->
  CreateRemoteAccessSession
newCreateRemoteAccessSession pProjectArn_ pDeviceArn_ =
  CreateRemoteAccessSession'
    { clientId = Core.Nothing,
      interactionMode = Core.Nothing,
      configuration = Core.Nothing,
      name = Core.Nothing,
      instanceArn = Core.Nothing,
      remoteRecordEnabled = Core.Nothing,
      skipAppResign = Core.Nothing,
      sshPublicKey = Core.Nothing,
      remoteDebugEnabled = Core.Nothing,
      remoteRecordAppArn = Core.Nothing,
      projectArn = pProjectArn_,
      deviceArn = pDeviceArn_
    }

-- | Unique identifier for the client. If you want access to multiple devices
-- on the same client, you should pass the same @clientId@ value in each
-- call to @CreateRemoteAccessSession@. This identifier is required only if
-- @remoteDebugEnabled@ is set to @true@.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
createRemoteAccessSession_clientId :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Text)
createRemoteAccessSession_clientId = Lens.lens (\CreateRemoteAccessSession' {clientId} -> clientId) (\s@CreateRemoteAccessSession' {} a -> s {clientId = a} :: CreateRemoteAccessSession)

-- | The interaction mode of the remote access session. Valid values are:
--
-- -   INTERACTIVE: You can interact with the iOS device by viewing,
--     touching, and rotating the screen. You cannot run XCUITest
--     framework-based tests in this mode.
--
-- -   NO_VIDEO: You are connected to the device, but cannot interact with
--     it or view the screen. This mode has the fastest test execution
--     speed. You can run XCUITest framework-based tests in this mode.
--
-- -   VIDEO_ONLY: You can view the screen, but cannot touch or rotate it.
--     You can run XCUITest framework-based tests and watch the screen in
--     this mode.
createRemoteAccessSession_interactionMode :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe InteractionMode)
createRemoteAccessSession_interactionMode = Lens.lens (\CreateRemoteAccessSession' {interactionMode} -> interactionMode) (\s@CreateRemoteAccessSession' {} a -> s {interactionMode = a} :: CreateRemoteAccessSession)

-- | The configuration information for the remote access session request.
createRemoteAccessSession_configuration :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe CreateRemoteAccessSessionConfiguration)
createRemoteAccessSession_configuration = Lens.lens (\CreateRemoteAccessSession' {configuration} -> configuration) (\s@CreateRemoteAccessSession' {} a -> s {configuration = a} :: CreateRemoteAccessSession)

-- | The name of the remote access session to create.
createRemoteAccessSession_name :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Text)
createRemoteAccessSession_name = Lens.lens (\CreateRemoteAccessSession' {name} -> name) (\s@CreateRemoteAccessSession' {} a -> s {name = a} :: CreateRemoteAccessSession)

-- | The Amazon Resource Name (ARN) of the device instance for which you want
-- to create a remote access session.
createRemoteAccessSession_instanceArn :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Text)
createRemoteAccessSession_instanceArn = Lens.lens (\CreateRemoteAccessSession' {instanceArn} -> instanceArn) (\s@CreateRemoteAccessSession' {} a -> s {instanceArn = a} :: CreateRemoteAccessSession)

-- | Set to @true@ to enable remote recording for the remote access session.
createRemoteAccessSession_remoteRecordEnabled :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
createRemoteAccessSession_remoteRecordEnabled = Lens.lens (\CreateRemoteAccessSession' {remoteRecordEnabled} -> remoteRecordEnabled) (\s@CreateRemoteAccessSession' {} a -> s {remoteRecordEnabled = a} :: CreateRemoteAccessSession)

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information on how Device Farm modifies your uploads during
-- tests, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?>
createRemoteAccessSession_skipAppResign :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
createRemoteAccessSession_skipAppResign = Lens.lens (\CreateRemoteAccessSession' {skipAppResign} -> skipAppResign) (\s@CreateRemoteAccessSession' {} a -> s {skipAppResign = a} :: CreateRemoteAccessSession)

-- | Ignored. The public key of the @ssh@ key pair you want to use for
-- connecting to remote devices in your remote debugging session. This key
-- is required only if @remoteDebugEnabled@ is set to @true@.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
createRemoteAccessSession_sshPublicKey :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Text)
createRemoteAccessSession_sshPublicKey = Lens.lens (\CreateRemoteAccessSession' {sshPublicKey} -> sshPublicKey) (\s@CreateRemoteAccessSession' {} a -> s {sshPublicKey = a} :: CreateRemoteAccessSession)

-- | Set to @true@ if you want to access devices remotely for debugging in
-- your remote access session.
--
-- Remote debugging is
-- <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>.
createRemoteAccessSession_remoteDebugEnabled :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Bool)
createRemoteAccessSession_remoteDebugEnabled = Lens.lens (\CreateRemoteAccessSession' {remoteDebugEnabled} -> remoteDebugEnabled) (\s@CreateRemoteAccessSession' {} a -> s {remoteDebugEnabled = a} :: CreateRemoteAccessSession)

-- | The Amazon Resource Name (ARN) for the app to be recorded in the remote
-- access session.
createRemoteAccessSession_remoteRecordAppArn :: Lens.Lens' CreateRemoteAccessSession (Core.Maybe Core.Text)
createRemoteAccessSession_remoteRecordAppArn = Lens.lens (\CreateRemoteAccessSession' {remoteRecordAppArn} -> remoteRecordAppArn) (\s@CreateRemoteAccessSession' {} a -> s {remoteRecordAppArn = a} :: CreateRemoteAccessSession)

-- | The Amazon Resource Name (ARN) of the project for which you want to
-- create a remote access session.
createRemoteAccessSession_projectArn :: Lens.Lens' CreateRemoteAccessSession Core.Text
createRemoteAccessSession_projectArn = Lens.lens (\CreateRemoteAccessSession' {projectArn} -> projectArn) (\s@CreateRemoteAccessSession' {} a -> s {projectArn = a} :: CreateRemoteAccessSession)

-- | The ARN of the device for which you want to create a remote access
-- session.
createRemoteAccessSession_deviceArn :: Lens.Lens' CreateRemoteAccessSession Core.Text
createRemoteAccessSession_deviceArn = Lens.lens (\CreateRemoteAccessSession' {deviceArn} -> deviceArn) (\s@CreateRemoteAccessSession' {} a -> s {deviceArn = a} :: CreateRemoteAccessSession)

instance Core.AWSRequest CreateRemoteAccessSession where
  type
    AWSResponse CreateRemoteAccessSession =
      CreateRemoteAccessSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRemoteAccessSessionResponse'
            Core.<$> (x Core..?> "remoteAccessSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRemoteAccessSession

instance Core.NFData CreateRemoteAccessSession

instance Core.ToHeaders CreateRemoteAccessSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateRemoteAccessSession" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRemoteAccessSession where
  toJSON CreateRemoteAccessSession' {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientId" Core..=) Core.<$> clientId,
            ("interactionMode" Core..=) Core.<$> interactionMode,
            ("configuration" Core..=) Core.<$> configuration,
            ("name" Core..=) Core.<$> name,
            ("instanceArn" Core..=) Core.<$> instanceArn,
            ("remoteRecordEnabled" Core..=)
              Core.<$> remoteRecordEnabled,
            ("skipAppResign" Core..=) Core.<$> skipAppResign,
            ("sshPublicKey" Core..=) Core.<$> sshPublicKey,
            ("remoteDebugEnabled" Core..=)
              Core.<$> remoteDebugEnabled,
            ("remoteRecordAppArn" Core..=)
              Core.<$> remoteRecordAppArn,
            Core.Just ("projectArn" Core..= projectArn),
            Core.Just ("deviceArn" Core..= deviceArn)
          ]
      )

instance Core.ToPath CreateRemoteAccessSession where
  toPath = Core.const "/"

instance Core.ToQuery CreateRemoteAccessSession where
  toQuery = Core.const Core.mempty

-- | Represents the server response from a request to create a remote access
-- session.
--
-- /See:/ 'newCreateRemoteAccessSessionResponse' smart constructor.
data CreateRemoteAccessSessionResponse = CreateRemoteAccessSessionResponse'
  { -- | A container that describes the remote access session when the request to
    -- create a remote access session is sent.
    remoteAccessSession :: Core.Maybe RemoteAccessSession,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRemoteAccessSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessSession', 'createRemoteAccessSessionResponse_remoteAccessSession' - A container that describes the remote access session when the request to
-- create a remote access session is sent.
--
-- 'httpStatus', 'createRemoteAccessSessionResponse_httpStatus' - The response's http status code.
newCreateRemoteAccessSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRemoteAccessSessionResponse
newCreateRemoteAccessSessionResponse pHttpStatus_ =
  CreateRemoteAccessSessionResponse'
    { remoteAccessSession =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A container that describes the remote access session when the request to
-- create a remote access session is sent.
createRemoteAccessSessionResponse_remoteAccessSession :: Lens.Lens' CreateRemoteAccessSessionResponse (Core.Maybe RemoteAccessSession)
createRemoteAccessSessionResponse_remoteAccessSession = Lens.lens (\CreateRemoteAccessSessionResponse' {remoteAccessSession} -> remoteAccessSession) (\s@CreateRemoteAccessSessionResponse' {} a -> s {remoteAccessSession = a} :: CreateRemoteAccessSessionResponse)

-- | The response's http status code.
createRemoteAccessSessionResponse_httpStatus :: Lens.Lens' CreateRemoteAccessSessionResponse Core.Int
createRemoteAccessSessionResponse_httpStatus = Lens.lens (\CreateRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@CreateRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: CreateRemoteAccessSessionResponse)

instance
  Core.NFData
    CreateRemoteAccessSessionResponse
