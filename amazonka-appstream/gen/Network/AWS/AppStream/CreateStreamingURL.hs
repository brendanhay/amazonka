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
-- Module      : Network.AWS.AppStream.CreateStreamingURL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary URL to start an AppStream 2.0 streaming session for
-- the specified user. A streaming URL enables application streaming to be
-- tested without user setup.
module Network.AWS.AppStream.CreateStreamingURL
  ( -- * Creating a Request
    CreateStreamingURL (..),
    newCreateStreamingURL,

    -- * Request Lenses
    createStreamingURL_applicationId,
    createStreamingURL_sessionContext,
    createStreamingURL_validity,
    createStreamingURL_stackName,
    createStreamingURL_fleetName,
    createStreamingURL_userId,

    -- * Destructuring the Response
    CreateStreamingURLResponse (..),
    newCreateStreamingURLResponse,

    -- * Response Lenses
    createStreamingURLResponse_streamingURL,
    createStreamingURLResponse_expires,
    createStreamingURLResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStreamingURL' smart constructor.
data CreateStreamingURL = CreateStreamingURL'
  { -- | The name of the application to launch after the session starts. This is
    -- the name that you specified as __Name__ in the Image Assistant. If your
    -- fleet is enabled for the __Desktop__ stream view, you can also choose to
    -- launch directly to the operating system desktop. To do so, specify
    -- __Desktop__.
    applicationId :: Core.Maybe Core.Text,
    -- | The session context. For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    sessionContext :: Core.Maybe Core.Text,
    -- | The time that the streaming URL will be valid, in seconds. Specify a
    -- value between 1 and 604800 seconds. The default is 60 seconds.
    validity :: Core.Maybe Core.Integer,
    -- | The name of the stack.
    stackName :: Core.Text,
    -- | The name of the fleet.
    fleetName :: Core.Text,
    -- | The identifier of the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStreamingURL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createStreamingURL_applicationId' - The name of the application to launch after the session starts. This is
-- the name that you specified as __Name__ in the Image Assistant. If your
-- fleet is enabled for the __Desktop__ stream view, you can also choose to
-- launch directly to the operating system desktop. To do so, specify
-- __Desktop__.
--
-- 'sessionContext', 'createStreamingURL_sessionContext' - The session context. For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'validity', 'createStreamingURL_validity' - The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 60 seconds.
--
-- 'stackName', 'createStreamingURL_stackName' - The name of the stack.
--
-- 'fleetName', 'createStreamingURL_fleetName' - The name of the fleet.
--
-- 'userId', 'createStreamingURL_userId' - The identifier of the user.
newCreateStreamingURL ::
  -- | 'stackName'
  Core.Text ->
  -- | 'fleetName'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  CreateStreamingURL
newCreateStreamingURL
  pStackName_
  pFleetName_
  pUserId_ =
    CreateStreamingURL'
      { applicationId = Core.Nothing,
        sessionContext = Core.Nothing,
        validity = Core.Nothing,
        stackName = pStackName_,
        fleetName = pFleetName_,
        userId = pUserId_
      }

-- | The name of the application to launch after the session starts. This is
-- the name that you specified as __Name__ in the Image Assistant. If your
-- fleet is enabled for the __Desktop__ stream view, you can also choose to
-- launch directly to the operating system desktop. To do so, specify
-- __Desktop__.
createStreamingURL_applicationId :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Text)
createStreamingURL_applicationId = Lens.lens (\CreateStreamingURL' {applicationId} -> applicationId) (\s@CreateStreamingURL' {} a -> s {applicationId = a} :: CreateStreamingURL)

-- | The session context. For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createStreamingURL_sessionContext :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Text)
createStreamingURL_sessionContext = Lens.lens (\CreateStreamingURL' {sessionContext} -> sessionContext) (\s@CreateStreamingURL' {} a -> s {sessionContext = a} :: CreateStreamingURL)

-- | The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 60 seconds.
createStreamingURL_validity :: Lens.Lens' CreateStreamingURL (Core.Maybe Core.Integer)
createStreamingURL_validity = Lens.lens (\CreateStreamingURL' {validity} -> validity) (\s@CreateStreamingURL' {} a -> s {validity = a} :: CreateStreamingURL)

-- | The name of the stack.
createStreamingURL_stackName :: Lens.Lens' CreateStreamingURL Core.Text
createStreamingURL_stackName = Lens.lens (\CreateStreamingURL' {stackName} -> stackName) (\s@CreateStreamingURL' {} a -> s {stackName = a} :: CreateStreamingURL)

-- | The name of the fleet.
createStreamingURL_fleetName :: Lens.Lens' CreateStreamingURL Core.Text
createStreamingURL_fleetName = Lens.lens (\CreateStreamingURL' {fleetName} -> fleetName) (\s@CreateStreamingURL' {} a -> s {fleetName = a} :: CreateStreamingURL)

-- | The identifier of the user.
createStreamingURL_userId :: Lens.Lens' CreateStreamingURL Core.Text
createStreamingURL_userId = Lens.lens (\CreateStreamingURL' {userId} -> userId) (\s@CreateStreamingURL' {} a -> s {userId = a} :: CreateStreamingURL)

instance Core.AWSRequest CreateStreamingURL where
  type
    AWSResponse CreateStreamingURL =
      CreateStreamingURLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingURLResponse'
            Core.<$> (x Core..?> "StreamingURL")
            Core.<*> (x Core..?> "Expires")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateStreamingURL

instance Core.NFData CreateStreamingURL

instance Core.ToHeaders CreateStreamingURL where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateStreamingURL" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStreamingURL where
  toJSON CreateStreamingURL' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApplicationId" Core..=) Core.<$> applicationId,
            ("SessionContext" Core..=) Core.<$> sessionContext,
            ("Validity" Core..=) Core.<$> validity,
            Core.Just ("StackName" Core..= stackName),
            Core.Just ("FleetName" Core..= fleetName),
            Core.Just ("UserId" Core..= userId)
          ]
      )

instance Core.ToPath CreateStreamingURL where
  toPath = Core.const "/"

instance Core.ToQuery CreateStreamingURL where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
  { -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Core.Maybe Core.Text,
    -- | The elapsed time, in seconds after the Unix epoch, when this URL
    -- expires.
    expires :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStreamingURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingURL', 'createStreamingURLResponse_streamingURL' - The URL to start the AppStream 2.0 streaming session.
--
-- 'expires', 'createStreamingURLResponse_expires' - The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
--
-- 'httpStatus', 'createStreamingURLResponse_httpStatus' - The response's http status code.
newCreateStreamingURLResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateStreamingURLResponse
newCreateStreamingURLResponse pHttpStatus_ =
  CreateStreamingURLResponse'
    { streamingURL =
        Core.Nothing,
      expires = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL to start the AppStream 2.0 streaming session.
createStreamingURLResponse_streamingURL :: Lens.Lens' CreateStreamingURLResponse (Core.Maybe Core.Text)
createStreamingURLResponse_streamingURL = Lens.lens (\CreateStreamingURLResponse' {streamingURL} -> streamingURL) (\s@CreateStreamingURLResponse' {} a -> s {streamingURL = a} :: CreateStreamingURLResponse)

-- | The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
createStreamingURLResponse_expires :: Lens.Lens' CreateStreamingURLResponse (Core.Maybe Core.UTCTime)
createStreamingURLResponse_expires = Lens.lens (\CreateStreamingURLResponse' {expires} -> expires) (\s@CreateStreamingURLResponse' {} a -> s {expires = a} :: CreateStreamingURLResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createStreamingURLResponse_httpStatus :: Lens.Lens' CreateStreamingURLResponse Core.Int
createStreamingURLResponse_httpStatus = Lens.lens (\CreateStreamingURLResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingURLResponse' {} a -> s {httpStatus = a} :: CreateStreamingURLResponse)

instance Core.NFData CreateStreamingURLResponse
