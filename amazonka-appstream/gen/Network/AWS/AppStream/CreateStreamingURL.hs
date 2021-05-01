{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStreamingURL' smart constructor.
data CreateStreamingURL = CreateStreamingURL'
  { -- | The name of the application to launch after the session starts. This is
    -- the name that you specified as __Name__ in the Image Assistant. If your
    -- fleet is enabled for the __Desktop__ stream view, you can also choose to
    -- launch directly to the operating system desktop. To do so, specify
    -- __Desktop__.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The session context. For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    sessionContext :: Prelude.Maybe Prelude.Text,
    -- | The time that the streaming URL will be valid, in seconds. Specify a
    -- value between 1 and 604800 seconds. The default is 60 seconds.
    validity :: Prelude.Maybe Prelude.Integer,
    -- | The name of the stack.
    stackName :: Prelude.Text,
    -- | The name of the fleet.
    fleetName :: Prelude.Text,
    -- | The identifier of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  CreateStreamingURL
newCreateStreamingURL
  pStackName_
  pFleetName_
  pUserId_ =
    CreateStreamingURL'
      { applicationId =
          Prelude.Nothing,
        sessionContext = Prelude.Nothing,
        validity = Prelude.Nothing,
        stackName = pStackName_,
        fleetName = pFleetName_,
        userId = pUserId_
      }

-- | The name of the application to launch after the session starts. This is
-- the name that you specified as __Name__ in the Image Assistant. If your
-- fleet is enabled for the __Desktop__ stream view, you can also choose to
-- launch directly to the operating system desktop. To do so, specify
-- __Desktop__.
createStreamingURL_applicationId :: Lens.Lens' CreateStreamingURL (Prelude.Maybe Prelude.Text)
createStreamingURL_applicationId = Lens.lens (\CreateStreamingURL' {applicationId} -> applicationId) (\s@CreateStreamingURL' {} a -> s {applicationId = a} :: CreateStreamingURL)

-- | The session context. For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createStreamingURL_sessionContext :: Lens.Lens' CreateStreamingURL (Prelude.Maybe Prelude.Text)
createStreamingURL_sessionContext = Lens.lens (\CreateStreamingURL' {sessionContext} -> sessionContext) (\s@CreateStreamingURL' {} a -> s {sessionContext = a} :: CreateStreamingURL)

-- | The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 60 seconds.
createStreamingURL_validity :: Lens.Lens' CreateStreamingURL (Prelude.Maybe Prelude.Integer)
createStreamingURL_validity = Lens.lens (\CreateStreamingURL' {validity} -> validity) (\s@CreateStreamingURL' {} a -> s {validity = a} :: CreateStreamingURL)

-- | The name of the stack.
createStreamingURL_stackName :: Lens.Lens' CreateStreamingURL Prelude.Text
createStreamingURL_stackName = Lens.lens (\CreateStreamingURL' {stackName} -> stackName) (\s@CreateStreamingURL' {} a -> s {stackName = a} :: CreateStreamingURL)

-- | The name of the fleet.
createStreamingURL_fleetName :: Lens.Lens' CreateStreamingURL Prelude.Text
createStreamingURL_fleetName = Lens.lens (\CreateStreamingURL' {fleetName} -> fleetName) (\s@CreateStreamingURL' {} a -> s {fleetName = a} :: CreateStreamingURL)

-- | The identifier of the user.
createStreamingURL_userId :: Lens.Lens' CreateStreamingURL Prelude.Text
createStreamingURL_userId = Lens.lens (\CreateStreamingURL' {userId} -> userId) (\s@CreateStreamingURL' {} a -> s {userId = a} :: CreateStreamingURL)

instance Prelude.AWSRequest CreateStreamingURL where
  type
    Rs CreateStreamingURL =
      CreateStreamingURLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingURLResponse'
            Prelude.<$> (x Prelude..?> "StreamingURL")
            Prelude.<*> (x Prelude..?> "Expires")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamingURL

instance Prelude.NFData CreateStreamingURL

instance Prelude.ToHeaders CreateStreamingURL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.CreateStreamingURL" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateStreamingURL where
  toJSON CreateStreamingURL' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ApplicationId" Prelude..=)
              Prelude.<$> applicationId,
            ("SessionContext" Prelude..=)
              Prelude.<$> sessionContext,
            ("Validity" Prelude..=) Prelude.<$> validity,
            Prelude.Just ("StackName" Prelude..= stackName),
            Prelude.Just ("FleetName" Prelude..= fleetName),
            Prelude.Just ("UserId" Prelude..= userId)
          ]
      )

instance Prelude.ToPath CreateStreamingURL where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStreamingURL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
  { -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Prelude.Maybe Prelude.Text,
    -- | The elapsed time, in seconds after the Unix epoch, when this URL
    -- expires.
    expires :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateStreamingURLResponse
newCreateStreamingURLResponse pHttpStatus_ =
  CreateStreamingURLResponse'
    { streamingURL =
        Prelude.Nothing,
      expires = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL to start the AppStream 2.0 streaming session.
createStreamingURLResponse_streamingURL :: Lens.Lens' CreateStreamingURLResponse (Prelude.Maybe Prelude.Text)
createStreamingURLResponse_streamingURL = Lens.lens (\CreateStreamingURLResponse' {streamingURL} -> streamingURL) (\s@CreateStreamingURLResponse' {} a -> s {streamingURL = a} :: CreateStreamingURLResponse)

-- | The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
createStreamingURLResponse_expires :: Lens.Lens' CreateStreamingURLResponse (Prelude.Maybe Prelude.UTCTime)
createStreamingURLResponse_expires = Lens.lens (\CreateStreamingURLResponse' {expires} -> expires) (\s@CreateStreamingURLResponse' {} a -> s {expires = a} :: CreateStreamingURLResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
createStreamingURLResponse_httpStatus :: Lens.Lens' CreateStreamingURLResponse Prelude.Int
createStreamingURLResponse_httpStatus = Lens.lens (\CreateStreamingURLResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingURLResponse' {} a -> s {httpStatus = a} :: CreateStreamingURLResponse)

instance Prelude.NFData CreateStreamingURLResponse
