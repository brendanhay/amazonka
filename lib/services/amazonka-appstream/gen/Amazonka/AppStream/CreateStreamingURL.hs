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
-- Module      : Amazonka.AppStream.CreateStreamingURL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary URL to start an AppStream 2.0 streaming session for
-- the specified user. A streaming URL enables application streaming to be
-- tested without user setup.
module Amazonka.AppStream.CreateStreamingURL
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
    createStreamingURLResponse_expires,
    createStreamingURLResponse_streamingURL,
    createStreamingURLResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CreateStreamingURL where
  type
    AWSResponse CreateStreamingURL =
      CreateStreamingURLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingURLResponse'
            Prelude.<$> (x Data..?> "Expires")
            Prelude.<*> (x Data..?> "StreamingURL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamingURL where
  hashWithSalt _salt CreateStreamingURL' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` sessionContext
      `Prelude.hashWithSalt` validity
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` userId

instance Prelude.NFData CreateStreamingURL where
  rnf CreateStreamingURL' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf sessionContext
      `Prelude.seq` Prelude.rnf validity
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders CreateStreamingURL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateStreamingURL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStreamingURL where
  toJSON CreateStreamingURL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationId" Data..=) Prelude.<$> applicationId,
            ("SessionContext" Data..=)
              Prelude.<$> sessionContext,
            ("Validity" Data..=) Prelude.<$> validity,
            Prelude.Just ("StackName" Data..= stackName),
            Prelude.Just ("FleetName" Data..= fleetName),
            Prelude.Just ("UserId" Data..= userId)
          ]
      )

instance Data.ToPath CreateStreamingURL where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStreamingURL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
  { -- | The elapsed time, in seconds after the Unix epoch, when this URL
    -- expires.
    expires :: Prelude.Maybe Data.POSIX,
    -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expires', 'createStreamingURLResponse_expires' - The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
--
-- 'streamingURL', 'createStreamingURLResponse_streamingURL' - The URL to start the AppStream 2.0 streaming session.
--
-- 'httpStatus', 'createStreamingURLResponse_httpStatus' - The response's http status code.
newCreateStreamingURLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamingURLResponse
newCreateStreamingURLResponse pHttpStatus_ =
  CreateStreamingURLResponse'
    { expires =
        Prelude.Nothing,
      streamingURL = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
createStreamingURLResponse_expires :: Lens.Lens' CreateStreamingURLResponse (Prelude.Maybe Prelude.UTCTime)
createStreamingURLResponse_expires = Lens.lens (\CreateStreamingURLResponse' {expires} -> expires) (\s@CreateStreamingURLResponse' {} a -> s {expires = a} :: CreateStreamingURLResponse) Prelude.. Lens.mapping Data._Time

-- | The URL to start the AppStream 2.0 streaming session.
createStreamingURLResponse_streamingURL :: Lens.Lens' CreateStreamingURLResponse (Prelude.Maybe Prelude.Text)
createStreamingURLResponse_streamingURL = Lens.lens (\CreateStreamingURLResponse' {streamingURL} -> streamingURL) (\s@CreateStreamingURLResponse' {} a -> s {streamingURL = a} :: CreateStreamingURLResponse)

-- | The response's http status code.
createStreamingURLResponse_httpStatus :: Lens.Lens' CreateStreamingURLResponse Prelude.Int
createStreamingURLResponse_httpStatus = Lens.lens (\CreateStreamingURLResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingURLResponse' {} a -> s {httpStatus = a} :: CreateStreamingURLResponse)

instance Prelude.NFData CreateStreamingURLResponse where
  rnf CreateStreamingURLResponse' {..} =
    Prelude.rnf expires
      `Prelude.seq` Prelude.rnf streamingURL
      `Prelude.seq` Prelude.rnf httpStatus
