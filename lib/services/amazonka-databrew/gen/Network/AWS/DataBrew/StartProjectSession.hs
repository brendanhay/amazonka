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
-- Module      : Network.AWS.DataBrew.StartProjectSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an interactive session, enabling you to manipulate data in a
-- DataBrew project.
module Network.AWS.DataBrew.StartProjectSession
  ( -- * Creating a Request
    StartProjectSession (..),
    newStartProjectSession,

    -- * Request Lenses
    startProjectSession_assumeControl,
    startProjectSession_name,

    -- * Destructuring the Response
    StartProjectSessionResponse (..),
    newStartProjectSessionResponse,

    -- * Response Lenses
    startProjectSessionResponse_clientSessionId,
    startProjectSessionResponse_httpStatus,
    startProjectSessionResponse_name,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataBrew.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartProjectSession' smart constructor.
data StartProjectSession = StartProjectSession'
  { -- | A value that, if true, enables you to take control of a session, even if
    -- a different client is currently accessing the project.
    assumeControl :: Prelude.Maybe Prelude.Bool,
    -- | The name of the project to act upon.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProjectSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumeControl', 'startProjectSession_assumeControl' - A value that, if true, enables you to take control of a session, even if
-- a different client is currently accessing the project.
--
-- 'name', 'startProjectSession_name' - The name of the project to act upon.
newStartProjectSession ::
  -- | 'name'
  Prelude.Text ->
  StartProjectSession
newStartProjectSession pName_ =
  StartProjectSession'
    { assumeControl =
        Prelude.Nothing,
      name = pName_
    }

-- | A value that, if true, enables you to take control of a session, even if
-- a different client is currently accessing the project.
startProjectSession_assumeControl :: Lens.Lens' StartProjectSession (Prelude.Maybe Prelude.Bool)
startProjectSession_assumeControl = Lens.lens (\StartProjectSession' {assumeControl} -> assumeControl) (\s@StartProjectSession' {} a -> s {assumeControl = a} :: StartProjectSession)

-- | The name of the project to act upon.
startProjectSession_name :: Lens.Lens' StartProjectSession Prelude.Text
startProjectSession_name = Lens.lens (\StartProjectSession' {name} -> name) (\s@StartProjectSession' {} a -> s {name = a} :: StartProjectSession)

instance Core.AWSRequest StartProjectSession where
  type
    AWSResponse StartProjectSession =
      StartProjectSessionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProjectSessionResponse'
            Prelude.<$> (x Core..?> "ClientSessionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
      )

instance Prelude.Hashable StartProjectSession

instance Prelude.NFData StartProjectSession

instance Core.ToHeaders StartProjectSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartProjectSession where
  toJSON StartProjectSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AssumeControl" Core..=)
              Prelude.<$> assumeControl
          ]
      )

instance Core.ToPath StartProjectSession where
  toPath StartProjectSession' {..} =
    Prelude.mconcat
      [ "/projects/",
        Core.toBS name,
        "/startProjectSession"
      ]

instance Core.ToQuery StartProjectSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartProjectSessionResponse' smart constructor.
data StartProjectSessionResponse = StartProjectSessionResponse'
  { -- | A system-generated identifier for the session.
    clientSessionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the project to be acted upon.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProjectSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientSessionId', 'startProjectSessionResponse_clientSessionId' - A system-generated identifier for the session.
--
-- 'httpStatus', 'startProjectSessionResponse_httpStatus' - The response's http status code.
--
-- 'name', 'startProjectSessionResponse_name' - The name of the project to be acted upon.
newStartProjectSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  StartProjectSessionResponse
newStartProjectSessionResponse pHttpStatus_ pName_ =
  StartProjectSessionResponse'
    { clientSessionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      name = pName_
    }

-- | A system-generated identifier for the session.
startProjectSessionResponse_clientSessionId :: Lens.Lens' StartProjectSessionResponse (Prelude.Maybe Prelude.Text)
startProjectSessionResponse_clientSessionId = Lens.lens (\StartProjectSessionResponse' {clientSessionId} -> clientSessionId) (\s@StartProjectSessionResponse' {} a -> s {clientSessionId = a} :: StartProjectSessionResponse)

-- | The response's http status code.
startProjectSessionResponse_httpStatus :: Lens.Lens' StartProjectSessionResponse Prelude.Int
startProjectSessionResponse_httpStatus = Lens.lens (\StartProjectSessionResponse' {httpStatus} -> httpStatus) (\s@StartProjectSessionResponse' {} a -> s {httpStatus = a} :: StartProjectSessionResponse)

-- | The name of the project to be acted upon.
startProjectSessionResponse_name :: Lens.Lens' StartProjectSessionResponse Prelude.Text
startProjectSessionResponse_name = Lens.lens (\StartProjectSessionResponse' {name} -> name) (\s@StartProjectSessionResponse' {} a -> s {name = a} :: StartProjectSessionResponse)

instance Prelude.NFData StartProjectSessionResponse
