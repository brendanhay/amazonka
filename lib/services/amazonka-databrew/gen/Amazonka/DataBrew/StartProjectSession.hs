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
-- Module      : Amazonka.DataBrew.StartProjectSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an interactive session, enabling you to manipulate data in a
-- DataBrew project.
module Amazonka.DataBrew.StartProjectSession
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProjectSessionResponse'
            Prelude.<$> (x Data..?> "ClientSessionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable StartProjectSession where
  hashWithSalt _salt StartProjectSession' {..} =
    _salt `Prelude.hashWithSalt` assumeControl
      `Prelude.hashWithSalt` name

instance Prelude.NFData StartProjectSession where
  rnf StartProjectSession' {..} =
    Prelude.rnf assumeControl
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders StartProjectSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartProjectSession where
  toJSON StartProjectSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssumeControl" Data..=)
              Prelude.<$> assumeControl
          ]
      )

instance Data.ToPath StartProjectSession where
  toPath StartProjectSession' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS name,
        "/startProjectSession"
      ]

instance Data.ToQuery StartProjectSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartProjectSessionResponse' smart constructor.
data StartProjectSessionResponse = StartProjectSessionResponse'
  { -- | A system-generated identifier for the session.
    clientSessionId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the project to be acted upon.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
startProjectSessionResponse_clientSessionId = Lens.lens (\StartProjectSessionResponse' {clientSessionId} -> clientSessionId) (\s@StartProjectSessionResponse' {} a -> s {clientSessionId = a} :: StartProjectSessionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
startProjectSessionResponse_httpStatus :: Lens.Lens' StartProjectSessionResponse Prelude.Int
startProjectSessionResponse_httpStatus = Lens.lens (\StartProjectSessionResponse' {httpStatus} -> httpStatus) (\s@StartProjectSessionResponse' {} a -> s {httpStatus = a} :: StartProjectSessionResponse)

-- | The name of the project to be acted upon.
startProjectSessionResponse_name :: Lens.Lens' StartProjectSessionResponse Prelude.Text
startProjectSessionResponse_name = Lens.lens (\StartProjectSessionResponse' {name} -> name) (\s@StartProjectSessionResponse' {} a -> s {name = a} :: StartProjectSessionResponse)

instance Prelude.NFData StartProjectSessionResponse where
  rnf StartProjectSessionResponse' {..} =
    Prelude.rnf clientSessionId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
