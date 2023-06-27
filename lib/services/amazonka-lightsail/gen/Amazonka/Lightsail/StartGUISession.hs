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
-- Module      : Amazonka.Lightsail.StartGUISession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a graphical user interface (GUI) session that’s used to access
-- a virtual computer’s operating system and application. The session will
-- be active for 1 hour. Use this action to resume the session after it
-- expires.
module Amazonka.Lightsail.StartGUISession
  ( -- * Creating a Request
    StartGUISession (..),
    newStartGUISession,

    -- * Request Lenses
    startGUISession_resourceName,

    -- * Destructuring the Response
    StartGUISessionResponse (..),
    newStartGUISessionResponse,

    -- * Response Lenses
    startGUISessionResponse_operations,
    startGUISessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartGUISession' smart constructor.
data StartGUISession = StartGUISession'
  { -- | The resource name.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartGUISession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'startGUISession_resourceName' - The resource name.
newStartGUISession ::
  -- | 'resourceName'
  Prelude.Text ->
  StartGUISession
newStartGUISession pResourceName_ =
  StartGUISession' {resourceName = pResourceName_}

-- | The resource name.
startGUISession_resourceName :: Lens.Lens' StartGUISession Prelude.Text
startGUISession_resourceName = Lens.lens (\StartGUISession' {resourceName} -> resourceName) (\s@StartGUISession' {} a -> s {resourceName = a} :: StartGUISession)

instance Core.AWSRequest StartGUISession where
  type
    AWSResponse StartGUISession =
      StartGUISessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGUISessionResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartGUISession where
  hashWithSalt _salt StartGUISession' {..} =
    _salt `Prelude.hashWithSalt` resourceName

instance Prelude.NFData StartGUISession where
  rnf StartGUISession' {..} = Prelude.rnf resourceName

instance Data.ToHeaders StartGUISession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.StartGUISession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartGUISession where
  toJSON StartGUISession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath StartGUISession where
  toPath = Prelude.const "/"

instance Data.ToQuery StartGUISession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartGUISessionResponse' smart constructor.
data StartGUISessionResponse = StartGUISessionResponse'
  { -- | The available API operations.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartGUISessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'startGUISessionResponse_operations' - The available API operations.
--
-- 'httpStatus', 'startGUISessionResponse_httpStatus' - The response's http status code.
newStartGUISessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartGUISessionResponse
newStartGUISessionResponse pHttpStatus_ =
  StartGUISessionResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The available API operations.
startGUISessionResponse_operations :: Lens.Lens' StartGUISessionResponse (Prelude.Maybe [Operation])
startGUISessionResponse_operations = Lens.lens (\StartGUISessionResponse' {operations} -> operations) (\s@StartGUISessionResponse' {} a -> s {operations = a} :: StartGUISessionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startGUISessionResponse_httpStatus :: Lens.Lens' StartGUISessionResponse Prelude.Int
startGUISessionResponse_httpStatus = Lens.lens (\StartGUISessionResponse' {httpStatus} -> httpStatus) (\s@StartGUISessionResponse' {} a -> s {httpStatus = a} :: StartGUISessionResponse)

instance Prelude.NFData StartGUISessionResponse where
  rnf StartGUISessionResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
