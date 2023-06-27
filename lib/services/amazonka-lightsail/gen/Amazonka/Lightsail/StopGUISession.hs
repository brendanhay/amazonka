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
-- Module      : Amazonka.Lightsail.StopGUISession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a web-based NICE DCV session that’s used to access a virtual
-- computer’s operating system or application. The session will close and
-- any unsaved data will be lost.
module Amazonka.Lightsail.StopGUISession
  ( -- * Creating a Request
    StopGUISession (..),
    newStopGUISession,

    -- * Request Lenses
    stopGUISession_resourceName,

    -- * Destructuring the Response
    StopGUISessionResponse (..),
    newStopGUISessionResponse,

    -- * Response Lenses
    stopGUISessionResponse_operations,
    stopGUISessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopGUISession' smart constructor.
data StopGUISession = StopGUISession'
  { -- | The resource name.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopGUISession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'stopGUISession_resourceName' - The resource name.
newStopGUISession ::
  -- | 'resourceName'
  Prelude.Text ->
  StopGUISession
newStopGUISession pResourceName_ =
  StopGUISession' {resourceName = pResourceName_}

-- | The resource name.
stopGUISession_resourceName :: Lens.Lens' StopGUISession Prelude.Text
stopGUISession_resourceName = Lens.lens (\StopGUISession' {resourceName} -> resourceName) (\s@StopGUISession' {} a -> s {resourceName = a} :: StopGUISession)

instance Core.AWSRequest StopGUISession where
  type
    AWSResponse StopGUISession =
      StopGUISessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopGUISessionResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopGUISession where
  hashWithSalt _salt StopGUISession' {..} =
    _salt `Prelude.hashWithSalt` resourceName

instance Prelude.NFData StopGUISession where
  rnf StopGUISession' {..} = Prelude.rnf resourceName

instance Data.ToHeaders StopGUISession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.StopGUISession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopGUISession where
  toJSON StopGUISession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath StopGUISession where
  toPath = Prelude.const "/"

instance Data.ToQuery StopGUISession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopGUISessionResponse' smart constructor.
data StopGUISessionResponse = StopGUISessionResponse'
  { -- | The available API operations.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopGUISessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'stopGUISessionResponse_operations' - The available API operations.
--
-- 'httpStatus', 'stopGUISessionResponse_httpStatus' - The response's http status code.
newStopGUISessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopGUISessionResponse
newStopGUISessionResponse pHttpStatus_ =
  StopGUISessionResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The available API operations.
stopGUISessionResponse_operations :: Lens.Lens' StopGUISessionResponse (Prelude.Maybe [Operation])
stopGUISessionResponse_operations = Lens.lens (\StopGUISessionResponse' {operations} -> operations) (\s@StopGUISessionResponse' {} a -> s {operations = a} :: StopGUISessionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
stopGUISessionResponse_httpStatus :: Lens.Lens' StopGUISessionResponse Prelude.Int
stopGUISessionResponse_httpStatus = Lens.lens (\StopGUISessionResponse' {httpStatus} -> httpStatus) (\s@StopGUISessionResponse' {} a -> s {httpStatus = a} :: StopGUISessionResponse)

instance Prelude.NFData StopGUISessionResponse where
  rnf StopGUISessionResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
