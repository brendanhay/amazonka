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
-- Module      : Amazonka.SMS.TerminateApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the stack for the specified application.
module Amazonka.SMS.TerminateApp
  ( -- * Creating a Request
    TerminateApp (..),
    newTerminateApp,

    -- * Request Lenses
    terminateApp_appId,

    -- * Destructuring the Response
    TerminateAppResponse (..),
    newTerminateAppResponse,

    -- * Response Lenses
    terminateAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newTerminateApp' smart constructor.
data TerminateApp = TerminateApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'terminateApp_appId' - The ID of the application.
newTerminateApp ::
  TerminateApp
newTerminateApp =
  TerminateApp' {appId = Prelude.Nothing}

-- | The ID of the application.
terminateApp_appId :: Lens.Lens' TerminateApp (Prelude.Maybe Prelude.Text)
terminateApp_appId = Lens.lens (\TerminateApp' {appId} -> appId) (\s@TerminateApp' {} a -> s {appId = a} :: TerminateApp)

instance Core.AWSRequest TerminateApp where
  type AWSResponse TerminateApp = TerminateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateApp where
  hashWithSalt _salt TerminateApp' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData TerminateApp where
  rnf TerminateApp' {..} = Prelude.rnf appId

instance Data.ToHeaders TerminateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.TerminateApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateApp where
  toJSON TerminateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [("appId" Data..=) Prelude.<$> appId]
      )

instance Data.ToPath TerminateApp where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateAppResponse' smart constructor.
data TerminateAppResponse = TerminateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'terminateAppResponse_httpStatus' - The response's http status code.
newTerminateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateAppResponse
newTerminateAppResponse pHttpStatus_ =
  TerminateAppResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
terminateAppResponse_httpStatus :: Lens.Lens' TerminateAppResponse Prelude.Int
terminateAppResponse_httpStatus = Lens.lens (\TerminateAppResponse' {httpStatus} -> httpStatus) (\s@TerminateAppResponse' {} a -> s {httpStatus = a} :: TerminateAppResponse)

instance Prelude.NFData TerminateAppResponse where
  rnf TerminateAppResponse' {..} =
    Prelude.rnf httpStatus
