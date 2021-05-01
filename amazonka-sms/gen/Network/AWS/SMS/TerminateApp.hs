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
-- Module      : Network.AWS.SMS.TerminateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the stack for the specified application.
module Network.AWS.SMS.TerminateApp
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newTerminateApp' smart constructor.
data TerminateApp = TerminateApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest TerminateApp where
  type Rs TerminateApp = TerminateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateApp

instance Prelude.NFData TerminateApp

instance Prelude.ToHeaders TerminateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.TerminateApp" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TerminateApp where
  toJSON TerminateApp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("appId" Prelude..=) Prelude.<$> appId]
      )

instance Prelude.ToPath TerminateApp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TerminateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateAppResponse' smart constructor.
data TerminateAppResponse = TerminateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData TerminateAppResponse
