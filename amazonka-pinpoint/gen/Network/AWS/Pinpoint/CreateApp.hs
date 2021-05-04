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
-- Module      : Network.AWS.Pinpoint.CreateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Network.AWS.Pinpoint.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_createApplicationRequest,

    -- * Destructuring the Response
    CreateAppResponse (..),
    newCreateAppResponse,

    -- * Response Lenses
    createAppResponse_httpStatus,
    createAppResponse_applicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { createApplicationRequest :: CreateApplicationRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createApplicationRequest', 'createApp_createApplicationRequest' - Undocumented member.
newCreateApp ::
  -- | 'createApplicationRequest'
  CreateApplicationRequest ->
  CreateApp
newCreateApp pCreateApplicationRequest_ =
  CreateApp'
    { createApplicationRequest =
        pCreateApplicationRequest_
    }

-- | Undocumented member.
createApp_createApplicationRequest :: Lens.Lens' CreateApp CreateApplicationRequest
createApp_createApplicationRequest = Lens.lens (\CreateApp' {createApplicationRequest} -> createApplicationRequest) (\s@CreateApp' {} a -> s {createApplicationRequest = a} :: CreateApp)

instance Prelude.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable CreateApp

instance Prelude.NFData CreateApp

instance Prelude.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CreateApplicationRequest"
                  Prelude..= createApplicationRequest
              )
          ]
      )

instance Prelude.ToPath CreateApp where
  toPath = Prelude.const "/v1/apps"

instance Prelude.ToQuery CreateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    applicationResponse :: ApplicationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAppResponse_httpStatus' - The response's http status code.
--
-- 'applicationResponse', 'createAppResponse_applicationResponse' - Undocumented member.
newCreateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationResponse'
  ApplicationResponse ->
  CreateAppResponse
newCreateAppResponse
  pHttpStatus_
  pApplicationResponse_ =
    CreateAppResponse'
      { httpStatus = pHttpStatus_,
        applicationResponse = pApplicationResponse_
      }

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Prelude.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

-- | Undocumented member.
createAppResponse_applicationResponse :: Lens.Lens' CreateAppResponse ApplicationResponse
createAppResponse_applicationResponse = Lens.lens (\CreateAppResponse' {applicationResponse} -> applicationResponse) (\s@CreateAppResponse' {} a -> s {applicationResponse = a} :: CreateAppResponse)

instance Prelude.NFData CreateAppResponse
