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
-- Module      : Network.AWS.Chime.CreateSipMediaApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a SIP media application.
module Network.AWS.Chime.CreateSipMediaApplication
  ( -- * Creating a Request
    CreateSipMediaApplication (..),
    newCreateSipMediaApplication,

    -- * Request Lenses
    createSipMediaApplication_awsRegion,
    createSipMediaApplication_name,
    createSipMediaApplication_endpoints,

    -- * Destructuring the Response
    CreateSipMediaApplicationResponse (..),
    newCreateSipMediaApplicationResponse,

    -- * Response Lenses
    createSipMediaApplicationResponse_sipMediaApplication,
    createSipMediaApplicationResponse_httpStatus,
  )
where

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSipMediaApplication' smart constructor.
data CreateSipMediaApplication = CreateSipMediaApplication'
  { -- | The AWS Region assigned to the SIP media application.
    awsRegion :: Prelude.Text,
    -- | The SIP media application name.
    name :: Prelude.Text,
    -- | List of endpoints (Lambda Amazon Resource Names) specified for the SIP
    -- media application. Currently, only one endpoint is supported.
    endpoints :: Prelude.NonEmpty SipMediaApplicationEndpoint
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSipMediaApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegion', 'createSipMediaApplication_awsRegion' - The AWS Region assigned to the SIP media application.
--
-- 'name', 'createSipMediaApplication_name' - The SIP media application name.
--
-- 'endpoints', 'createSipMediaApplication_endpoints' - List of endpoints (Lambda Amazon Resource Names) specified for the SIP
-- media application. Currently, only one endpoint is supported.
newCreateSipMediaApplication ::
  -- | 'awsRegion'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'endpoints'
  Prelude.NonEmpty SipMediaApplicationEndpoint ->
  CreateSipMediaApplication
newCreateSipMediaApplication
  pAwsRegion_
  pName_
  pEndpoints_ =
    CreateSipMediaApplication'
      { awsRegion = pAwsRegion_,
        name = pName_,
        endpoints = Lens.coerced Lens.# pEndpoints_
      }

-- | The AWS Region assigned to the SIP media application.
createSipMediaApplication_awsRegion :: Lens.Lens' CreateSipMediaApplication Prelude.Text
createSipMediaApplication_awsRegion = Lens.lens (\CreateSipMediaApplication' {awsRegion} -> awsRegion) (\s@CreateSipMediaApplication' {} a -> s {awsRegion = a} :: CreateSipMediaApplication)

-- | The SIP media application name.
createSipMediaApplication_name :: Lens.Lens' CreateSipMediaApplication Prelude.Text
createSipMediaApplication_name = Lens.lens (\CreateSipMediaApplication' {name} -> name) (\s@CreateSipMediaApplication' {} a -> s {name = a} :: CreateSipMediaApplication)

-- | List of endpoints (Lambda Amazon Resource Names) specified for the SIP
-- media application. Currently, only one endpoint is supported.
createSipMediaApplication_endpoints :: Lens.Lens' CreateSipMediaApplication (Prelude.NonEmpty SipMediaApplicationEndpoint)
createSipMediaApplication_endpoints = Lens.lens (\CreateSipMediaApplication' {endpoints} -> endpoints) (\s@CreateSipMediaApplication' {} a -> s {endpoints = a} :: CreateSipMediaApplication) Prelude.. Lens.coerced

instance Core.AWSRequest CreateSipMediaApplication where
  type
    AWSResponse CreateSipMediaApplication =
      CreateSipMediaApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSipMediaApplicationResponse'
            Prelude.<$> (x Core..?> "SipMediaApplication")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSipMediaApplication

instance Prelude.NFData CreateSipMediaApplication

instance Core.ToHeaders CreateSipMediaApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateSipMediaApplication where
  toJSON CreateSipMediaApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AwsRegion" Core..= awsRegion),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Endpoints" Core..= endpoints)
          ]
      )

instance Core.ToPath CreateSipMediaApplication where
  toPath = Prelude.const "/sip-media-applications"

instance Core.ToQuery CreateSipMediaApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSipMediaApplicationResponse' smart constructor.
data CreateSipMediaApplicationResponse = CreateSipMediaApplicationResponse'
  { -- | The SIP media application details.
    sipMediaApplication :: Prelude.Maybe SipMediaApplication,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSipMediaApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplication', 'createSipMediaApplicationResponse_sipMediaApplication' - The SIP media application details.
--
-- 'httpStatus', 'createSipMediaApplicationResponse_httpStatus' - The response's http status code.
newCreateSipMediaApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSipMediaApplicationResponse
newCreateSipMediaApplicationResponse pHttpStatus_ =
  CreateSipMediaApplicationResponse'
    { sipMediaApplication =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The SIP media application details.
createSipMediaApplicationResponse_sipMediaApplication :: Lens.Lens' CreateSipMediaApplicationResponse (Prelude.Maybe SipMediaApplication)
createSipMediaApplicationResponse_sipMediaApplication = Lens.lens (\CreateSipMediaApplicationResponse' {sipMediaApplication} -> sipMediaApplication) (\s@CreateSipMediaApplicationResponse' {} a -> s {sipMediaApplication = a} :: CreateSipMediaApplicationResponse)

-- | The response's http status code.
createSipMediaApplicationResponse_httpStatus :: Lens.Lens' CreateSipMediaApplicationResponse Prelude.Int
createSipMediaApplicationResponse_httpStatus = Lens.lens (\CreateSipMediaApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateSipMediaApplicationResponse' {} a -> s {httpStatus = a} :: CreateSipMediaApplicationResponse)

instance
  Prelude.NFData
    CreateSipMediaApplicationResponse
