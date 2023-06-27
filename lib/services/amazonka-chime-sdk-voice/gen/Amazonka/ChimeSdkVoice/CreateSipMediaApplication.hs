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
-- Module      : Amazonka.ChimeSdkVoice.CreateSipMediaApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a SIP media application. For more information about SIP media
-- applications, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/ag/manage-sip-applications.html Managing SIP media applications and rules>
-- in the /Amazon Chime SDK Administrator Guide/.
module Amazonka.ChimeSdkVoice.CreateSipMediaApplication
  ( -- * Creating a Request
    CreateSipMediaApplication (..),
    newCreateSipMediaApplication,

    -- * Request Lenses
    createSipMediaApplication_tags,
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

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSipMediaApplication' smart constructor.
data CreateSipMediaApplication = CreateSipMediaApplication'
  { -- | The tags assigned to the SIP media application.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The AWS Region assigned to the SIP media application.
    awsRegion :: Prelude.Text,
    -- | The SIP media application\'s name.
    name :: Prelude.Text,
    -- | List of endpoints (Lambda ARNs) specified for the SIP media application.
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
-- 'tags', 'createSipMediaApplication_tags' - The tags assigned to the SIP media application.
--
-- 'awsRegion', 'createSipMediaApplication_awsRegion' - The AWS Region assigned to the SIP media application.
--
-- 'name', 'createSipMediaApplication_name' - The SIP media application\'s name.
--
-- 'endpoints', 'createSipMediaApplication_endpoints' - List of endpoints (Lambda ARNs) specified for the SIP media application.
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
      { tags = Prelude.Nothing,
        awsRegion = pAwsRegion_,
        name = pName_,
        endpoints = Lens.coerced Lens.# pEndpoints_
      }

-- | The tags assigned to the SIP media application.
createSipMediaApplication_tags :: Lens.Lens' CreateSipMediaApplication (Prelude.Maybe (Prelude.NonEmpty Tag))
createSipMediaApplication_tags = Lens.lens (\CreateSipMediaApplication' {tags} -> tags) (\s@CreateSipMediaApplication' {} a -> s {tags = a} :: CreateSipMediaApplication) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Region assigned to the SIP media application.
createSipMediaApplication_awsRegion :: Lens.Lens' CreateSipMediaApplication Prelude.Text
createSipMediaApplication_awsRegion = Lens.lens (\CreateSipMediaApplication' {awsRegion} -> awsRegion) (\s@CreateSipMediaApplication' {} a -> s {awsRegion = a} :: CreateSipMediaApplication)

-- | The SIP media application\'s name.
createSipMediaApplication_name :: Lens.Lens' CreateSipMediaApplication Prelude.Text
createSipMediaApplication_name = Lens.lens (\CreateSipMediaApplication' {name} -> name) (\s@CreateSipMediaApplication' {} a -> s {name = a} :: CreateSipMediaApplication)

-- | List of endpoints (Lambda ARNs) specified for the SIP media application.
createSipMediaApplication_endpoints :: Lens.Lens' CreateSipMediaApplication (Prelude.NonEmpty SipMediaApplicationEndpoint)
createSipMediaApplication_endpoints = Lens.lens (\CreateSipMediaApplication' {endpoints} -> endpoints) (\s@CreateSipMediaApplication' {} a -> s {endpoints = a} :: CreateSipMediaApplication) Prelude.. Lens.coerced

instance Core.AWSRequest CreateSipMediaApplication where
  type
    AWSResponse CreateSipMediaApplication =
      CreateSipMediaApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSipMediaApplicationResponse'
            Prelude.<$> (x Data..?> "SipMediaApplication")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSipMediaApplication where
  hashWithSalt _salt CreateSipMediaApplication' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` endpoints

instance Prelude.NFData CreateSipMediaApplication where
  rnf CreateSipMediaApplication' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf endpoints

instance Data.ToHeaders CreateSipMediaApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateSipMediaApplication where
  toJSON CreateSipMediaApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("AwsRegion" Data..= awsRegion),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Endpoints" Data..= endpoints)
          ]
      )

instance Data.ToPath CreateSipMediaApplication where
  toPath = Prelude.const "/sip-media-applications"

instance Data.ToQuery CreateSipMediaApplication where
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
  where
  rnf CreateSipMediaApplicationResponse' {..} =
    Prelude.rnf sipMediaApplication
      `Prelude.seq` Prelude.rnf httpStatus
