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
-- Module      : Amazonka.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a platform application object for one of the supported push
-- notification services, such as APNS and GCM (Firebase Cloud Messaging),
-- to which devices and mobile apps may register. You must specify
-- @PlatformPrincipal@ and @PlatformCredential@ attributes when using the
-- @CreatePlatformApplication@ action.
--
-- @PlatformPrincipal@ and @PlatformCredential@ are received from the
-- notification service.
--
-- -   For @ADM@, @PlatformPrincipal@ is @client id@ and
--     @PlatformCredential@ is @client secret@.
--
-- -   For @Baidu@, @PlatformPrincipal@ is @API key@ and
--     @PlatformCredential@ is @secret key@.
--
-- -   For @APNS@ and @APNS_SANDBOX@ using certificate credentials,
--     @PlatformPrincipal@ is @SSL certificate@ and @PlatformCredential@ is
--     @private key@.
--
-- -   For @APNS@ and @APNS_SANDBOX@ using token credentials,
--     @PlatformPrincipal@ is @signing key ID@ and @PlatformCredential@ is
--     @signing key@.
--
-- -   For @GCM@ (Firebase Cloud Messaging), there is no
--     @PlatformPrincipal@ and the @PlatformCredential@ is @API key@.
--
-- -   For @MPNS@, @PlatformPrincipal@ is @TLS certificate@ and
--     @PlatformCredential@ is @private key@.
--
-- -   For @WNS@, @PlatformPrincipal@ is @Package Security Identifier@ and
--     @PlatformCredential@ is @secret key@.
--
-- You can use the returned @PlatformApplicationArn@ as an attribute for
-- the @CreatePlatformEndpoint@ action.
module Amazonka.SNS.CreatePlatformApplication
  ( -- * Creating a Request
    CreatePlatformApplication (..),
    newCreatePlatformApplication,

    -- * Request Lenses
    createPlatformApplication_name,
    createPlatformApplication_platform,
    createPlatformApplication_attributes,

    -- * Destructuring the Response
    CreatePlatformApplicationResponse (..),
    newCreatePlatformApplicationResponse,

    -- * Response Lenses
    createPlatformApplicationResponse_platformApplicationArn,
    createPlatformApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for CreatePlatformApplication action.
--
-- /See:/ 'newCreatePlatformApplication' smart constructor.
data CreatePlatformApplication = CreatePlatformApplication'
  { -- | Application names must be made up of only uppercase and lowercase ASCII
    -- letters, numbers, underscores, hyphens, and periods, and must be between
    -- 1 and 256 characters long.
    name :: Prelude.Text,
    -- | The following platforms are supported: ADM (Amazon Device Messaging),
    -- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase
    -- Cloud Messaging).
    platform :: Prelude.Text,
    -- | For a list of attributes, see
    -- <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlatformApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createPlatformApplication_name' - Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between
-- 1 and 256 characters long.
--
-- 'platform', 'createPlatformApplication_platform' - The following platforms are supported: ADM (Amazon Device Messaging),
-- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase
-- Cloud Messaging).
--
-- 'attributes', 'createPlatformApplication_attributes' - For a list of attributes, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>.
newCreatePlatformApplication ::
  -- | 'name'
  Prelude.Text ->
  -- | 'platform'
  Prelude.Text ->
  CreatePlatformApplication
newCreatePlatformApplication pName_ pPlatform_ =
  CreatePlatformApplication'
    { name = pName_,
      platform = pPlatform_,
      attributes = Prelude.mempty
    }

-- | Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between
-- 1 and 256 characters long.
createPlatformApplication_name :: Lens.Lens' CreatePlatformApplication Prelude.Text
createPlatformApplication_name = Lens.lens (\CreatePlatformApplication' {name} -> name) (\s@CreatePlatformApplication' {} a -> s {name = a} :: CreatePlatformApplication)

-- | The following platforms are supported: ADM (Amazon Device Messaging),
-- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase
-- Cloud Messaging).
createPlatformApplication_platform :: Lens.Lens' CreatePlatformApplication Prelude.Text
createPlatformApplication_platform = Lens.lens (\CreatePlatformApplication' {platform} -> platform) (\s@CreatePlatformApplication' {} a -> s {platform = a} :: CreatePlatformApplication)

-- | For a list of attributes, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>.
createPlatformApplication_attributes :: Lens.Lens' CreatePlatformApplication (Prelude.HashMap Prelude.Text Prelude.Text)
createPlatformApplication_attributes = Lens.lens (\CreatePlatformApplication' {attributes} -> attributes) (\s@CreatePlatformApplication' {} a -> s {attributes = a} :: CreatePlatformApplication) Prelude.. Lens.coerced

instance Core.AWSRequest CreatePlatformApplication where
  type
    AWSResponse CreatePlatformApplication =
      CreatePlatformApplicationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreatePlatformApplicationResult"
      ( \s h x ->
          CreatePlatformApplicationResponse'
            Prelude.<$> (x Data..@? "PlatformApplicationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlatformApplication where
  hashWithSalt _salt CreatePlatformApplication' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData CreatePlatformApplication where
  rnf CreatePlatformApplication' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToHeaders CreatePlatformApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePlatformApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePlatformApplication where
  toQuery CreatePlatformApplication' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreatePlatformApplication" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "Name" Data.=: name,
        "Platform" Data.=: platform,
        "Attributes"
          Data.=: Data.toQueryMap "entry" "key" "value" attributes
      ]

-- | Response from CreatePlatformApplication action.
--
-- /See:/ 'newCreatePlatformApplicationResponse' smart constructor.
data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse'
  { -- | PlatformApplicationArn is returned.
    platformApplicationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlatformApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformApplicationArn', 'createPlatformApplicationResponse_platformApplicationArn' - PlatformApplicationArn is returned.
--
-- 'httpStatus', 'createPlatformApplicationResponse_httpStatus' - The response's http status code.
newCreatePlatformApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePlatformApplicationResponse
newCreatePlatformApplicationResponse pHttpStatus_ =
  CreatePlatformApplicationResponse'
    { platformApplicationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | PlatformApplicationArn is returned.
createPlatformApplicationResponse_platformApplicationArn :: Lens.Lens' CreatePlatformApplicationResponse (Prelude.Maybe Prelude.Text)
createPlatformApplicationResponse_platformApplicationArn = Lens.lens (\CreatePlatformApplicationResponse' {platformApplicationArn} -> platformApplicationArn) (\s@CreatePlatformApplicationResponse' {} a -> s {platformApplicationArn = a} :: CreatePlatformApplicationResponse)

-- | The response's http status code.
createPlatformApplicationResponse_httpStatus :: Lens.Lens' CreatePlatformApplicationResponse Prelude.Int
createPlatformApplicationResponse_httpStatus = Lens.lens (\CreatePlatformApplicationResponse' {httpStatus} -> httpStatus) (\s@CreatePlatformApplicationResponse' {} a -> s {httpStatus = a} :: CreatePlatformApplicationResponse)

instance
  Prelude.NFData
    CreatePlatformApplicationResponse
  where
  rnf CreatePlatformApplicationResponse' {..} =
    Prelude.rnf platformApplicationArn
      `Prelude.seq` Prelude.rnf httpStatus
