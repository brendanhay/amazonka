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
-- Module      : Amazonka.KinesisAnalyticsV2.CreateApplicationPresignedUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and returns a URL that you can use to connect to an
-- application\'s extension.
--
-- The IAM role or user used to call this API defines the permissions to
-- access the extension. After the presigned URL is created, no additional
-- permission is required to access this URL. IAM authorization policies
-- for this API are also enforced for every HTTP request that attempts to
-- connect to the extension.
--
-- You control the amount of time that the URL will be valid using the
-- @SessionExpirationDurationInSeconds@ parameter. If you do not provide
-- this parameter, the returned URL is valid for twelve hours.
--
-- The URL that you get from a call to CreateApplicationPresignedUrl must
-- be used within 3 minutes to be valid. If you first try to use the URL
-- after the 3-minute limit expires, the service returns an HTTP 403
-- Forbidden error.
module Amazonka.KinesisAnalyticsV2.CreateApplicationPresignedUrl
  ( -- * Creating a Request
    CreateApplicationPresignedUrl (..),
    newCreateApplicationPresignedUrl,

    -- * Request Lenses
    createApplicationPresignedUrl_sessionExpirationDurationInSeconds,
    createApplicationPresignedUrl_applicationName,
    createApplicationPresignedUrl_urlType,

    -- * Destructuring the Response
    CreateApplicationPresignedUrlResponse (..),
    newCreateApplicationPresignedUrlResponse,

    -- * Response Lenses
    createApplicationPresignedUrlResponse_authorizedUrl,
    createApplicationPresignedUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplicationPresignedUrl' smart constructor.
data CreateApplicationPresignedUrl = CreateApplicationPresignedUrl'
  { -- | The duration in seconds for which the returned URL will be valid.
    sessionExpirationDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the application.
    applicationName :: Prelude.Text,
    -- | The type of the extension for which to create and return a URL.
    -- Currently, the only valid extension URL type is @FLINK_DASHBOARD_URL@.
    urlType :: UrlType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationPresignedUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionExpirationDurationInSeconds', 'createApplicationPresignedUrl_sessionExpirationDurationInSeconds' - The duration in seconds for which the returned URL will be valid.
--
-- 'applicationName', 'createApplicationPresignedUrl_applicationName' - The name of the application.
--
-- 'urlType', 'createApplicationPresignedUrl_urlType' - The type of the extension for which to create and return a URL.
-- Currently, the only valid extension URL type is @FLINK_DASHBOARD_URL@.
newCreateApplicationPresignedUrl ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'urlType'
  UrlType ->
  CreateApplicationPresignedUrl
newCreateApplicationPresignedUrl
  pApplicationName_
  pUrlType_ =
    CreateApplicationPresignedUrl'
      { sessionExpirationDurationInSeconds =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        urlType = pUrlType_
      }

-- | The duration in seconds for which the returned URL will be valid.
createApplicationPresignedUrl_sessionExpirationDurationInSeconds :: Lens.Lens' CreateApplicationPresignedUrl (Prelude.Maybe Prelude.Natural)
createApplicationPresignedUrl_sessionExpirationDurationInSeconds = Lens.lens (\CreateApplicationPresignedUrl' {sessionExpirationDurationInSeconds} -> sessionExpirationDurationInSeconds) (\s@CreateApplicationPresignedUrl' {} a -> s {sessionExpirationDurationInSeconds = a} :: CreateApplicationPresignedUrl)

-- | The name of the application.
createApplicationPresignedUrl_applicationName :: Lens.Lens' CreateApplicationPresignedUrl Prelude.Text
createApplicationPresignedUrl_applicationName = Lens.lens (\CreateApplicationPresignedUrl' {applicationName} -> applicationName) (\s@CreateApplicationPresignedUrl' {} a -> s {applicationName = a} :: CreateApplicationPresignedUrl)

-- | The type of the extension for which to create and return a URL.
-- Currently, the only valid extension URL type is @FLINK_DASHBOARD_URL@.
createApplicationPresignedUrl_urlType :: Lens.Lens' CreateApplicationPresignedUrl UrlType
createApplicationPresignedUrl_urlType = Lens.lens (\CreateApplicationPresignedUrl' {urlType} -> urlType) (\s@CreateApplicationPresignedUrl' {} a -> s {urlType = a} :: CreateApplicationPresignedUrl)

instance
  Core.AWSRequest
    CreateApplicationPresignedUrl
  where
  type
    AWSResponse CreateApplicationPresignedUrl =
      CreateApplicationPresignedUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationPresignedUrlResponse'
            Prelude.<$> (x Data..?> "AuthorizedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateApplicationPresignedUrl
  where
  hashWithSalt _salt CreateApplicationPresignedUrl' {..} =
    _salt
      `Prelude.hashWithSalt` sessionExpirationDurationInSeconds
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` urlType

instance Prelude.NFData CreateApplicationPresignedUrl where
  rnf CreateApplicationPresignedUrl' {..} =
    Prelude.rnf sessionExpirationDurationInSeconds
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf urlType

instance Data.ToHeaders CreateApplicationPresignedUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.CreateApplicationPresignedUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplicationPresignedUrl where
  toJSON CreateApplicationPresignedUrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SessionExpirationDurationInSeconds" Data..=)
              Prelude.<$> sessionExpirationDurationInSeconds,
            Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just ("UrlType" Data..= urlType)
          ]
      )

instance Data.ToPath CreateApplicationPresignedUrl where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateApplicationPresignedUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationPresignedUrlResponse' smart constructor.
data CreateApplicationPresignedUrlResponse = CreateApplicationPresignedUrlResponse'
  { -- | The URL of the extension.
    authorizedUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationPresignedUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedUrl', 'createApplicationPresignedUrlResponse_authorizedUrl' - The URL of the extension.
--
-- 'httpStatus', 'createApplicationPresignedUrlResponse_httpStatus' - The response's http status code.
newCreateApplicationPresignedUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationPresignedUrlResponse
newCreateApplicationPresignedUrlResponse pHttpStatus_ =
  CreateApplicationPresignedUrlResponse'
    { authorizedUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL of the extension.
createApplicationPresignedUrlResponse_authorizedUrl :: Lens.Lens' CreateApplicationPresignedUrlResponse (Prelude.Maybe Prelude.Text)
createApplicationPresignedUrlResponse_authorizedUrl = Lens.lens (\CreateApplicationPresignedUrlResponse' {authorizedUrl} -> authorizedUrl) (\s@CreateApplicationPresignedUrlResponse' {} a -> s {authorizedUrl = a} :: CreateApplicationPresignedUrlResponse)

-- | The response's http status code.
createApplicationPresignedUrlResponse_httpStatus :: Lens.Lens' CreateApplicationPresignedUrlResponse Prelude.Int
createApplicationPresignedUrlResponse_httpStatus = Lens.lens (\CreateApplicationPresignedUrlResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationPresignedUrlResponse' {} a -> s {httpStatus = a} :: CreateApplicationPresignedUrlResponse)

instance
  Prelude.NFData
    CreateApplicationPresignedUrlResponse
  where
  rnf CreateApplicationPresignedUrlResponse' {..} =
    Prelude.rnf authorizedUrl
      `Prelude.seq` Prelude.rnf httpStatus
