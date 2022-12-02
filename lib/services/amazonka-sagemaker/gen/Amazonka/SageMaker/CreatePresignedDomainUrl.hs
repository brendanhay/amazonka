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
-- Module      : Amazonka.SageMaker.CreatePresignedDomainUrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL for a specified UserProfile in a Domain. When accessed in
-- a web browser, the user will be automatically signed in to Amazon
-- SageMaker Studio, and granted access to all of the Apps and files
-- associated with the Domain\'s Amazon Elastic File System (EFS) volume.
-- This operation can only be called when the authentication mode equals
-- IAM.
--
-- The IAM role or user passed to this API defines the permissions to
-- access the app. Once the presigned URL is created, no additional
-- permission is required to access this URL. IAM authorization policies
-- for this API are also enforced for every HTTP request and WebSocket
-- frame that attempts to connect to the app.
--
-- You can restrict access to this API and to the URL that it returns to a
-- list of IP addresses, Amazon VPCs or Amazon VPC Endpoints that you
-- specify. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-interface-endpoint.html Connect to SageMaker Studio Through an Interface VPC Endpoint>
-- .
--
-- The URL that you get from a call to @CreatePresignedDomainUrl@ has a
-- default timeout of 5 minutes. You can configure this value using
-- @ExpiresInSeconds@. If you try to use the URL after the timeout limit
-- expires, you are directed to the Amazon Web Services console sign-in
-- page.
module Amazonka.SageMaker.CreatePresignedDomainUrl
  ( -- * Creating a Request
    CreatePresignedDomainUrl (..),
    newCreatePresignedDomainUrl,

    -- * Request Lenses
    createPresignedDomainUrl_expiresInSeconds,
    createPresignedDomainUrl_sessionExpirationDurationInSeconds,
    createPresignedDomainUrl_domainId,
    createPresignedDomainUrl_userProfileName,

    -- * Destructuring the Response
    CreatePresignedDomainUrlResponse (..),
    newCreatePresignedDomainUrlResponse,

    -- * Response Lenses
    createPresignedDomainUrlResponse_authorizedUrl,
    createPresignedDomainUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreatePresignedDomainUrl' smart constructor.
data CreatePresignedDomainUrl = CreatePresignedDomainUrl'
  { -- | The number of seconds until the pre-signed URL expires. This value
    -- defaults to 300.
    expiresInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The session expiration duration in seconds. This value defaults to
    -- 43200.
    sessionExpirationDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The name of the UserProfile to sign-in as.
    userProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePresignedDomainUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresInSeconds', 'createPresignedDomainUrl_expiresInSeconds' - The number of seconds until the pre-signed URL expires. This value
-- defaults to 300.
--
-- 'sessionExpirationDurationInSeconds', 'createPresignedDomainUrl_sessionExpirationDurationInSeconds' - The session expiration duration in seconds. This value defaults to
-- 43200.
--
-- 'domainId', 'createPresignedDomainUrl_domainId' - The domain ID.
--
-- 'userProfileName', 'createPresignedDomainUrl_userProfileName' - The name of the UserProfile to sign-in as.
newCreatePresignedDomainUrl ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  CreatePresignedDomainUrl
newCreatePresignedDomainUrl
  pDomainId_
  pUserProfileName_ =
    CreatePresignedDomainUrl'
      { expiresInSeconds =
          Prelude.Nothing,
        sessionExpirationDurationInSeconds =
          Prelude.Nothing,
        domainId = pDomainId_,
        userProfileName = pUserProfileName_
      }

-- | The number of seconds until the pre-signed URL expires. This value
-- defaults to 300.
createPresignedDomainUrl_expiresInSeconds :: Lens.Lens' CreatePresignedDomainUrl (Prelude.Maybe Prelude.Natural)
createPresignedDomainUrl_expiresInSeconds = Lens.lens (\CreatePresignedDomainUrl' {expiresInSeconds} -> expiresInSeconds) (\s@CreatePresignedDomainUrl' {} a -> s {expiresInSeconds = a} :: CreatePresignedDomainUrl)

-- | The session expiration duration in seconds. This value defaults to
-- 43200.
createPresignedDomainUrl_sessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedDomainUrl (Prelude.Maybe Prelude.Natural)
createPresignedDomainUrl_sessionExpirationDurationInSeconds = Lens.lens (\CreatePresignedDomainUrl' {sessionExpirationDurationInSeconds} -> sessionExpirationDurationInSeconds) (\s@CreatePresignedDomainUrl' {} a -> s {sessionExpirationDurationInSeconds = a} :: CreatePresignedDomainUrl)

-- | The domain ID.
createPresignedDomainUrl_domainId :: Lens.Lens' CreatePresignedDomainUrl Prelude.Text
createPresignedDomainUrl_domainId = Lens.lens (\CreatePresignedDomainUrl' {domainId} -> domainId) (\s@CreatePresignedDomainUrl' {} a -> s {domainId = a} :: CreatePresignedDomainUrl)

-- | The name of the UserProfile to sign-in as.
createPresignedDomainUrl_userProfileName :: Lens.Lens' CreatePresignedDomainUrl Prelude.Text
createPresignedDomainUrl_userProfileName = Lens.lens (\CreatePresignedDomainUrl' {userProfileName} -> userProfileName) (\s@CreatePresignedDomainUrl' {} a -> s {userProfileName = a} :: CreatePresignedDomainUrl)

instance Core.AWSRequest CreatePresignedDomainUrl where
  type
    AWSResponse CreatePresignedDomainUrl =
      CreatePresignedDomainUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresignedDomainUrlResponse'
            Prelude.<$> (x Data..?> "AuthorizedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePresignedDomainUrl where
  hashWithSalt _salt CreatePresignedDomainUrl' {..} =
    _salt `Prelude.hashWithSalt` expiresInSeconds
      `Prelude.hashWithSalt` sessionExpirationDurationInSeconds
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData CreatePresignedDomainUrl where
  rnf CreatePresignedDomainUrl' {..} =
    Prelude.rnf expiresInSeconds
      `Prelude.seq` Prelude.rnf sessionExpirationDurationInSeconds
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf userProfileName

instance Data.ToHeaders CreatePresignedDomainUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreatePresignedDomainUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePresignedDomainUrl where
  toJSON CreatePresignedDomainUrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpiresInSeconds" Data..=)
              Prelude.<$> expiresInSeconds,
            ("SessionExpirationDurationInSeconds" Data..=)
              Prelude.<$> sessionExpirationDurationInSeconds,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just
              ("UserProfileName" Data..= userProfileName)
          ]
      )

instance Data.ToPath CreatePresignedDomainUrl where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePresignedDomainUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePresignedDomainUrlResponse' smart constructor.
data CreatePresignedDomainUrlResponse = CreatePresignedDomainUrlResponse'
  { -- | The presigned URL.
    authorizedUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePresignedDomainUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedUrl', 'createPresignedDomainUrlResponse_authorizedUrl' - The presigned URL.
--
-- 'httpStatus', 'createPresignedDomainUrlResponse_httpStatus' - The response's http status code.
newCreatePresignedDomainUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePresignedDomainUrlResponse
newCreatePresignedDomainUrlResponse pHttpStatus_ =
  CreatePresignedDomainUrlResponse'
    { authorizedUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The presigned URL.
createPresignedDomainUrlResponse_authorizedUrl :: Lens.Lens' CreatePresignedDomainUrlResponse (Prelude.Maybe Prelude.Text)
createPresignedDomainUrlResponse_authorizedUrl = Lens.lens (\CreatePresignedDomainUrlResponse' {authorizedUrl} -> authorizedUrl) (\s@CreatePresignedDomainUrlResponse' {} a -> s {authorizedUrl = a} :: CreatePresignedDomainUrlResponse)

-- | The response's http status code.
createPresignedDomainUrlResponse_httpStatus :: Lens.Lens' CreatePresignedDomainUrlResponse Prelude.Int
createPresignedDomainUrlResponse_httpStatus = Lens.lens (\CreatePresignedDomainUrlResponse' {httpStatus} -> httpStatus) (\s@CreatePresignedDomainUrlResponse' {} a -> s {httpStatus = a} :: CreatePresignedDomainUrlResponse)

instance
  Prelude.NFData
    CreatePresignedDomainUrlResponse
  where
  rnf CreatePresignedDomainUrlResponse' {..} =
    Prelude.rnf authorizedUrl
      `Prelude.seq` Prelude.rnf httpStatus
