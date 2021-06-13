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
-- Module      : Network.AWS.SageMaker.CreatePresignedDomainUrl
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- The URL that you get from a call to @CreatePresignedDomainUrl@ has a
-- default timeout of 5 minutes. You can configure this value using
-- @ExpiresInSeconds@. If you try to use the URL after the timeout limit
-- expires, you are directed to the AWS console sign-in page.
module Network.AWS.SageMaker.CreatePresignedDomainUrl
  ( -- * Creating a Request
    CreatePresignedDomainUrl (..),
    newCreatePresignedDomainUrl,

    -- * Request Lenses
    createPresignedDomainUrl_sessionExpirationDurationInSeconds,
    createPresignedDomainUrl_expiresInSeconds,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreatePresignedDomainUrl' smart constructor.
data CreatePresignedDomainUrl = CreatePresignedDomainUrl'
  { -- | The session expiration duration in seconds. This value defaults to
    -- 43200.
    sessionExpirationDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The number of seconds until the pre-signed URL expires. This value
    -- defaults to 300.
    expiresInSeconds :: Prelude.Maybe Prelude.Natural,
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
-- 'sessionExpirationDurationInSeconds', 'createPresignedDomainUrl_sessionExpirationDurationInSeconds' - The session expiration duration in seconds. This value defaults to
-- 43200.
--
-- 'expiresInSeconds', 'createPresignedDomainUrl_expiresInSeconds' - The number of seconds until the pre-signed URL expires. This value
-- defaults to 300.
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
      { sessionExpirationDurationInSeconds =
          Prelude.Nothing,
        expiresInSeconds = Prelude.Nothing,
        domainId = pDomainId_,
        userProfileName = pUserProfileName_
      }

-- | The session expiration duration in seconds. This value defaults to
-- 43200.
createPresignedDomainUrl_sessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedDomainUrl (Prelude.Maybe Prelude.Natural)
createPresignedDomainUrl_sessionExpirationDurationInSeconds = Lens.lens (\CreatePresignedDomainUrl' {sessionExpirationDurationInSeconds} -> sessionExpirationDurationInSeconds) (\s@CreatePresignedDomainUrl' {} a -> s {sessionExpirationDurationInSeconds = a} :: CreatePresignedDomainUrl)

-- | The number of seconds until the pre-signed URL expires. This value
-- defaults to 300.
createPresignedDomainUrl_expiresInSeconds :: Lens.Lens' CreatePresignedDomainUrl (Prelude.Maybe Prelude.Natural)
createPresignedDomainUrl_expiresInSeconds = Lens.lens (\CreatePresignedDomainUrl' {expiresInSeconds} -> expiresInSeconds) (\s@CreatePresignedDomainUrl' {} a -> s {expiresInSeconds = a} :: CreatePresignedDomainUrl)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresignedDomainUrlResponse'
            Prelude.<$> (x Core..?> "AuthorizedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePresignedDomainUrl

instance Prelude.NFData CreatePresignedDomainUrl

instance Core.ToHeaders CreatePresignedDomainUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreatePresignedDomainUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePresignedDomainUrl where
  toJSON CreatePresignedDomainUrl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SessionExpirationDurationInSeconds" Core..=)
              Prelude.<$> sessionExpirationDurationInSeconds,
            ("ExpiresInSeconds" Core..=)
              Prelude.<$> expiresInSeconds,
            Prelude.Just ("DomainId" Core..= domainId),
            Prelude.Just
              ("UserProfileName" Core..= userProfileName)
          ]
      )

instance Core.ToPath CreatePresignedDomainUrl where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePresignedDomainUrl where
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
