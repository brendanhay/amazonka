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
-- Module      : Amazonka.SageMaker.CreatePresignedNotebookInstanceUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a URL that you can use to connect to the Jupyter server from a
-- notebook instance. In the SageMaker console, when you choose @Open@ next
-- to a notebook instance, SageMaker opens a new tab showing the Jupyter
-- server home page from the notebook instance. The console uses this API
-- to get the URL and show the page.
--
-- The IAM role or user used to call this API defines the permissions to
-- access the notebook instance. Once the presigned URL is created, no
-- additional permission is required to access this URL. IAM authorization
-- policies for this API are also enforced for every HTTP request and
-- WebSocket frame that attempts to connect to the notebook instance.
--
-- You can restrict access to this API and to the URL that it returns to a
-- list of IP addresses that you specify. Use the @NotIpAddress@ condition
-- operator and the @aws:SourceIP@ condition context key to specify the
-- list of IP addresses that you want to have access to the notebook
-- instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/security_iam_id-based-policy-examples.html#nbi-ip-filter Limit Access to a Notebook Instance by IP Address>.
--
-- The URL that you get from a call to CreatePresignedNotebookInstanceUrl
-- is valid only for 5 minutes. If you try to use the URL after the
-- 5-minute limit expires, you are directed to the Amazon Web Services
-- console sign-in page.
module Amazonka.SageMaker.CreatePresignedNotebookInstanceUrl
  ( -- * Creating a Request
    CreatePresignedNotebookInstanceUrl (..),
    newCreatePresignedNotebookInstanceUrl,

    -- * Request Lenses
    createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds,
    createPresignedNotebookInstanceUrl_notebookInstanceName,

    -- * Destructuring the Response
    CreatePresignedNotebookInstanceUrlResponse (..),
    newCreatePresignedNotebookInstanceUrlResponse,

    -- * Response Lenses
    createPresignedNotebookInstanceUrlResponse_authorizedUrl,
    createPresignedNotebookInstanceUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreatePresignedNotebookInstanceUrl' smart constructor.
data CreatePresignedNotebookInstanceUrl = CreatePresignedNotebookInstanceUrl'
  { -- | The duration of the session, in seconds. The default is 12 hours.
    sessionExpirationDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the notebook instance.
    notebookInstanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePresignedNotebookInstanceUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionExpirationDurationInSeconds', 'createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds' - The duration of the session, in seconds. The default is 12 hours.
--
-- 'notebookInstanceName', 'createPresignedNotebookInstanceUrl_notebookInstanceName' - The name of the notebook instance.
newCreatePresignedNotebookInstanceUrl ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  CreatePresignedNotebookInstanceUrl
newCreatePresignedNotebookInstanceUrl
  pNotebookInstanceName_ =
    CreatePresignedNotebookInstanceUrl'
      { sessionExpirationDurationInSeconds =
          Prelude.Nothing,
        notebookInstanceName =
          pNotebookInstanceName_
      }

-- | The duration of the session, in seconds. The default is 12 hours.
createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedNotebookInstanceUrl (Prelude.Maybe Prelude.Natural)
createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds = Lens.lens (\CreatePresignedNotebookInstanceUrl' {sessionExpirationDurationInSeconds} -> sessionExpirationDurationInSeconds) (\s@CreatePresignedNotebookInstanceUrl' {} a -> s {sessionExpirationDurationInSeconds = a} :: CreatePresignedNotebookInstanceUrl)

-- | The name of the notebook instance.
createPresignedNotebookInstanceUrl_notebookInstanceName :: Lens.Lens' CreatePresignedNotebookInstanceUrl Prelude.Text
createPresignedNotebookInstanceUrl_notebookInstanceName = Lens.lens (\CreatePresignedNotebookInstanceUrl' {notebookInstanceName} -> notebookInstanceName) (\s@CreatePresignedNotebookInstanceUrl' {} a -> s {notebookInstanceName = a} :: CreatePresignedNotebookInstanceUrl)

instance
  Core.AWSRequest
    CreatePresignedNotebookInstanceUrl
  where
  type
    AWSResponse CreatePresignedNotebookInstanceUrl =
      CreatePresignedNotebookInstanceUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresignedNotebookInstanceUrlResponse'
            Prelude.<$> (x Data..?> "AuthorizedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreatePresignedNotebookInstanceUrl
  where
  hashWithSalt
    _salt
    CreatePresignedNotebookInstanceUrl' {..} =
      _salt
        `Prelude.hashWithSalt` sessionExpirationDurationInSeconds
        `Prelude.hashWithSalt` notebookInstanceName

instance
  Prelude.NFData
    CreatePresignedNotebookInstanceUrl
  where
  rnf CreatePresignedNotebookInstanceUrl' {..} =
    Prelude.rnf sessionExpirationDurationInSeconds `Prelude.seq`
      Prelude.rnf notebookInstanceName

instance
  Data.ToHeaders
    CreatePresignedNotebookInstanceUrl
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreatePresignedNotebookInstanceUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreatePresignedNotebookInstanceUrl
  where
  toJSON CreatePresignedNotebookInstanceUrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SessionExpirationDurationInSeconds" Data..=)
              Prelude.<$> sessionExpirationDurationInSeconds,
            Prelude.Just
              ( "NotebookInstanceName"
                  Data..= notebookInstanceName
              )
          ]
      )

instance
  Data.ToPath
    CreatePresignedNotebookInstanceUrl
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreatePresignedNotebookInstanceUrl
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePresignedNotebookInstanceUrlResponse' smart constructor.
data CreatePresignedNotebookInstanceUrlResponse = CreatePresignedNotebookInstanceUrlResponse'
  { -- | A JSON object that contains the URL string.
    authorizedUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePresignedNotebookInstanceUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedUrl', 'createPresignedNotebookInstanceUrlResponse_authorizedUrl' - A JSON object that contains the URL string.
--
-- 'httpStatus', 'createPresignedNotebookInstanceUrlResponse_httpStatus' - The response's http status code.
newCreatePresignedNotebookInstanceUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePresignedNotebookInstanceUrlResponse
newCreatePresignedNotebookInstanceUrlResponse
  pHttpStatus_ =
    CreatePresignedNotebookInstanceUrlResponse'
      { authorizedUrl =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A JSON object that contains the URL string.
createPresignedNotebookInstanceUrlResponse_authorizedUrl :: Lens.Lens' CreatePresignedNotebookInstanceUrlResponse (Prelude.Maybe Prelude.Text)
createPresignedNotebookInstanceUrlResponse_authorizedUrl = Lens.lens (\CreatePresignedNotebookInstanceUrlResponse' {authorizedUrl} -> authorizedUrl) (\s@CreatePresignedNotebookInstanceUrlResponse' {} a -> s {authorizedUrl = a} :: CreatePresignedNotebookInstanceUrlResponse)

-- | The response's http status code.
createPresignedNotebookInstanceUrlResponse_httpStatus :: Lens.Lens' CreatePresignedNotebookInstanceUrlResponse Prelude.Int
createPresignedNotebookInstanceUrlResponse_httpStatus = Lens.lens (\CreatePresignedNotebookInstanceUrlResponse' {httpStatus} -> httpStatus) (\s@CreatePresignedNotebookInstanceUrlResponse' {} a -> s {httpStatus = a} :: CreatePresignedNotebookInstanceUrlResponse)

instance
  Prelude.NFData
    CreatePresignedNotebookInstanceUrlResponse
  where
  rnf CreatePresignedNotebookInstanceUrlResponse' {..} =
    Prelude.rnf authorizedUrl `Prelude.seq`
      Prelude.rnf httpStatus
