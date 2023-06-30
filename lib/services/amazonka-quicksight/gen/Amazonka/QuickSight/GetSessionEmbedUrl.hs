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
-- Module      : Amazonka.QuickSight.GetSessionEmbedUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a session URL and authorization code that you can use to embed
-- the Amazon Amazon QuickSight console in your web server code. Use
-- @GetSessionEmbedUrl@ where you want to provide an authoring portal that
-- allows users to create data sources, datasets, analyses, and dashboards.
-- The users who access an embedded Amazon QuickSight console need belong
-- to the author or admin security cohort. If you want to restrict
-- permissions to some of these features, add a custom permissions profile
-- to the user with the
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_UpdateUser.html UpdateUser>@ @
-- API operation. Use
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_RegisterUser.html RegisterUser>@ @
-- API operation to add a new user with a custom permission profile
-- attached. For more information, see the following sections in the
-- /Amazon QuickSight User Guide/:
--
-- -   <https://docs.aws.amazon.com/quicksight/latest/user/embedded-analytics.html Embedding Analytics>
--
-- -   <https://docs.aws.amazon.com/quicksight/latest/user/customizing-permissions-to-the-quicksight-console.html Customizing Access to the Amazon QuickSight Console>
module Amazonka.QuickSight.GetSessionEmbedUrl
  ( -- * Creating a Request
    GetSessionEmbedUrl (..),
    newGetSessionEmbedUrl,

    -- * Request Lenses
    getSessionEmbedUrl_entryPoint,
    getSessionEmbedUrl_sessionLifetimeInMinutes,
    getSessionEmbedUrl_userArn,
    getSessionEmbedUrl_awsAccountId,

    -- * Destructuring the Response
    GetSessionEmbedUrlResponse (..),
    newGetSessionEmbedUrlResponse,

    -- * Response Lenses
    getSessionEmbedUrlResponse_embedUrl,
    getSessionEmbedUrlResponse_requestId,
    getSessionEmbedUrlResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSessionEmbedUrl' smart constructor.
data GetSessionEmbedUrl = GetSessionEmbedUrl'
  { -- | The URL you use to access the embedded session. The entry point URL is
    -- constrained to the following paths:
    --
    -- -   @\/start@
    --
    -- -   @\/start\/analyses@
    --
    -- -   @\/start\/dashboards@
    --
    -- -   @\/start\/favorites@
    --
    -- -   @\/dashboards\/@/@DashboardId@/@ @ - where @DashboardId@ is the
    --     actual ID key from the Amazon QuickSight console URL of the
    --     dashboard
    --
    -- -   @\/analyses\/@/@AnalysisId@/@ @ - where @AnalysisId@ is the actual
    --     ID key from the Amazon QuickSight console URL of the analysis
    entryPoint :: Prelude.Maybe Prelude.Text,
    -- | How many minutes the session is valid. The session lifetime must be
    -- 15-600 minutes.
    sessionLifetimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon QuickSight user\'s Amazon Resource Name (ARN), for use with
    -- @QUICKSIGHT@ identity type. You can use this for any type of Amazon
    -- QuickSight users in your account (readers, authors, or admins). They
    -- need to be authenticated as one of the following:
    --
    -- 1.  Active Directory (AD) users or group members
    --
    -- 2.  Invited nonfederated users
    --
    -- 3.  Identity and Access Management (IAM) users and IAM role-based
    --     sessions authenticated through Federated Single Sign-On using SAML,
    --     OpenID Connect, or IAM federation
    --
    -- Omit this parameter for users in the third group, IAM users and IAM
    -- role-based sessions.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account associated with your Amazon
    -- QuickSight subscription.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionEmbedUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryPoint', 'getSessionEmbedUrl_entryPoint' - The URL you use to access the embedded session. The entry point URL is
-- constrained to the following paths:
--
-- -   @\/start@
--
-- -   @\/start\/analyses@
--
-- -   @\/start\/dashboards@
--
-- -   @\/start\/favorites@
--
-- -   @\/dashboards\/@/@DashboardId@/@ @ - where @DashboardId@ is the
--     actual ID key from the Amazon QuickSight console URL of the
--     dashboard
--
-- -   @\/analyses\/@/@AnalysisId@/@ @ - where @AnalysisId@ is the actual
--     ID key from the Amazon QuickSight console URL of the analysis
--
-- 'sessionLifetimeInMinutes', 'getSessionEmbedUrl_sessionLifetimeInMinutes' - How many minutes the session is valid. The session lifetime must be
-- 15-600 minutes.
--
-- 'userArn', 'getSessionEmbedUrl_userArn' - The Amazon QuickSight user\'s Amazon Resource Name (ARN), for use with
-- @QUICKSIGHT@ identity type. You can use this for any type of Amazon
-- QuickSight users in your account (readers, authors, or admins). They
-- need to be authenticated as one of the following:
--
-- 1.  Active Directory (AD) users or group members
--
-- 2.  Invited nonfederated users
--
-- 3.  Identity and Access Management (IAM) users and IAM role-based
--     sessions authenticated through Federated Single Sign-On using SAML,
--     OpenID Connect, or IAM federation
--
-- Omit this parameter for users in the third group, IAM users and IAM
-- role-based sessions.
--
-- 'awsAccountId', 'getSessionEmbedUrl_awsAccountId' - The ID for the Amazon Web Services account associated with your Amazon
-- QuickSight subscription.
newGetSessionEmbedUrl ::
  -- | 'awsAccountId'
  Prelude.Text ->
  GetSessionEmbedUrl
newGetSessionEmbedUrl pAwsAccountId_ =
  GetSessionEmbedUrl'
    { entryPoint = Prelude.Nothing,
      sessionLifetimeInMinutes = Prelude.Nothing,
      userArn = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The URL you use to access the embedded session. The entry point URL is
-- constrained to the following paths:
--
-- -   @\/start@
--
-- -   @\/start\/analyses@
--
-- -   @\/start\/dashboards@
--
-- -   @\/start\/favorites@
--
-- -   @\/dashboards\/@/@DashboardId@/@ @ - where @DashboardId@ is the
--     actual ID key from the Amazon QuickSight console URL of the
--     dashboard
--
-- -   @\/analyses\/@/@AnalysisId@/@ @ - where @AnalysisId@ is the actual
--     ID key from the Amazon QuickSight console URL of the analysis
getSessionEmbedUrl_entryPoint :: Lens.Lens' GetSessionEmbedUrl (Prelude.Maybe Prelude.Text)
getSessionEmbedUrl_entryPoint = Lens.lens (\GetSessionEmbedUrl' {entryPoint} -> entryPoint) (\s@GetSessionEmbedUrl' {} a -> s {entryPoint = a} :: GetSessionEmbedUrl)

-- | How many minutes the session is valid. The session lifetime must be
-- 15-600 minutes.
getSessionEmbedUrl_sessionLifetimeInMinutes :: Lens.Lens' GetSessionEmbedUrl (Prelude.Maybe Prelude.Natural)
getSessionEmbedUrl_sessionLifetimeInMinutes = Lens.lens (\GetSessionEmbedUrl' {sessionLifetimeInMinutes} -> sessionLifetimeInMinutes) (\s@GetSessionEmbedUrl' {} a -> s {sessionLifetimeInMinutes = a} :: GetSessionEmbedUrl)

-- | The Amazon QuickSight user\'s Amazon Resource Name (ARN), for use with
-- @QUICKSIGHT@ identity type. You can use this for any type of Amazon
-- QuickSight users in your account (readers, authors, or admins). They
-- need to be authenticated as one of the following:
--
-- 1.  Active Directory (AD) users or group members
--
-- 2.  Invited nonfederated users
--
-- 3.  Identity and Access Management (IAM) users and IAM role-based
--     sessions authenticated through Federated Single Sign-On using SAML,
--     OpenID Connect, or IAM federation
--
-- Omit this parameter for users in the third group, IAM users and IAM
-- role-based sessions.
getSessionEmbedUrl_userArn :: Lens.Lens' GetSessionEmbedUrl (Prelude.Maybe Prelude.Text)
getSessionEmbedUrl_userArn = Lens.lens (\GetSessionEmbedUrl' {userArn} -> userArn) (\s@GetSessionEmbedUrl' {} a -> s {userArn = a} :: GetSessionEmbedUrl)

-- | The ID for the Amazon Web Services account associated with your Amazon
-- QuickSight subscription.
getSessionEmbedUrl_awsAccountId :: Lens.Lens' GetSessionEmbedUrl Prelude.Text
getSessionEmbedUrl_awsAccountId = Lens.lens (\GetSessionEmbedUrl' {awsAccountId} -> awsAccountId) (\s@GetSessionEmbedUrl' {} a -> s {awsAccountId = a} :: GetSessionEmbedUrl)

instance Core.AWSRequest GetSessionEmbedUrl where
  type
    AWSResponse GetSessionEmbedUrl =
      GetSessionEmbedUrlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionEmbedUrlResponse'
            Prelude.<$> (x Data..?> "EmbedUrl")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSessionEmbedUrl where
  hashWithSalt _salt GetSessionEmbedUrl' {..} =
    _salt
      `Prelude.hashWithSalt` entryPoint
      `Prelude.hashWithSalt` sessionLifetimeInMinutes
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData GetSessionEmbedUrl where
  rnf GetSessionEmbedUrl' {..} =
    Prelude.rnf entryPoint
      `Prelude.seq` Prelude.rnf sessionLifetimeInMinutes
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders GetSessionEmbedUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSessionEmbedUrl where
  toPath GetSessionEmbedUrl' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/session-embed-url"
      ]

instance Data.ToQuery GetSessionEmbedUrl where
  toQuery GetSessionEmbedUrl' {..} =
    Prelude.mconcat
      [ "entry-point" Data.=: entryPoint,
        "session-lifetime" Data.=: sessionLifetimeInMinutes,
        "user-arn" Data.=: userArn
      ]

-- | /See:/ 'newGetSessionEmbedUrlResponse' smart constructor.
data GetSessionEmbedUrlResponse = GetSessionEmbedUrlResponse'
  { -- | A single-use URL that you can put into your server-side web page to
    -- embed your Amazon QuickSight session. This URL is valid for 5 minutes.
    -- The API operation provides the URL with an @auth_code@ value that
    -- enables one (and only one) sign-on to a user session that is valid for
    -- 10 hours.
    embedUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionEmbedUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'embedUrl', 'getSessionEmbedUrlResponse_embedUrl' - A single-use URL that you can put into your server-side web page to
-- embed your Amazon QuickSight session. This URL is valid for 5 minutes.
-- The API operation provides the URL with an @auth_code@ value that
-- enables one (and only one) sign-on to a user session that is valid for
-- 10 hours.
--
-- 'requestId', 'getSessionEmbedUrlResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'getSessionEmbedUrlResponse_status' - The HTTP status of the request.
newGetSessionEmbedUrlResponse ::
  -- | 'status'
  Prelude.Int ->
  GetSessionEmbedUrlResponse
newGetSessionEmbedUrlResponse pStatus_ =
  GetSessionEmbedUrlResponse'
    { embedUrl =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A single-use URL that you can put into your server-side web page to
-- embed your Amazon QuickSight session. This URL is valid for 5 minutes.
-- The API operation provides the URL with an @auth_code@ value that
-- enables one (and only one) sign-on to a user session that is valid for
-- 10 hours.
getSessionEmbedUrlResponse_embedUrl :: Lens.Lens' GetSessionEmbedUrlResponse (Prelude.Maybe Prelude.Text)
getSessionEmbedUrlResponse_embedUrl = Lens.lens (\GetSessionEmbedUrlResponse' {embedUrl} -> embedUrl) (\s@GetSessionEmbedUrlResponse' {} a -> s {embedUrl = a} :: GetSessionEmbedUrlResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Web Services request ID for this operation.
getSessionEmbedUrlResponse_requestId :: Lens.Lens' GetSessionEmbedUrlResponse (Prelude.Maybe Prelude.Text)
getSessionEmbedUrlResponse_requestId = Lens.lens (\GetSessionEmbedUrlResponse' {requestId} -> requestId) (\s@GetSessionEmbedUrlResponse' {} a -> s {requestId = a} :: GetSessionEmbedUrlResponse)

-- | The HTTP status of the request.
getSessionEmbedUrlResponse_status :: Lens.Lens' GetSessionEmbedUrlResponse Prelude.Int
getSessionEmbedUrlResponse_status = Lens.lens (\GetSessionEmbedUrlResponse' {status} -> status) (\s@GetSessionEmbedUrlResponse' {} a -> s {status = a} :: GetSessionEmbedUrlResponse)

instance Prelude.NFData GetSessionEmbedUrlResponse where
  rnf GetSessionEmbedUrlResponse' {..} =
    Prelude.rnf embedUrl
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
