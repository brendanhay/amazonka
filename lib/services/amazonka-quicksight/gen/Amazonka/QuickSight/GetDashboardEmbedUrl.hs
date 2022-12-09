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
-- Module      : Amazonka.QuickSight.GetDashboardEmbedUrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a temporary session URL and authorization code(bearer token)
-- that you can use to embed an Amazon QuickSight read-only dashboard in
-- your website or application. Before you use this command, make sure that
-- you have configured the dashboards and permissions.
--
-- Currently, you can use @GetDashboardEmbedURL@ only from the server, not
-- from the user\'s browser. The following rules apply to the generated
-- URL:
--
-- -   They must be used together.
--
-- -   They can be used one time only.
--
-- -   They are valid for 5 minutes after you run this command.
--
-- -   You are charged only when the URL is used or there is interaction
--     with Amazon QuickSight.
--
-- -   The resulting user session is valid for 15 minutes (default) up to
--     10 hours (maximum). You can use the optional
--     @SessionLifetimeInMinutes@ parameter to customize session duration.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/embedded-analytics-deprecated.html Embedding Analytics Using GetDashboardEmbedUrl>
-- in the /Amazon QuickSight User Guide/.
--
-- For more information about the high-level steps for embedding and for an
-- interactive demo of the ways you can customize embedding, visit the
-- <https://docs.aws.amazon.com/quicksight/latest/user/quicksight-dev-portal.html Amazon QuickSight Developer Portal>.
module Amazonka.QuickSight.GetDashboardEmbedUrl
  ( -- * Creating a Request
    GetDashboardEmbedUrl (..),
    newGetDashboardEmbedUrl,

    -- * Request Lenses
    getDashboardEmbedUrl_additionalDashboardIds,
    getDashboardEmbedUrl_namespace,
    getDashboardEmbedUrl_resetDisabled,
    getDashboardEmbedUrl_sessionLifetimeInMinutes,
    getDashboardEmbedUrl_statePersistenceEnabled,
    getDashboardEmbedUrl_undoRedoDisabled,
    getDashboardEmbedUrl_userArn,
    getDashboardEmbedUrl_awsAccountId,
    getDashboardEmbedUrl_dashboardId,
    getDashboardEmbedUrl_identityType,

    -- * Destructuring the Response
    GetDashboardEmbedUrlResponse (..),
    newGetDashboardEmbedUrlResponse,

    -- * Response Lenses
    getDashboardEmbedUrlResponse_embedUrl,
    getDashboardEmbedUrlResponse_requestId,
    getDashboardEmbedUrlResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDashboardEmbedUrl' smart constructor.
data GetDashboardEmbedUrl = GetDashboardEmbedUrl'
  { -- | A list of one or more dashboard IDs that you want anonymous users to
    -- have tempporary access to. Currently, the @IdentityType@ parameter must
    -- be set to @ANONYMOUS@ because other identity types authenticate as
    -- Amazon QuickSight or IAM users. For example, if you set
    -- \"@--dashboard-id dash_id1 --dashboard-id dash_id2 dash_id3 identity-type ANONYMOUS@\",
    -- the session can access all three dashboards.
    additionalDashboardIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon QuickSight namespace that contains the dashboard IDs in this
    -- request. If you\'re not using a custom namespace, set
    -- @Namespace = default@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | Remove the reset button on the embedded dashboard. The default is FALSE,
    -- which enables the reset button.
    resetDisabled :: Prelude.Maybe Prelude.Bool,
    -- | How many minutes the session is valid. The session lifetime must be
    -- 15-600 minutes.
    sessionLifetimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Adds persistence of state for the user session in an embedded dashboard.
    -- Persistence applies to the sheet and the parameter settings. These are
    -- control settings that the dashboard subscriber (Amazon QuickSight
    -- reader) chooses while viewing the dashboard. If this is set to @TRUE@,
    -- the settings are the same when the subscriber reopens the same dashboard
    -- URL. The state is stored in Amazon QuickSight, not in a browser cookie.
    -- If this is set to FALSE, the state of the user session is not persisted.
    -- The default is @FALSE@.
    statePersistenceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Remove the undo\/redo button on the embedded dashboard. The default is
    -- FALSE, which enables the undo\/redo button.
    undoRedoDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon QuickSight user\'s Amazon Resource Name (ARN), for use with
    -- @QUICKSIGHT@ identity type. You can use this for any Amazon QuickSight
    -- users in your account (readers, authors, or admins) authenticated as one
    -- of the following:
    --
    -- -   Active Directory (AD) users or group members
    --
    -- -   Invited nonfederated users
    --
    -- -   IAM users and IAM role-based sessions authenticated through
    --     Federated Single Sign-On using SAML, OpenID Connect, or IAM
    --     federation.
    --
    -- Omit this parameter for users in the third group – IAM users and IAM
    -- role-based sessions.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that contains the dashboard
    -- that you\'re embedding.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard, also added to the Identity and Access
    -- Management (IAM) policy.
    dashboardId :: Prelude.Text,
    -- | The authentication method that the user uses to sign in.
    identityType :: EmbeddingIdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDashboardEmbedUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDashboardIds', 'getDashboardEmbedUrl_additionalDashboardIds' - A list of one or more dashboard IDs that you want anonymous users to
-- have tempporary access to. Currently, the @IdentityType@ parameter must
-- be set to @ANONYMOUS@ because other identity types authenticate as
-- Amazon QuickSight or IAM users. For example, if you set
-- \"@--dashboard-id dash_id1 --dashboard-id dash_id2 dash_id3 identity-type ANONYMOUS@\",
-- the session can access all three dashboards.
--
-- 'namespace', 'getDashboardEmbedUrl_namespace' - The Amazon QuickSight namespace that contains the dashboard IDs in this
-- request. If you\'re not using a custom namespace, set
-- @Namespace = default@.
--
-- 'resetDisabled', 'getDashboardEmbedUrl_resetDisabled' - Remove the reset button on the embedded dashboard. The default is FALSE,
-- which enables the reset button.
--
-- 'sessionLifetimeInMinutes', 'getDashboardEmbedUrl_sessionLifetimeInMinutes' - How many minutes the session is valid. The session lifetime must be
-- 15-600 minutes.
--
-- 'statePersistenceEnabled', 'getDashboardEmbedUrl_statePersistenceEnabled' - Adds persistence of state for the user session in an embedded dashboard.
-- Persistence applies to the sheet and the parameter settings. These are
-- control settings that the dashboard subscriber (Amazon QuickSight
-- reader) chooses while viewing the dashboard. If this is set to @TRUE@,
-- the settings are the same when the subscriber reopens the same dashboard
-- URL. The state is stored in Amazon QuickSight, not in a browser cookie.
-- If this is set to FALSE, the state of the user session is not persisted.
-- The default is @FALSE@.
--
-- 'undoRedoDisabled', 'getDashboardEmbedUrl_undoRedoDisabled' - Remove the undo\/redo button on the embedded dashboard. The default is
-- FALSE, which enables the undo\/redo button.
--
-- 'userArn', 'getDashboardEmbedUrl_userArn' - The Amazon QuickSight user\'s Amazon Resource Name (ARN), for use with
-- @QUICKSIGHT@ identity type. You can use this for any Amazon QuickSight
-- users in your account (readers, authors, or admins) authenticated as one
-- of the following:
--
-- -   Active Directory (AD) users or group members
--
-- -   Invited nonfederated users
--
-- -   IAM users and IAM role-based sessions authenticated through
--     Federated Single Sign-On using SAML, OpenID Connect, or IAM
--     federation.
--
-- Omit this parameter for users in the third group – IAM users and IAM
-- role-based sessions.
--
-- 'awsAccountId', 'getDashboardEmbedUrl_awsAccountId' - The ID for the Amazon Web Services account that contains the dashboard
-- that you\'re embedding.
--
-- 'dashboardId', 'getDashboardEmbedUrl_dashboardId' - The ID for the dashboard, also added to the Identity and Access
-- Management (IAM) policy.
--
-- 'identityType', 'getDashboardEmbedUrl_identityType' - The authentication method that the user uses to sign in.
newGetDashboardEmbedUrl ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'identityType'
  EmbeddingIdentityType ->
  GetDashboardEmbedUrl
newGetDashboardEmbedUrl
  pAwsAccountId_
  pDashboardId_
  pIdentityType_ =
    GetDashboardEmbedUrl'
      { additionalDashboardIds =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        resetDisabled = Prelude.Nothing,
        sessionLifetimeInMinutes = Prelude.Nothing,
        statePersistenceEnabled = Prelude.Nothing,
        undoRedoDisabled = Prelude.Nothing,
        userArn = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dashboardId = pDashboardId_,
        identityType = pIdentityType_
      }

-- | A list of one or more dashboard IDs that you want anonymous users to
-- have tempporary access to. Currently, the @IdentityType@ parameter must
-- be set to @ANONYMOUS@ because other identity types authenticate as
-- Amazon QuickSight or IAM users. For example, if you set
-- \"@--dashboard-id dash_id1 --dashboard-id dash_id2 dash_id3 identity-type ANONYMOUS@\",
-- the session can access all three dashboards.
getDashboardEmbedUrl_additionalDashboardIds :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getDashboardEmbedUrl_additionalDashboardIds = Lens.lens (\GetDashboardEmbedUrl' {additionalDashboardIds} -> additionalDashboardIds) (\s@GetDashboardEmbedUrl' {} a -> s {additionalDashboardIds = a} :: GetDashboardEmbedUrl) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon QuickSight namespace that contains the dashboard IDs in this
-- request. If you\'re not using a custom namespace, set
-- @Namespace = default@.
getDashboardEmbedUrl_namespace :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrl_namespace = Lens.lens (\GetDashboardEmbedUrl' {namespace} -> namespace) (\s@GetDashboardEmbedUrl' {} a -> s {namespace = a} :: GetDashboardEmbedUrl)

-- | Remove the reset button on the embedded dashboard. The default is FALSE,
-- which enables the reset button.
getDashboardEmbedUrl_resetDisabled :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Bool)
getDashboardEmbedUrl_resetDisabled = Lens.lens (\GetDashboardEmbedUrl' {resetDisabled} -> resetDisabled) (\s@GetDashboardEmbedUrl' {} a -> s {resetDisabled = a} :: GetDashboardEmbedUrl)

-- | How many minutes the session is valid. The session lifetime must be
-- 15-600 minutes.
getDashboardEmbedUrl_sessionLifetimeInMinutes :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Natural)
getDashboardEmbedUrl_sessionLifetimeInMinutes = Lens.lens (\GetDashboardEmbedUrl' {sessionLifetimeInMinutes} -> sessionLifetimeInMinutes) (\s@GetDashboardEmbedUrl' {} a -> s {sessionLifetimeInMinutes = a} :: GetDashboardEmbedUrl)

-- | Adds persistence of state for the user session in an embedded dashboard.
-- Persistence applies to the sheet and the parameter settings. These are
-- control settings that the dashboard subscriber (Amazon QuickSight
-- reader) chooses while viewing the dashboard. If this is set to @TRUE@,
-- the settings are the same when the subscriber reopens the same dashboard
-- URL. The state is stored in Amazon QuickSight, not in a browser cookie.
-- If this is set to FALSE, the state of the user session is not persisted.
-- The default is @FALSE@.
getDashboardEmbedUrl_statePersistenceEnabled :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Bool)
getDashboardEmbedUrl_statePersistenceEnabled = Lens.lens (\GetDashboardEmbedUrl' {statePersistenceEnabled} -> statePersistenceEnabled) (\s@GetDashboardEmbedUrl' {} a -> s {statePersistenceEnabled = a} :: GetDashboardEmbedUrl)

-- | Remove the undo\/redo button on the embedded dashboard. The default is
-- FALSE, which enables the undo\/redo button.
getDashboardEmbedUrl_undoRedoDisabled :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Bool)
getDashboardEmbedUrl_undoRedoDisabled = Lens.lens (\GetDashboardEmbedUrl' {undoRedoDisabled} -> undoRedoDisabled) (\s@GetDashboardEmbedUrl' {} a -> s {undoRedoDisabled = a} :: GetDashboardEmbedUrl)

-- | The Amazon QuickSight user\'s Amazon Resource Name (ARN), for use with
-- @QUICKSIGHT@ identity type. You can use this for any Amazon QuickSight
-- users in your account (readers, authors, or admins) authenticated as one
-- of the following:
--
-- -   Active Directory (AD) users or group members
--
-- -   Invited nonfederated users
--
-- -   IAM users and IAM role-based sessions authenticated through
--     Federated Single Sign-On using SAML, OpenID Connect, or IAM
--     federation.
--
-- Omit this parameter for users in the third group – IAM users and IAM
-- role-based sessions.
getDashboardEmbedUrl_userArn :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrl_userArn = Lens.lens (\GetDashboardEmbedUrl' {userArn} -> userArn) (\s@GetDashboardEmbedUrl' {} a -> s {userArn = a} :: GetDashboardEmbedUrl)

-- | The ID for the Amazon Web Services account that contains the dashboard
-- that you\'re embedding.
getDashboardEmbedUrl_awsAccountId :: Lens.Lens' GetDashboardEmbedUrl Prelude.Text
getDashboardEmbedUrl_awsAccountId = Lens.lens (\GetDashboardEmbedUrl' {awsAccountId} -> awsAccountId) (\s@GetDashboardEmbedUrl' {} a -> s {awsAccountId = a} :: GetDashboardEmbedUrl)

-- | The ID for the dashboard, also added to the Identity and Access
-- Management (IAM) policy.
getDashboardEmbedUrl_dashboardId :: Lens.Lens' GetDashboardEmbedUrl Prelude.Text
getDashboardEmbedUrl_dashboardId = Lens.lens (\GetDashboardEmbedUrl' {dashboardId} -> dashboardId) (\s@GetDashboardEmbedUrl' {} a -> s {dashboardId = a} :: GetDashboardEmbedUrl)

-- | The authentication method that the user uses to sign in.
getDashboardEmbedUrl_identityType :: Lens.Lens' GetDashboardEmbedUrl EmbeddingIdentityType
getDashboardEmbedUrl_identityType = Lens.lens (\GetDashboardEmbedUrl' {identityType} -> identityType) (\s@GetDashboardEmbedUrl' {} a -> s {identityType = a} :: GetDashboardEmbedUrl)

instance Core.AWSRequest GetDashboardEmbedUrl where
  type
    AWSResponse GetDashboardEmbedUrl =
      GetDashboardEmbedUrlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDashboardEmbedUrlResponse'
            Prelude.<$> (x Data..?> "EmbedUrl")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDashboardEmbedUrl where
  hashWithSalt _salt GetDashboardEmbedUrl' {..} =
    _salt `Prelude.hashWithSalt` additionalDashboardIds
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` resetDisabled
      `Prelude.hashWithSalt` sessionLifetimeInMinutes
      `Prelude.hashWithSalt` statePersistenceEnabled
      `Prelude.hashWithSalt` undoRedoDisabled
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` identityType

instance Prelude.NFData GetDashboardEmbedUrl where
  rnf GetDashboardEmbedUrl' {..} =
    Prelude.rnf additionalDashboardIds
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf resetDisabled
      `Prelude.seq` Prelude.rnf sessionLifetimeInMinutes
      `Prelude.seq` Prelude.rnf statePersistenceEnabled
      `Prelude.seq` Prelude.rnf undoRedoDisabled
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf identityType

instance Data.ToHeaders GetDashboardEmbedUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDashboardEmbedUrl where
  toPath GetDashboardEmbedUrl' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId,
        "/embed-url"
      ]

instance Data.ToQuery GetDashboardEmbedUrl where
  toQuery GetDashboardEmbedUrl' {..} =
    Prelude.mconcat
      [ "additional-dashboard-ids"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> additionalDashboardIds
            ),
        "namespace" Data.=: namespace,
        "reset-disabled" Data.=: resetDisabled,
        "session-lifetime" Data.=: sessionLifetimeInMinutes,
        "state-persistence-enabled"
          Data.=: statePersistenceEnabled,
        "undo-redo-disabled" Data.=: undoRedoDisabled,
        "user-arn" Data.=: userArn,
        "creds-type" Data.=: identityType
      ]

-- | Output returned from the @GetDashboardEmbedUrl@ operation.
--
-- /See:/ 'newGetDashboardEmbedUrlResponse' smart constructor.
data GetDashboardEmbedUrlResponse = GetDashboardEmbedUrlResponse'
  { -- | A single-use URL that you can put into your server-side webpage to embed
    -- your dashboard. This URL is valid for 5 minutes. The API operation
    -- provides the URL with an @auth_code@ value that enables one (and only
    -- one) sign-on to a user session that is valid for 10 hours.
    embedUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDashboardEmbedUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'embedUrl', 'getDashboardEmbedUrlResponse_embedUrl' - A single-use URL that you can put into your server-side webpage to embed
-- your dashboard. This URL is valid for 5 minutes. The API operation
-- provides the URL with an @auth_code@ value that enables one (and only
-- one) sign-on to a user session that is valid for 10 hours.
--
-- 'requestId', 'getDashboardEmbedUrlResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'getDashboardEmbedUrlResponse_status' - The HTTP status of the request.
newGetDashboardEmbedUrlResponse ::
  -- | 'status'
  Prelude.Int ->
  GetDashboardEmbedUrlResponse
newGetDashboardEmbedUrlResponse pStatus_ =
  GetDashboardEmbedUrlResponse'
    { embedUrl =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A single-use URL that you can put into your server-side webpage to embed
-- your dashboard. This URL is valid for 5 minutes. The API operation
-- provides the URL with an @auth_code@ value that enables one (and only
-- one) sign-on to a user session that is valid for 10 hours.
getDashboardEmbedUrlResponse_embedUrl :: Lens.Lens' GetDashboardEmbedUrlResponse (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrlResponse_embedUrl = Lens.lens (\GetDashboardEmbedUrlResponse' {embedUrl} -> embedUrl) (\s@GetDashboardEmbedUrlResponse' {} a -> s {embedUrl = a} :: GetDashboardEmbedUrlResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Web Services request ID for this operation.
getDashboardEmbedUrlResponse_requestId :: Lens.Lens' GetDashboardEmbedUrlResponse (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrlResponse_requestId = Lens.lens (\GetDashboardEmbedUrlResponse' {requestId} -> requestId) (\s@GetDashboardEmbedUrlResponse' {} a -> s {requestId = a} :: GetDashboardEmbedUrlResponse)

-- | The HTTP status of the request.
getDashboardEmbedUrlResponse_status :: Lens.Lens' GetDashboardEmbedUrlResponse Prelude.Int
getDashboardEmbedUrlResponse_status = Lens.lens (\GetDashboardEmbedUrlResponse' {status} -> status) (\s@GetDashboardEmbedUrlResponse' {} a -> s {status = a} :: GetDashboardEmbedUrlResponse)

instance Prelude.NFData GetDashboardEmbedUrlResponse where
  rnf GetDashboardEmbedUrlResponse' {..} =
    Prelude.rnf embedUrl
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
