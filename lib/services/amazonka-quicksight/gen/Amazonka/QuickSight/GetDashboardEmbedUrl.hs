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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a session URL and authorization code that you can use to embed
-- an Amazon Amazon QuickSight read-only dashboard in your web server code.
-- Before you use this command, make sure that you have configured the
-- dashboards and permissions.
--
-- Currently, you can use @GetDashboardEmbedURL@ only from the server, not
-- from the user\'s browser. The following rules apply to the combination
-- of URL and authorization code:
--
-- -   They must be used together.
--
-- -   They can be used one time only.
--
-- -   They are valid for 5 minutes after you run this command.
--
-- -   The resulting user session is valid for 10 hours.
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
    getDashboardEmbedUrl_sessionLifetimeInMinutes,
    getDashboardEmbedUrl_statePersistenceEnabled,
    getDashboardEmbedUrl_namespace,
    getDashboardEmbedUrl_additionalDashboardIds,
    getDashboardEmbedUrl_undoRedoDisabled,
    getDashboardEmbedUrl_userArn,
    getDashboardEmbedUrl_resetDisabled,
    getDashboardEmbedUrl_awsAccountId,
    getDashboardEmbedUrl_dashboardId,
    getDashboardEmbedUrl_identityType,

    -- * Destructuring the Response
    GetDashboardEmbedUrlResponse (..),
    newGetDashboardEmbedUrlResponse,

    -- * Response Lenses
    getDashboardEmbedUrlResponse_requestId,
    getDashboardEmbedUrlResponse_embedUrl,
    getDashboardEmbedUrlResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDashboardEmbedUrl' smart constructor.
data GetDashboardEmbedUrl = GetDashboardEmbedUrl'
  { -- | How many minutes the session is valid. The session lifetime must be
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
    -- | The Amazon QuickSight namespace that contains the dashboard IDs in this
    -- request. If you\'re not using a custom namespace, set
    -- @Namespace = default@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | A list of one or more dashboard IDs that you want to add to a session
    -- that includes anonymous users. The @IdentityType@ parameter must be set
    -- to @ANONYMOUS@ for this to work, because other identity types
    -- authenticate as Amazon QuickSight or IAMusers. For example, if you set
    -- \"@--dashboard-id dash_id1 --dashboard-id dash_id2 dash_id3 identity-type ANONYMOUS@\",
    -- the session can access all three dashboards.
    additionalDashboardIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
    -- -   IAMusers and IAMrole-based sessions authenticated through Federated
    --     Single Sign-On using SAML, OpenID Connect, or IAMfederation.
    --
    -- Omit this parameter for users in the third group – IAMusers and IAM
    -- role-based sessions.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | Remove the reset button on the embedded dashboard. The default is FALSE,
    -- which enables the reset button.
    resetDisabled :: Prelude.Maybe Prelude.Bool,
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
-- 'namespace', 'getDashboardEmbedUrl_namespace' - The Amazon QuickSight namespace that contains the dashboard IDs in this
-- request. If you\'re not using a custom namespace, set
-- @Namespace = default@.
--
-- 'additionalDashboardIds', 'getDashboardEmbedUrl_additionalDashboardIds' - A list of one or more dashboard IDs that you want to add to a session
-- that includes anonymous users. The @IdentityType@ parameter must be set
-- to @ANONYMOUS@ for this to work, because other identity types
-- authenticate as Amazon QuickSight or IAMusers. For example, if you set
-- \"@--dashboard-id dash_id1 --dashboard-id dash_id2 dash_id3 identity-type ANONYMOUS@\",
-- the session can access all three dashboards.
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
-- -   IAMusers and IAMrole-based sessions authenticated through Federated
--     Single Sign-On using SAML, OpenID Connect, or IAMfederation.
--
-- Omit this parameter for users in the third group – IAMusers and IAM
-- role-based sessions.
--
-- 'resetDisabled', 'getDashboardEmbedUrl_resetDisabled' - Remove the reset button on the embedded dashboard. The default is FALSE,
-- which enables the reset button.
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
      { sessionLifetimeInMinutes =
          Prelude.Nothing,
        statePersistenceEnabled = Prelude.Nothing,
        namespace = Prelude.Nothing,
        additionalDashboardIds = Prelude.Nothing,
        undoRedoDisabled = Prelude.Nothing,
        userArn = Prelude.Nothing,
        resetDisabled = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dashboardId = pDashboardId_,
        identityType = pIdentityType_
      }

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

-- | The Amazon QuickSight namespace that contains the dashboard IDs in this
-- request. If you\'re not using a custom namespace, set
-- @Namespace = default@.
getDashboardEmbedUrl_namespace :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrl_namespace = Lens.lens (\GetDashboardEmbedUrl' {namespace} -> namespace) (\s@GetDashboardEmbedUrl' {} a -> s {namespace = a} :: GetDashboardEmbedUrl)

-- | A list of one or more dashboard IDs that you want to add to a session
-- that includes anonymous users. The @IdentityType@ parameter must be set
-- to @ANONYMOUS@ for this to work, because other identity types
-- authenticate as Amazon QuickSight or IAMusers. For example, if you set
-- \"@--dashboard-id dash_id1 --dashboard-id dash_id2 dash_id3 identity-type ANONYMOUS@\",
-- the session can access all three dashboards.
getDashboardEmbedUrl_additionalDashboardIds :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getDashboardEmbedUrl_additionalDashboardIds = Lens.lens (\GetDashboardEmbedUrl' {additionalDashboardIds} -> additionalDashboardIds) (\s@GetDashboardEmbedUrl' {} a -> s {additionalDashboardIds = a} :: GetDashboardEmbedUrl) Prelude.. Lens.mapping Lens.coerced

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
-- -   IAMusers and IAMrole-based sessions authenticated through Federated
--     Single Sign-On using SAML, OpenID Connect, or IAMfederation.
--
-- Omit this parameter for users in the third group – IAMusers and IAM
-- role-based sessions.
getDashboardEmbedUrl_userArn :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrl_userArn = Lens.lens (\GetDashboardEmbedUrl' {userArn} -> userArn) (\s@GetDashboardEmbedUrl' {} a -> s {userArn = a} :: GetDashboardEmbedUrl)

-- | Remove the reset button on the embedded dashboard. The default is FALSE,
-- which enables the reset button.
getDashboardEmbedUrl_resetDisabled :: Lens.Lens' GetDashboardEmbedUrl (Prelude.Maybe Prelude.Bool)
getDashboardEmbedUrl_resetDisabled = Lens.lens (\GetDashboardEmbedUrl' {resetDisabled} -> resetDisabled) (\s@GetDashboardEmbedUrl' {} a -> s {resetDisabled = a} :: GetDashboardEmbedUrl)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDashboardEmbedUrlResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "EmbedUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDashboardEmbedUrl where
  hashWithSalt salt' GetDashboardEmbedUrl' {..} =
    salt' `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` resetDisabled
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` undoRedoDisabled
      `Prelude.hashWithSalt` additionalDashboardIds
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` statePersistenceEnabled
      `Prelude.hashWithSalt` sessionLifetimeInMinutes

instance Prelude.NFData GetDashboardEmbedUrl where
  rnf GetDashboardEmbedUrl' {..} =
    Prelude.rnf sessionLifetimeInMinutes
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf resetDisabled
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf undoRedoDisabled
      `Prelude.seq` Prelude.rnf additionalDashboardIds
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statePersistenceEnabled

instance Core.ToHeaders GetDashboardEmbedUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDashboardEmbedUrl where
  toPath GetDashboardEmbedUrl' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/dashboards/",
        Core.toBS dashboardId,
        "/embed-url"
      ]

instance Core.ToQuery GetDashboardEmbedUrl where
  toQuery GetDashboardEmbedUrl' {..} =
    Prelude.mconcat
      [ "session-lifetime" Core.=: sessionLifetimeInMinutes,
        "state-persistence-enabled"
          Core.=: statePersistenceEnabled,
        "namespace" Core.=: namespace,
        "additional-dashboard-ids"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> additionalDashboardIds
            ),
        "undo-redo-disabled" Core.=: undoRedoDisabled,
        "user-arn" Core.=: userArn,
        "reset-disabled" Core.=: resetDisabled,
        "creds-type" Core.=: identityType
      ]

-- | Output returned from the @GetDashboardEmbedUrl@ operation.
--
-- /See:/ 'newGetDashboardEmbedUrlResponse' smart constructor.
data GetDashboardEmbedUrlResponse = GetDashboardEmbedUrlResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A single-use URL that you can put into your server-side webpage to embed
    -- your dashboard. This URL is valid for 5 minutes. The API operation
    -- provides the URL with an @auth_code@ value that enables one (and only
    -- one) sign-on to a user session that is valid for 10 hours.
    embedUrl :: Prelude.Maybe (Core.Sensitive Prelude.Text),
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
-- 'requestId', 'getDashboardEmbedUrlResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'embedUrl', 'getDashboardEmbedUrlResponse_embedUrl' - A single-use URL that you can put into your server-side webpage to embed
-- your dashboard. This URL is valid for 5 minutes. The API operation
-- provides the URL with an @auth_code@ value that enables one (and only
-- one) sign-on to a user session that is valid for 10 hours.
--
-- 'status', 'getDashboardEmbedUrlResponse_status' - The HTTP status of the request.
newGetDashboardEmbedUrlResponse ::
  -- | 'status'
  Prelude.Int ->
  GetDashboardEmbedUrlResponse
newGetDashboardEmbedUrlResponse pStatus_ =
  GetDashboardEmbedUrlResponse'
    { requestId =
        Prelude.Nothing,
      embedUrl = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
getDashboardEmbedUrlResponse_requestId :: Lens.Lens' GetDashboardEmbedUrlResponse (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrlResponse_requestId = Lens.lens (\GetDashboardEmbedUrlResponse' {requestId} -> requestId) (\s@GetDashboardEmbedUrlResponse' {} a -> s {requestId = a} :: GetDashboardEmbedUrlResponse)

-- | A single-use URL that you can put into your server-side webpage to embed
-- your dashboard. This URL is valid for 5 minutes. The API operation
-- provides the URL with an @auth_code@ value that enables one (and only
-- one) sign-on to a user session that is valid for 10 hours.
getDashboardEmbedUrlResponse_embedUrl :: Lens.Lens' GetDashboardEmbedUrlResponse (Prelude.Maybe Prelude.Text)
getDashboardEmbedUrlResponse_embedUrl = Lens.lens (\GetDashboardEmbedUrlResponse' {embedUrl} -> embedUrl) (\s@GetDashboardEmbedUrlResponse' {} a -> s {embedUrl = a} :: GetDashboardEmbedUrlResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The HTTP status of the request.
getDashboardEmbedUrlResponse_status :: Lens.Lens' GetDashboardEmbedUrlResponse Prelude.Int
getDashboardEmbedUrlResponse_status = Lens.lens (\GetDashboardEmbedUrlResponse' {status} -> status) (\s@GetDashboardEmbedUrlResponse' {} a -> s {status = a} :: GetDashboardEmbedUrlResponse)

instance Prelude.NFData GetDashboardEmbedUrlResponse where
  rnf GetDashboardEmbedUrlResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf embedUrl
