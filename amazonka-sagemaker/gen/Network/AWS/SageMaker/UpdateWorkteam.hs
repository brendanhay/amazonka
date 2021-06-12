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
-- Module      : Network.AWS.SageMaker.UpdateWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing work team with new member definitions or
-- description.
module Network.AWS.SageMaker.UpdateWorkteam
  ( -- * Creating a Request
    UpdateWorkteam (..),
    newUpdateWorkteam,

    -- * Request Lenses
    updateWorkteam_memberDefinitions,
    updateWorkteam_notificationConfiguration,
    updateWorkteam_description,
    updateWorkteam_workteamName,

    -- * Destructuring the Response
    UpdateWorkteamResponse (..),
    newUpdateWorkteamResponse,

    -- * Response Lenses
    updateWorkteamResponse_httpStatus,
    updateWorkteamResponse_workteam,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateWorkteam' smart constructor.
data UpdateWorkteam = UpdateWorkteam'
  { -- | A list of @MemberDefinition@ objects that contains objects that identify
    -- the workers that make up the work team.
    --
    -- Workforces can be created using Amazon Cognito or your own OIDC Identity
    -- Provider (IdP). For private workforces created using Amazon Cognito use
    -- @CognitoMemberDefinition@. For workforces created using your own OIDC
    -- identity provider (IdP) use @OidcMemberDefinition@. You should not
    -- provide input for both of these parameters in a single request.
    --
    -- For workforces created using Amazon Cognito, private work teams
    -- correspond to Amazon Cognito /user groups/ within the user pool used to
    -- create a workforce. All of the @CognitoMemberDefinition@ objects that
    -- make up the member definition must have the same @ClientId@ and
    -- @UserPool@ values. To add a Amazon Cognito user group to an existing
    -- worker pool, see < Adding groups to a User Pool>. For more information
    -- about user pools, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools>.
    --
    -- For workforces created using your own OIDC IdP, specify the user groups
    -- that you want to include in your private work team in
    -- @OidcMemberDefinition@ by listing those groups in @Groups@. Be aware
    -- that user groups that are already in the work team must also be listed
    -- in @Groups@ when you make this request to remain on the work team. If
    -- you do not include these user groups, they will no longer be associated
    -- with the work team you update.
    memberDefinitions :: Core.Maybe (Core.NonEmpty MemberDefinition),
    -- | Configures SNS topic notifications for available or expiring work items
    notificationConfiguration :: Core.Maybe NotificationConfiguration,
    -- | An updated description for the work team.
    description :: Core.Maybe Core.Text,
    -- | The name of the work team to update.
    workteamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberDefinitions', 'updateWorkteam_memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify
-- the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity
-- Provider (IdP). For private workforces created using Amazon Cognito use
-- @CognitoMemberDefinition@. For workforces created using your own OIDC
-- identity provider (IdP) use @OidcMemberDefinition@. You should not
-- provide input for both of these parameters in a single request.
--
-- For workforces created using Amazon Cognito, private work teams
-- correspond to Amazon Cognito /user groups/ within the user pool used to
-- create a workforce. All of the @CognitoMemberDefinition@ objects that
-- make up the member definition must have the same @ClientId@ and
-- @UserPool@ values. To add a Amazon Cognito user group to an existing
-- worker pool, see < Adding groups to a User Pool>. For more information
-- about user pools, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools>.
--
-- For workforces created using your own OIDC IdP, specify the user groups
-- that you want to include in your private work team in
-- @OidcMemberDefinition@ by listing those groups in @Groups@. Be aware
-- that user groups that are already in the work team must also be listed
-- in @Groups@ when you make this request to remain on the work team. If
-- you do not include these user groups, they will no longer be associated
-- with the work team you update.
--
-- 'notificationConfiguration', 'updateWorkteam_notificationConfiguration' - Configures SNS topic notifications for available or expiring work items
--
-- 'description', 'updateWorkteam_description' - An updated description for the work team.
--
-- 'workteamName', 'updateWorkteam_workteamName' - The name of the work team to update.
newUpdateWorkteam ::
  -- | 'workteamName'
  Core.Text ->
  UpdateWorkteam
newUpdateWorkteam pWorkteamName_ =
  UpdateWorkteam'
    { memberDefinitions = Core.Nothing,
      notificationConfiguration = Core.Nothing,
      description = Core.Nothing,
      workteamName = pWorkteamName_
    }

-- | A list of @MemberDefinition@ objects that contains objects that identify
-- the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity
-- Provider (IdP). For private workforces created using Amazon Cognito use
-- @CognitoMemberDefinition@. For workforces created using your own OIDC
-- identity provider (IdP) use @OidcMemberDefinition@. You should not
-- provide input for both of these parameters in a single request.
--
-- For workforces created using Amazon Cognito, private work teams
-- correspond to Amazon Cognito /user groups/ within the user pool used to
-- create a workforce. All of the @CognitoMemberDefinition@ objects that
-- make up the member definition must have the same @ClientId@ and
-- @UserPool@ values. To add a Amazon Cognito user group to an existing
-- worker pool, see < Adding groups to a User Pool>. For more information
-- about user pools, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools>.
--
-- For workforces created using your own OIDC IdP, specify the user groups
-- that you want to include in your private work team in
-- @OidcMemberDefinition@ by listing those groups in @Groups@. Be aware
-- that user groups that are already in the work team must also be listed
-- in @Groups@ when you make this request to remain on the work team. If
-- you do not include these user groups, they will no longer be associated
-- with the work team you update.
updateWorkteam_memberDefinitions :: Lens.Lens' UpdateWorkteam (Core.Maybe (Core.NonEmpty MemberDefinition))
updateWorkteam_memberDefinitions = Lens.lens (\UpdateWorkteam' {memberDefinitions} -> memberDefinitions) (\s@UpdateWorkteam' {} a -> s {memberDefinitions = a} :: UpdateWorkteam) Core.. Lens.mapping Lens._Coerce

-- | Configures SNS topic notifications for available or expiring work items
updateWorkteam_notificationConfiguration :: Lens.Lens' UpdateWorkteam (Core.Maybe NotificationConfiguration)
updateWorkteam_notificationConfiguration = Lens.lens (\UpdateWorkteam' {notificationConfiguration} -> notificationConfiguration) (\s@UpdateWorkteam' {} a -> s {notificationConfiguration = a} :: UpdateWorkteam)

-- | An updated description for the work team.
updateWorkteam_description :: Lens.Lens' UpdateWorkteam (Core.Maybe Core.Text)
updateWorkteam_description = Lens.lens (\UpdateWorkteam' {description} -> description) (\s@UpdateWorkteam' {} a -> s {description = a} :: UpdateWorkteam)

-- | The name of the work team to update.
updateWorkteam_workteamName :: Lens.Lens' UpdateWorkteam Core.Text
updateWorkteam_workteamName = Lens.lens (\UpdateWorkteam' {workteamName} -> workteamName) (\s@UpdateWorkteam' {} a -> s {workteamName = a} :: UpdateWorkteam)

instance Core.AWSRequest UpdateWorkteam where
  type
    AWSResponse UpdateWorkteam =
      UpdateWorkteamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkteamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Workteam")
      )

instance Core.Hashable UpdateWorkteam

instance Core.NFData UpdateWorkteam

instance Core.ToHeaders UpdateWorkteam where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateWorkteam" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateWorkteam where
  toJSON UpdateWorkteam' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MemberDefinitions" Core..=)
              Core.<$> memberDefinitions,
            ("NotificationConfiguration" Core..=)
              Core.<$> notificationConfiguration,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("WorkteamName" Core..= workteamName)
          ]
      )

instance Core.ToPath UpdateWorkteam where
  toPath = Core.const "/"

instance Core.ToQuery UpdateWorkteam where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateWorkteamResponse' smart constructor.
data UpdateWorkteamResponse = UpdateWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A @Workteam@ object that describes the updated work team.
    workteam :: Workteam
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'workteam', 'updateWorkteamResponse_workteam' - A @Workteam@ object that describes the updated work team.
newUpdateWorkteamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'workteam'
  Workteam ->
  UpdateWorkteamResponse
newUpdateWorkteamResponse pHttpStatus_ pWorkteam_ =
  UpdateWorkteamResponse'
    { httpStatus = pHttpStatus_,
      workteam = pWorkteam_
    }

-- | The response's http status code.
updateWorkteamResponse_httpStatus :: Lens.Lens' UpdateWorkteamResponse Core.Int
updateWorkteamResponse_httpStatus = Lens.lens (\UpdateWorkteamResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkteamResponse' {} a -> s {httpStatus = a} :: UpdateWorkteamResponse)

-- | A @Workteam@ object that describes the updated work team.
updateWorkteamResponse_workteam :: Lens.Lens' UpdateWorkteamResponse Workteam
updateWorkteamResponse_workteam = Lens.lens (\UpdateWorkteamResponse' {workteam} -> workteam) (\s@UpdateWorkteamResponse' {} a -> s {workteam = a} :: UpdateWorkteamResponse)

instance Core.NFData UpdateWorkteamResponse
