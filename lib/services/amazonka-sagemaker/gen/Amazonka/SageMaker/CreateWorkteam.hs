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
-- Module      : Amazonka.SageMaker.CreateWorkteam
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new work team for labeling your data. A work team is defined
-- by one or more Amazon Cognito user pools. You must first create the user
-- pools before you can create a work team.
--
-- You cannot create more than 25 work teams in an account and region.
module Amazonka.SageMaker.CreateWorkteam
  ( -- * Creating a Request
    CreateWorkteam (..),
    newCreateWorkteam,

    -- * Request Lenses
    createWorkteam_tags,
    createWorkteam_notificationConfiguration,
    createWorkteam_workforceName,
    createWorkteam_workteamName,
    createWorkteam_memberDefinitions,
    createWorkteam_description,

    -- * Destructuring the Response
    CreateWorkteamResponse (..),
    newCreateWorkteamResponse,

    -- * Response Lenses
    createWorkteamResponse_workteamArn,
    createWorkteamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateWorkteam' smart constructor.
data CreateWorkteam = CreateWorkteam'
  { -- | An array of key-value pairs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag>
    -- and
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | Configures notification of workers regarding available or expiring work
    -- items.
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The name of the workforce.
    workforceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the work team. Use this name to identify the work team.
    workteamName :: Prelude.Text,
    -- | A list of @MemberDefinition@ objects that contains objects that identify
    -- the workers that make up the work team.
    --
    -- Workforces can be created using Amazon Cognito or your own OIDC Identity
    -- Provider (IdP). For private workforces created using Amazon Cognito use
    -- @CognitoMemberDefinition@. For workforces created using your own OIDC
    -- identity provider (IdP) use @OidcMemberDefinition@. Do not provide input
    -- for both of these parameters in a single request.
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
    -- @OidcMemberDefinition@ by listing those groups in @Groups@.
    memberDefinitions :: Prelude.NonEmpty MemberDefinition,
    -- | A description of the work team.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorkteam_tags' - An array of key-value pairs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag>
-- and
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
--
-- 'notificationConfiguration', 'createWorkteam_notificationConfiguration' - Configures notification of workers regarding available or expiring work
-- items.
--
-- 'workforceName', 'createWorkteam_workforceName' - The name of the workforce.
--
-- 'workteamName', 'createWorkteam_workteamName' - The name of the work team. Use this name to identify the work team.
--
-- 'memberDefinitions', 'createWorkteam_memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify
-- the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity
-- Provider (IdP). For private workforces created using Amazon Cognito use
-- @CognitoMemberDefinition@. For workforces created using your own OIDC
-- identity provider (IdP) use @OidcMemberDefinition@. Do not provide input
-- for both of these parameters in a single request.
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
-- @OidcMemberDefinition@ by listing those groups in @Groups@.
--
-- 'description', 'createWorkteam_description' - A description of the work team.
newCreateWorkteam ::
  -- | 'workteamName'
  Prelude.Text ->
  -- | 'memberDefinitions'
  Prelude.NonEmpty MemberDefinition ->
  -- | 'description'
  Prelude.Text ->
  CreateWorkteam
newCreateWorkteam
  pWorkteamName_
  pMemberDefinitions_
  pDescription_ =
    CreateWorkteam'
      { tags = Prelude.Nothing,
        notificationConfiguration = Prelude.Nothing,
        workforceName = Prelude.Nothing,
        workteamName = pWorkteamName_,
        memberDefinitions =
          Lens.coerced Lens.# pMemberDefinitions_,
        description = pDescription_
      }

-- | An array of key-value pairs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag>
-- and
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
createWorkteam_tags :: Lens.Lens' CreateWorkteam (Prelude.Maybe [Tag])
createWorkteam_tags = Lens.lens (\CreateWorkteam' {tags} -> tags) (\s@CreateWorkteam' {} a -> s {tags = a} :: CreateWorkteam) Prelude.. Lens.mapping Lens.coerced

-- | Configures notification of workers regarding available or expiring work
-- items.
createWorkteam_notificationConfiguration :: Lens.Lens' CreateWorkteam (Prelude.Maybe NotificationConfiguration)
createWorkteam_notificationConfiguration = Lens.lens (\CreateWorkteam' {notificationConfiguration} -> notificationConfiguration) (\s@CreateWorkteam' {} a -> s {notificationConfiguration = a} :: CreateWorkteam)

-- | The name of the workforce.
createWorkteam_workforceName :: Lens.Lens' CreateWorkteam (Prelude.Maybe Prelude.Text)
createWorkteam_workforceName = Lens.lens (\CreateWorkteam' {workforceName} -> workforceName) (\s@CreateWorkteam' {} a -> s {workforceName = a} :: CreateWorkteam)

-- | The name of the work team. Use this name to identify the work team.
createWorkteam_workteamName :: Lens.Lens' CreateWorkteam Prelude.Text
createWorkteam_workteamName = Lens.lens (\CreateWorkteam' {workteamName} -> workteamName) (\s@CreateWorkteam' {} a -> s {workteamName = a} :: CreateWorkteam)

-- | A list of @MemberDefinition@ objects that contains objects that identify
-- the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity
-- Provider (IdP). For private workforces created using Amazon Cognito use
-- @CognitoMemberDefinition@. For workforces created using your own OIDC
-- identity provider (IdP) use @OidcMemberDefinition@. Do not provide input
-- for both of these parameters in a single request.
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
-- @OidcMemberDefinition@ by listing those groups in @Groups@.
createWorkteam_memberDefinitions :: Lens.Lens' CreateWorkteam (Prelude.NonEmpty MemberDefinition)
createWorkteam_memberDefinitions = Lens.lens (\CreateWorkteam' {memberDefinitions} -> memberDefinitions) (\s@CreateWorkteam' {} a -> s {memberDefinitions = a} :: CreateWorkteam) Prelude.. Lens.coerced

-- | A description of the work team.
createWorkteam_description :: Lens.Lens' CreateWorkteam Prelude.Text
createWorkteam_description = Lens.lens (\CreateWorkteam' {description} -> description) (\s@CreateWorkteam' {} a -> s {description = a} :: CreateWorkteam)

instance Core.AWSRequest CreateWorkteam where
  type
    AWSResponse CreateWorkteam =
      CreateWorkteamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkteamResponse'
            Prelude.<$> (x Core..?> "WorkteamArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkteam where
  hashWithSalt _salt CreateWorkteam' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` notificationConfiguration
      `Prelude.hashWithSalt` workforceName
      `Prelude.hashWithSalt` workteamName
      `Prelude.hashWithSalt` memberDefinitions
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateWorkteam where
  rnf CreateWorkteam' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf workforceName
      `Prelude.seq` Prelude.rnf workteamName
      `Prelude.seq` Prelude.rnf memberDefinitions
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders CreateWorkteam where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateWorkteam" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorkteam where
  toJSON CreateWorkteam' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("NotificationConfiguration" Core..=)
              Prelude.<$> notificationConfiguration,
            ("WorkforceName" Core..=) Prelude.<$> workforceName,
            Prelude.Just ("WorkteamName" Core..= workteamName),
            Prelude.Just
              ("MemberDefinitions" Core..= memberDefinitions),
            Prelude.Just ("Description" Core..= description)
          ]
      )

instance Core.ToPath CreateWorkteam where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateWorkteam where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkteamResponse' smart constructor.
data CreateWorkteamResponse = CreateWorkteamResponse'
  { -- | The Amazon Resource Name (ARN) of the work team. You can use this ARN to
    -- identify the work team.
    workteamArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workteamArn', 'createWorkteamResponse_workteamArn' - The Amazon Resource Name (ARN) of the work team. You can use this ARN to
-- identify the work team.
--
-- 'httpStatus', 'createWorkteamResponse_httpStatus' - The response's http status code.
newCreateWorkteamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkteamResponse
newCreateWorkteamResponse pHttpStatus_ =
  CreateWorkteamResponse'
    { workteamArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the work team. You can use this ARN to
-- identify the work team.
createWorkteamResponse_workteamArn :: Lens.Lens' CreateWorkteamResponse (Prelude.Maybe Prelude.Text)
createWorkteamResponse_workteamArn = Lens.lens (\CreateWorkteamResponse' {workteamArn} -> workteamArn) (\s@CreateWorkteamResponse' {} a -> s {workteamArn = a} :: CreateWorkteamResponse)

-- | The response's http status code.
createWorkteamResponse_httpStatus :: Lens.Lens' CreateWorkteamResponse Prelude.Int
createWorkteamResponse_httpStatus = Lens.lens (\CreateWorkteamResponse' {httpStatus} -> httpStatus) (\s@CreateWorkteamResponse' {} a -> s {httpStatus = a} :: CreateWorkteamResponse)

instance Prelude.NFData CreateWorkteamResponse where
  rnf CreateWorkteamResponse' {..} =
    Prelude.rnf workteamArn
      `Prelude.seq` Prelude.rnf httpStatus
