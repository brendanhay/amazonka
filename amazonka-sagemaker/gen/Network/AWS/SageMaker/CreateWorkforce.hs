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
-- Module      : Network.AWS.SageMaker.CreateWorkforce
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to create a workforce. This operation will return an
-- error if a workforce already exists in the AWS Region that you specify.
-- You can only create one workforce in each AWS Region per AWS account.
--
-- If you want to create a new workforce in an AWS Region where a workforce
-- already exists, use the API operation to delete the existing workforce
-- and then use @CreateWorkforce@ to create a new workforce.
--
-- To create a private workforce using Amazon Cognito, you must specify a
-- Cognito user pool in @CognitoConfig@. You can also create an Amazon
-- Cognito workforce using the Amazon SageMaker console. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce (Amazon Cognito)>.
--
-- To create a private workforce using your own OIDC Identity Provider
-- (IdP), specify your IdP configuration in @OidcConfig@. Your OIDC IdP
-- must support /groups/ because groups are used by Ground Truth and Amazon
-- A2I to create work teams. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private-oidc.html Create a Private Workforce (OIDC IdP)>.
module Network.AWS.SageMaker.CreateWorkforce
  ( -- * Creating a Request
    CreateWorkforce (..),
    newCreateWorkforce,

    -- * Request Lenses
    createWorkforce_tags,
    createWorkforce_sourceIpConfig,
    createWorkforce_oidcConfig,
    createWorkforce_cognitoConfig,
    createWorkforce_workforceName,

    -- * Destructuring the Response
    CreateWorkforceResponse (..),
    newCreateWorkforceResponse,

    -- * Response Lenses
    createWorkforceResponse_httpStatus,
    createWorkforceResponse_workforceArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateWorkforce' smart constructor.
data CreateWorkforce = CreateWorkforce'
  { -- | An array of key-value pairs that contain metadata to help you categorize
    -- and organize our workforce. Each tag consists of a key and a value, both
    -- of which you define.
    tags :: Core.Maybe [Tag],
    sourceIpConfig :: Core.Maybe SourceIpConfig,
    -- | Use this parameter to configure a private workforce using your own OIDC
    -- Identity Provider.
    --
    -- Do not use @CognitoConfig@ if you specify values for @OidcConfig@.
    oidcConfig :: Core.Maybe OidcConfig,
    -- | Use this parameter to configure an Amazon Cognito private workforce. A
    -- single Cognito workforce is created using and corresponds to a single
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
    --
    -- Do not use @OidcConfig@ if you specify values for @CognitoConfig@.
    cognitoConfig :: Core.Maybe CognitoConfig,
    -- | The name of the private workforce.
    workforceName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateWorkforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorkforce_tags' - An array of key-value pairs that contain metadata to help you categorize
-- and organize our workforce. Each tag consists of a key and a value, both
-- of which you define.
--
-- 'sourceIpConfig', 'createWorkforce_sourceIpConfig' - Undocumented member.
--
-- 'oidcConfig', 'createWorkforce_oidcConfig' - Use this parameter to configure a private workforce using your own OIDC
-- Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@.
--
-- 'cognitoConfig', 'createWorkforce_cognitoConfig' - Use this parameter to configure an Amazon Cognito private workforce. A
-- single Cognito workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@.
--
-- 'workforceName', 'createWorkforce_workforceName' - The name of the private workforce.
newCreateWorkforce ::
  -- | 'workforceName'
  Core.Text ->
  CreateWorkforce
newCreateWorkforce pWorkforceName_ =
  CreateWorkforce'
    { tags = Core.Nothing,
      sourceIpConfig = Core.Nothing,
      oidcConfig = Core.Nothing,
      cognitoConfig = Core.Nothing,
      workforceName = pWorkforceName_
    }

-- | An array of key-value pairs that contain metadata to help you categorize
-- and organize our workforce. Each tag consists of a key and a value, both
-- of which you define.
createWorkforce_tags :: Lens.Lens' CreateWorkforce (Core.Maybe [Tag])
createWorkforce_tags = Lens.lens (\CreateWorkforce' {tags} -> tags) (\s@CreateWorkforce' {} a -> s {tags = a} :: CreateWorkforce) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createWorkforce_sourceIpConfig :: Lens.Lens' CreateWorkforce (Core.Maybe SourceIpConfig)
createWorkforce_sourceIpConfig = Lens.lens (\CreateWorkforce' {sourceIpConfig} -> sourceIpConfig) (\s@CreateWorkforce' {} a -> s {sourceIpConfig = a} :: CreateWorkforce)

-- | Use this parameter to configure a private workforce using your own OIDC
-- Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@.
createWorkforce_oidcConfig :: Lens.Lens' CreateWorkforce (Core.Maybe OidcConfig)
createWorkforce_oidcConfig = Lens.lens (\CreateWorkforce' {oidcConfig} -> oidcConfig) (\s@CreateWorkforce' {} a -> s {oidcConfig = a} :: CreateWorkforce)

-- | Use this parameter to configure an Amazon Cognito private workforce. A
-- single Cognito workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@.
createWorkforce_cognitoConfig :: Lens.Lens' CreateWorkforce (Core.Maybe CognitoConfig)
createWorkforce_cognitoConfig = Lens.lens (\CreateWorkforce' {cognitoConfig} -> cognitoConfig) (\s@CreateWorkforce' {} a -> s {cognitoConfig = a} :: CreateWorkforce)

-- | The name of the private workforce.
createWorkforce_workforceName :: Lens.Lens' CreateWorkforce Core.Text
createWorkforce_workforceName = Lens.lens (\CreateWorkforce' {workforceName} -> workforceName) (\s@CreateWorkforce' {} a -> s {workforceName = a} :: CreateWorkforce)

instance Core.AWSRequest CreateWorkforce where
  type
    AWSResponse CreateWorkforce =
      CreateWorkforceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkforceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "WorkforceArn")
      )

instance Core.Hashable CreateWorkforce

instance Core.NFData CreateWorkforce

instance Core.ToHeaders CreateWorkforce where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateWorkforce" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateWorkforce where
  toJSON CreateWorkforce' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("SourceIpConfig" Core..=) Core.<$> sourceIpConfig,
            ("OidcConfig" Core..=) Core.<$> oidcConfig,
            ("CognitoConfig" Core..=) Core.<$> cognitoConfig,
            Core.Just ("WorkforceName" Core..= workforceName)
          ]
      )

instance Core.ToPath CreateWorkforce where
  toPath = Core.const "/"

instance Core.ToQuery CreateWorkforce where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateWorkforceResponse' smart constructor.
data CreateWorkforceResponse = CreateWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the workforce.
    workforceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateWorkforceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkforceResponse_httpStatus' - The response's http status code.
--
-- 'workforceArn', 'createWorkforceResponse_workforceArn' - The Amazon Resource Name (ARN) of the workforce.
newCreateWorkforceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'workforceArn'
  Core.Text ->
  CreateWorkforceResponse
newCreateWorkforceResponse
  pHttpStatus_
  pWorkforceArn_ =
    CreateWorkforceResponse'
      { httpStatus = pHttpStatus_,
        workforceArn = pWorkforceArn_
      }

-- | The response's http status code.
createWorkforceResponse_httpStatus :: Lens.Lens' CreateWorkforceResponse Core.Int
createWorkforceResponse_httpStatus = Lens.lens (\CreateWorkforceResponse' {httpStatus} -> httpStatus) (\s@CreateWorkforceResponse' {} a -> s {httpStatus = a} :: CreateWorkforceResponse)

-- | The Amazon Resource Name (ARN) of the workforce.
createWorkforceResponse_workforceArn :: Lens.Lens' CreateWorkforceResponse Core.Text
createWorkforceResponse_workforceArn = Lens.lens (\CreateWorkforceResponse' {workforceArn} -> workforceArn) (\s@CreateWorkforceResponse' {} a -> s {workforceArn = a} :: CreateWorkforceResponse)

instance Core.NFData CreateWorkforceResponse
