{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateWorkforce' smart constructor.
data CreateWorkforce = CreateWorkforce'
  { -- | An array of key-value pairs that contain metadata to help you categorize
    -- and organize our workforce. Each tag consists of a key and a value, both
    -- of which you define.
    tags :: Prelude.Maybe [Tag],
    sourceIpConfig :: Prelude.Maybe SourceIpConfig,
    -- | Use this parameter to configure a private workforce using your own OIDC
    -- Identity Provider.
    --
    -- Do not use @CognitoConfig@ if you specify values for @OidcConfig@.
    oidcConfig :: Prelude.Maybe OidcConfig,
    -- | Use this parameter to configure an Amazon Cognito private workforce. A
    -- single Cognito workforce is created using and corresponds to a single
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
    --
    -- Do not use @OidcConfig@ if you specify values for @CognitoConfig@.
    cognitoConfig :: Prelude.Maybe CognitoConfig,
    -- | The name of the private workforce.
    workforceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateWorkforce
newCreateWorkforce pWorkforceName_ =
  CreateWorkforce'
    { tags = Prelude.Nothing,
      sourceIpConfig = Prelude.Nothing,
      oidcConfig = Prelude.Nothing,
      cognitoConfig = Prelude.Nothing,
      workforceName = pWorkforceName_
    }

-- | An array of key-value pairs that contain metadata to help you categorize
-- and organize our workforce. Each tag consists of a key and a value, both
-- of which you define.
createWorkforce_tags :: Lens.Lens' CreateWorkforce (Prelude.Maybe [Tag])
createWorkforce_tags = Lens.lens (\CreateWorkforce' {tags} -> tags) (\s@CreateWorkforce' {} a -> s {tags = a} :: CreateWorkforce) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
createWorkforce_sourceIpConfig :: Lens.Lens' CreateWorkforce (Prelude.Maybe SourceIpConfig)
createWorkforce_sourceIpConfig = Lens.lens (\CreateWorkforce' {sourceIpConfig} -> sourceIpConfig) (\s@CreateWorkforce' {} a -> s {sourceIpConfig = a} :: CreateWorkforce)

-- | Use this parameter to configure a private workforce using your own OIDC
-- Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@.
createWorkforce_oidcConfig :: Lens.Lens' CreateWorkforce (Prelude.Maybe OidcConfig)
createWorkforce_oidcConfig = Lens.lens (\CreateWorkforce' {oidcConfig} -> oidcConfig) (\s@CreateWorkforce' {} a -> s {oidcConfig = a} :: CreateWorkforce)

-- | Use this parameter to configure an Amazon Cognito private workforce. A
-- single Cognito workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@.
createWorkforce_cognitoConfig :: Lens.Lens' CreateWorkforce (Prelude.Maybe CognitoConfig)
createWorkforce_cognitoConfig = Lens.lens (\CreateWorkforce' {cognitoConfig} -> cognitoConfig) (\s@CreateWorkforce' {} a -> s {cognitoConfig = a} :: CreateWorkforce)

-- | The name of the private workforce.
createWorkforce_workforceName :: Lens.Lens' CreateWorkforce Prelude.Text
createWorkforce_workforceName = Lens.lens (\CreateWorkforce' {workforceName} -> workforceName) (\s@CreateWorkforce' {} a -> s {workforceName = a} :: CreateWorkforce)

instance Prelude.AWSRequest CreateWorkforce where
  type Rs CreateWorkforce = CreateWorkforceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkforceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "WorkforceArn")
      )

instance Prelude.Hashable CreateWorkforce

instance Prelude.NFData CreateWorkforce

instance Prelude.ToHeaders CreateWorkforce where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreateWorkforce" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateWorkforce where
  toJSON CreateWorkforce' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("SourceIpConfig" Prelude..=)
              Prelude.<$> sourceIpConfig,
            ("OidcConfig" Prelude..=) Prelude.<$> oidcConfig,
            ("CognitoConfig" Prelude..=)
              Prelude.<$> cognitoConfig,
            Prelude.Just
              ("WorkforceName" Prelude..= workforceName)
          ]
      )

instance Prelude.ToPath CreateWorkforce where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateWorkforce where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkforceResponse' smart constructor.
data CreateWorkforceResponse = CreateWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the workforce.
    workforceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'workforceArn'
  Prelude.Text ->
  CreateWorkforceResponse
newCreateWorkforceResponse
  pHttpStatus_
  pWorkforceArn_ =
    CreateWorkforceResponse'
      { httpStatus = pHttpStatus_,
        workforceArn = pWorkforceArn_
      }

-- | The response's http status code.
createWorkforceResponse_httpStatus :: Lens.Lens' CreateWorkforceResponse Prelude.Int
createWorkforceResponse_httpStatus = Lens.lens (\CreateWorkforceResponse' {httpStatus} -> httpStatus) (\s@CreateWorkforceResponse' {} a -> s {httpStatus = a} :: CreateWorkforceResponse)

-- | The Amazon Resource Name (ARN) of the workforce.
createWorkforceResponse_workforceArn :: Lens.Lens' CreateWorkforceResponse Prelude.Text
createWorkforceResponse_workforceArn = Lens.lens (\CreateWorkforceResponse' {workforceArn} -> workforceArn) (\s@CreateWorkforceResponse' {} a -> s {workforceArn = a} :: CreateWorkforceResponse)

instance Prelude.NFData CreateWorkforceResponse
