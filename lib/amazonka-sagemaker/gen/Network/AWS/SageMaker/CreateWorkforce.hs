{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to create a workforce. This operation will return an error if a workforce already exists in the AWS Region that you specify. You can only create one workforce in each AWS Region per AWS account.
--
-- If you want to create a new workforce in an AWS Region where a workforce already exists, use the API operation to delete the existing workforce and then use @CreateWorkforce@ to create a new workforce.
-- To create a private workforce using Amazon Cognito, you must specify a Cognito user pool in @CognitoConfig@ . You can also create an Amazon Cognito workforce using the Amazon SageMaker console. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce (Amazon Cognito)> .
-- To create a private workforce using your own OIDC Identity Provider (IdP), specify your IdP configuration in @OidcConfig@ . Your OIDC IdP must support /groups/ because groups are used by Ground Truth and Amazon A2I to create work teams. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private-oidc.html Create a Private Workforce (OIDC IdP)> .
module Network.AWS.SageMaker.CreateWorkforce
  ( -- * Creating a request
    CreateWorkforce (..),
    mkCreateWorkforce,

    -- ** Request lenses
    cSourceIPConfig,
    cCognitoConfig,
    cOidcConfig,
    cWorkforceName,
    cTags,

    -- * Destructuring the response
    CreateWorkforceResponse (..),
    mkCreateWorkforceResponse,

    -- ** Response lenses
    cwrsWorkforceARN,
    cwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateWorkforce' smart constructor.
data CreateWorkforce = CreateWorkforce'
  { sourceIPConfig :: Lude.Maybe SourceIPConfig,
    -- | Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
    --
    -- Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
    cognitoConfig :: Lude.Maybe CognitoConfig,
    -- | Use this parameter to configure a private workforce using your own OIDC Identity Provider.
    --
    -- Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
    oidcConfig :: Lude.Maybe OidcConfig,
    -- | The name of the private workforce.
    workforceName :: Lude.Text,
    -- | An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkforce' with the minimum fields required to make a request.
--
-- * 'sourceIPConfig' -
-- * 'cognitoConfig' - Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
-- * 'oidcConfig' - Use this parameter to configure a private workforce using your own OIDC Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
-- * 'workforceName' - The name of the private workforce.
-- * 'tags' - An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
mkCreateWorkforce ::
  -- | 'workforceName'
  Lude.Text ->
  CreateWorkforce
mkCreateWorkforce pWorkforceName_ =
  CreateWorkforce'
    { sourceIPConfig = Lude.Nothing,
      cognitoConfig = Lude.Nothing,
      oidcConfig = Lude.Nothing,
      workforceName = pWorkforceName_,
      tags = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceIPConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceIPConfig :: Lens.Lens' CreateWorkforce (Lude.Maybe SourceIPConfig)
cSourceIPConfig = Lens.lens (sourceIPConfig :: CreateWorkforce -> Lude.Maybe SourceIPConfig) (\s a -> s {sourceIPConfig = a} :: CreateWorkforce)
{-# DEPRECATED cSourceIPConfig "Use generic-lens or generic-optics with 'sourceIPConfig' instead." #-}

-- | Use this parameter to configure an Amazon Cognito private workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- Do not use @OidcConfig@ if you specify values for @CognitoConfig@ .
--
-- /Note:/ Consider using 'cognitoConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCognitoConfig :: Lens.Lens' CreateWorkforce (Lude.Maybe CognitoConfig)
cCognitoConfig = Lens.lens (cognitoConfig :: CreateWorkforce -> Lude.Maybe CognitoConfig) (\s a -> s {cognitoConfig = a} :: CreateWorkforce)
{-# DEPRECATED cCognitoConfig "Use generic-lens or generic-optics with 'cognitoConfig' instead." #-}

-- | Use this parameter to configure a private workforce using your own OIDC Identity Provider.
--
-- Do not use @CognitoConfig@ if you specify values for @OidcConfig@ .
--
-- /Note:/ Consider using 'oidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOidcConfig :: Lens.Lens' CreateWorkforce (Lude.Maybe OidcConfig)
cOidcConfig = Lens.lens (oidcConfig :: CreateWorkforce -> Lude.Maybe OidcConfig) (\s a -> s {oidcConfig = a} :: CreateWorkforce)
{-# DEPRECATED cOidcConfig "Use generic-lens or generic-optics with 'oidcConfig' instead." #-}

-- | The name of the private workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWorkforceName :: Lens.Lens' CreateWorkforce Lude.Text
cWorkforceName = Lens.lens (workforceName :: CreateWorkforce -> Lude.Text) (\s a -> s {workforceName = a} :: CreateWorkforce)
{-# DEPRECATED cWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

-- | An array of key-value pairs that contain metadata to help you categorize and organize our workforce. Each tag consists of a key and a value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateWorkforce (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: CreateWorkforce -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateWorkforce)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateWorkforce where
  type Rs CreateWorkforce = CreateWorkforceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWorkforceResponse'
            Lude.<$> (x Lude..:> "WorkforceArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWorkforce where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateWorkforce" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWorkforce where
  toJSON CreateWorkforce' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceIpConfig" Lude..=) Lude.<$> sourceIPConfig,
            ("CognitoConfig" Lude..=) Lude.<$> cognitoConfig,
            ("OidcConfig" Lude..=) Lude.<$> oidcConfig,
            Lude.Just ("WorkforceName" Lude..= workforceName),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateWorkforce where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWorkforce where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWorkforceResponse' smart constructor.
data CreateWorkforceResponse = CreateWorkforceResponse'
  { -- | The Amazon Resource Name (ARN) of the workforce.
    workforceARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkforceResponse' with the minimum fields required to make a request.
--
-- * 'workforceARN' - The Amazon Resource Name (ARN) of the workforce.
-- * 'responseStatus' - The response status code.
mkCreateWorkforceResponse ::
  -- | 'workforceARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateWorkforceResponse
mkCreateWorkforceResponse pWorkforceARN_ pResponseStatus_ =
  CreateWorkforceResponse'
    { workforceARN = pWorkforceARN_,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the workforce.
--
-- /Note:/ Consider using 'workforceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsWorkforceARN :: Lens.Lens' CreateWorkforceResponse Lude.Text
cwrsWorkforceARN = Lens.lens (workforceARN :: CreateWorkforceResponse -> Lude.Text) (\s a -> s {workforceARN = a} :: CreateWorkforceResponse)
{-# DEPRECATED cwrsWorkforceARN "Use generic-lens or generic-optics with 'workforceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsResponseStatus :: Lens.Lens' CreateWorkforceResponse Lude.Int
cwrsResponseStatus = Lens.lens (responseStatus :: CreateWorkforceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWorkforceResponse)
{-# DEPRECATED cwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
