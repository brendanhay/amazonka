{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ImportSourceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the source repository credentials for an AWS CodeBuild project that has its source code stored in a GitHub, GitHub Enterprise, or Bitbucket repository.
module Network.AWS.CodeBuild.ImportSourceCredentials
  ( -- * Creating a request
    ImportSourceCredentials (..),
    mkImportSourceCredentials,

    -- ** Request lenses
    iscServerType,
    iscToken,
    iscUsername,
    iscShouldOverwrite,
    iscAuthType,

    -- * Destructuring the response
    ImportSourceCredentialsResponse (..),
    mkImportSourceCredentialsResponse,

    -- ** Response lenses
    iscrsArn,
    iscrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportSourceCredentials' smart constructor.
data ImportSourceCredentials = ImportSourceCredentials'
  { -- | The source provider used for this project.
    serverType :: ServerType,
    -- | For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password.
    token :: Lude.Sensitive Lude.Text,
    -- | The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections.
    username :: Lude.Maybe Lude.Text,
    -- | Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ .
    shouldOverwrite :: Lude.Maybe Lude.Bool,
    -- | The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console.
    authType :: AuthType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportSourceCredentials' with the minimum fields required to make a request.
--
-- * 'serverType' - The source provider used for this project.
-- * 'token' - For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password.
-- * 'username' - The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections.
-- * 'shouldOverwrite' - Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ .
-- * 'authType' - The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console.
mkImportSourceCredentials ::
  -- | 'serverType'
  ServerType ->
  -- | 'token'
  Lude.Sensitive Lude.Text ->
  -- | 'authType'
  AuthType ->
  ImportSourceCredentials
mkImportSourceCredentials pServerType_ pToken_ pAuthType_ =
  ImportSourceCredentials'
    { serverType = pServerType_,
      token = pToken_,
      username = Lude.Nothing,
      shouldOverwrite = Lude.Nothing,
      authType = pAuthType_
    }

-- | The source provider used for this project.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscServerType :: Lens.Lens' ImportSourceCredentials ServerType
iscServerType = Lens.lens (serverType :: ImportSourceCredentials -> ServerType) (\s a -> s {serverType = a} :: ImportSourceCredentials)
{-# DEPRECATED iscServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

-- | For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscToken :: Lens.Lens' ImportSourceCredentials (Lude.Sensitive Lude.Text)
iscToken = Lens.lens (token :: ImportSourceCredentials -> Lude.Sensitive Lude.Text) (\s a -> s {token = a} :: ImportSourceCredentials)
{-# DEPRECATED iscToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscUsername :: Lens.Lens' ImportSourceCredentials (Lude.Maybe Lude.Text)
iscUsername = Lens.lens (username :: ImportSourceCredentials -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: ImportSourceCredentials)
{-# DEPRECATED iscUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ .
--
-- /Note:/ Consider using 'shouldOverwrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscShouldOverwrite :: Lens.Lens' ImportSourceCredentials (Lude.Maybe Lude.Bool)
iscShouldOverwrite = Lens.lens (shouldOverwrite :: ImportSourceCredentials -> Lude.Maybe Lude.Bool) (\s a -> s {shouldOverwrite = a} :: ImportSourceCredentials)
{-# DEPRECATED iscShouldOverwrite "Use generic-lens or generic-optics with 'shouldOverwrite' instead." #-}

-- | The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscAuthType :: Lens.Lens' ImportSourceCredentials AuthType
iscAuthType = Lens.lens (authType :: ImportSourceCredentials -> AuthType) (\s a -> s {authType = a} :: ImportSourceCredentials)
{-# DEPRECATED iscAuthType "Use generic-lens or generic-optics with 'authType' instead." #-}

instance Lude.AWSRequest ImportSourceCredentials where
  type Rs ImportSourceCredentials = ImportSourceCredentialsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportSourceCredentialsResponse'
            Lude.<$> (x Lude..?> "arn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportSourceCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ImportSourceCredentials" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportSourceCredentials where
  toJSON ImportSourceCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("serverType" Lude..= serverType),
            Lude.Just ("token" Lude..= token),
            ("username" Lude..=) Lude.<$> username,
            ("shouldOverwrite" Lude..=) Lude.<$> shouldOverwrite,
            Lude.Just ("authType" Lude..= authType)
          ]
      )

instance Lude.ToPath ImportSourceCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportSourceCredentials where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportSourceCredentialsResponse' smart constructor.
data ImportSourceCredentialsResponse = ImportSourceCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportSourceCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the token.
-- * 'responseStatus' - The response status code.
mkImportSourceCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportSourceCredentialsResponse
mkImportSourceCredentialsResponse pResponseStatus_ =
  ImportSourceCredentialsResponse'
    { arn = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrsArn :: Lens.Lens' ImportSourceCredentialsResponse (Lude.Maybe Lude.Text)
iscrsArn = Lens.lens (arn :: ImportSourceCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ImportSourceCredentialsResponse)
{-# DEPRECATED iscrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrsResponseStatus :: Lens.Lens' ImportSourceCredentialsResponse Lude.Int
iscrsResponseStatus = Lens.lens (responseStatus :: ImportSourceCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportSourceCredentialsResponse)
{-# DEPRECATED iscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
