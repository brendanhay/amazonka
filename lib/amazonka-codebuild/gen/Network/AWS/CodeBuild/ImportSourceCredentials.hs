{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ImportSourceCredentials (..)
    , mkImportSourceCredentials
    -- ** Request lenses
    , iscToken
    , iscServerType
    , iscAuthType
    , iscShouldOverwrite
    , iscUsername

    -- * Destructuring the response
    , ImportSourceCredentialsResponse (..)
    , mkImportSourceCredentialsResponse
    -- ** Response lenses
    , iscrrsArn
    , iscrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportSourceCredentials' smart constructor.
data ImportSourceCredentials = ImportSourceCredentials'
  { token :: Types.Token
    -- ^ For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password. 
  , serverType :: Types.ServerType
    -- ^ The source provider used for this project. 
  , authType :: Types.AuthType
    -- ^ The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console. 
  , shouldOverwrite :: Core.Maybe Core.Bool
    -- ^ Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ . 
  , username :: Core.Maybe Types.Username
    -- ^ The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportSourceCredentials' value with any optional fields omitted.
mkImportSourceCredentials
    :: Types.Token -- ^ 'token'
    -> Types.ServerType -- ^ 'serverType'
    -> Types.AuthType -- ^ 'authType'
    -> ImportSourceCredentials
mkImportSourceCredentials token serverType authType
  = ImportSourceCredentials'{token, serverType, authType,
                             shouldOverwrite = Core.Nothing, username = Core.Nothing}

-- | For GitHub or GitHub Enterprise, this is the personal access token. For Bitbucket, this is the app password. 
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscToken :: Lens.Lens' ImportSourceCredentials Types.Token
iscToken = Lens.field @"token"
{-# INLINEABLE iscToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

-- | The source provider used for this project. 
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscServerType :: Lens.Lens' ImportSourceCredentials Types.ServerType
iscServerType = Lens.field @"serverType"
{-# INLINEABLE iscServerType #-}
{-# DEPRECATED serverType "Use generic-lens or generic-optics with 'serverType' instead"  #-}

-- | The type of authentication used to connect to a GitHub, GitHub Enterprise, or Bitbucket repository. An OAUTH connection is not supported by the API and must be created using the AWS CodeBuild console. 
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscAuthType :: Lens.Lens' ImportSourceCredentials Types.AuthType
iscAuthType = Lens.field @"authType"
{-# INLINEABLE iscAuthType #-}
{-# DEPRECATED authType "Use generic-lens or generic-optics with 'authType' instead"  #-}

-- | Set to @false@ to prevent overwriting the repository source credentials. Set to @true@ to overwrite the repository source credentials. The default value is @true@ . 
--
-- /Note:/ Consider using 'shouldOverwrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscShouldOverwrite :: Lens.Lens' ImportSourceCredentials (Core.Maybe Core.Bool)
iscShouldOverwrite = Lens.field @"shouldOverwrite"
{-# INLINEABLE iscShouldOverwrite #-}
{-# DEPRECATED shouldOverwrite "Use generic-lens or generic-optics with 'shouldOverwrite' instead"  #-}

-- | The Bitbucket username when the @authType@ is BASIC_AUTH. This parameter is not valid for other types of source providers or connections. 
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscUsername :: Lens.Lens' ImportSourceCredentials (Core.Maybe Types.Username)
iscUsername = Lens.field @"username"
{-# INLINEABLE iscUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery ImportSourceCredentials where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportSourceCredentials where
        toHeaders ImportSourceCredentials{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.ImportSourceCredentials")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportSourceCredentials where
        toJSON ImportSourceCredentials{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("token" Core..= token),
                  Core.Just ("serverType" Core..= serverType),
                  Core.Just ("authType" Core..= authType),
                  ("shouldOverwrite" Core..=) Core.<$> shouldOverwrite,
                  ("username" Core..=) Core.<$> username])

instance Core.AWSRequest ImportSourceCredentials where
        type Rs ImportSourceCredentials = ImportSourceCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportSourceCredentialsResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportSourceCredentialsResponse' smart constructor.
data ImportSourceCredentialsResponse = ImportSourceCredentialsResponse'
  { arn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the token. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportSourceCredentialsResponse' value with any optional fields omitted.
mkImportSourceCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportSourceCredentialsResponse
mkImportSourceCredentialsResponse responseStatus
  = ImportSourceCredentialsResponse'{arn = Core.Nothing,
                                     responseStatus}

-- | The Amazon Resource Name (ARN) of the token. 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrrsArn :: Lens.Lens' ImportSourceCredentialsResponse (Core.Maybe Types.NonEmptyString)
iscrrsArn = Lens.field @"arn"
{-# INLINEABLE iscrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrrsResponseStatus :: Lens.Lens' ImportSourceCredentialsResponse Core.Int
iscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
