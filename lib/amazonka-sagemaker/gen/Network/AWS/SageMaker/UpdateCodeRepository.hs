{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Git repository with the specified values.
module Network.AWS.SageMaker.UpdateCodeRepository
  ( -- * Creating a request
    UpdateCodeRepository (..),
    mkUpdateCodeRepository,

    -- ** Request lenses
    ucrCodeRepositoryName,
    ucrGitConfig,

    -- * Destructuring the response
    UpdateCodeRepositoryResponse (..),
    mkUpdateCodeRepositoryResponse,

    -- ** Response lenses
    ucrrrsCodeRepositoryArn,
    ucrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateCodeRepository' smart constructor.
data UpdateCodeRepository = UpdateCodeRepository'
  { -- | The name of the Git repository to update.
    codeRepositoryName :: Types.EntityName,
    -- | The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
    --
    -- @{"username": /UserName/ , "password": /Password/ }@
    gitConfig :: Core.Maybe Types.GitConfigForUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCodeRepository' value with any optional fields omitted.
mkUpdateCodeRepository ::
  -- | 'codeRepositoryName'
  Types.EntityName ->
  UpdateCodeRepository
mkUpdateCodeRepository codeRepositoryName =
  UpdateCodeRepository'
    { codeRepositoryName,
      gitConfig = Core.Nothing
    }

-- | The name of the Git repository to update.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrCodeRepositoryName :: Lens.Lens' UpdateCodeRepository Types.EntityName
ucrCodeRepositoryName = Lens.field @"codeRepositoryName"
{-# DEPRECATED ucrCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

-- | The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrGitConfig :: Lens.Lens' UpdateCodeRepository (Core.Maybe Types.GitConfigForUpdate)
ucrGitConfig = Lens.field @"gitConfig"
{-# DEPRECATED ucrGitConfig "Use generic-lens or generic-optics with 'gitConfig' instead." #-}

instance Core.FromJSON UpdateCodeRepository where
  toJSON UpdateCodeRepository {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CodeRepositoryName" Core..= codeRepositoryName),
            ("GitConfig" Core..=) Core.<$> gitConfig
          ]
      )

instance Core.AWSRequest UpdateCodeRepository where
  type Rs UpdateCodeRepository = UpdateCodeRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateCodeRepository")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCodeRepositoryResponse'
            Core.<$> (x Core..: "CodeRepositoryArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCodeRepositoryResponse' smart constructor.
data UpdateCodeRepositoryResponse = UpdateCodeRepositoryResponse'
  { -- | The ARN of the Git repository.
    codeRepositoryArn :: Types.CodeRepositoryArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCodeRepositoryResponse' value with any optional fields omitted.
mkUpdateCodeRepositoryResponse ::
  -- | 'codeRepositoryArn'
  Types.CodeRepositoryArn ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateCodeRepositoryResponse
mkUpdateCodeRepositoryResponse codeRepositoryArn responseStatus =
  UpdateCodeRepositoryResponse' {codeRepositoryArn, responseStatus}

-- | The ARN of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrrsCodeRepositoryArn :: Lens.Lens' UpdateCodeRepositoryResponse Types.CodeRepositoryArn
ucrrrsCodeRepositoryArn = Lens.field @"codeRepositoryArn"
{-# DEPRECATED ucrrrsCodeRepositoryArn "Use generic-lens or generic-optics with 'codeRepositoryArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrrsResponseStatus :: Lens.Lens' UpdateCodeRepositoryResponse Core.Int
ucrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
