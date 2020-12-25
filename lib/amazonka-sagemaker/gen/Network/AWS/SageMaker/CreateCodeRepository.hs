{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Git repository as a resource in your Amazon SageMaker account. You can associate the repository with notebook instances so that you can use Git source control for the notebooks you create. The Git repository is a resource in your Amazon SageMaker account, so it can be associated with more than one notebook instance, and it persists independently from the lifecycle of any notebook instances it is associated with.
--
-- The repository can be hosted either in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository.
module Network.AWS.SageMaker.CreateCodeRepository
  ( -- * Creating a request
    CreateCodeRepository (..),
    mkCreateCodeRepository,

    -- ** Request lenses
    ccrCodeRepositoryName,
    ccrGitConfig,

    -- * Destructuring the response
    CreateCodeRepositoryResponse (..),
    mkCreateCodeRepositoryResponse,

    -- ** Response lenses
    ccrrrsCodeRepositoryArn,
    ccrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateCodeRepository' smart constructor.
data CreateCodeRepository = CreateCodeRepository'
  { -- | The name of the Git repository. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    codeRepositoryName :: Types.EntityName,
    -- | Specifies details about the repository, including the URL where the repository is located, the default branch, and credentials to use to access the repository.
    gitConfig :: Types.GitConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCodeRepository' value with any optional fields omitted.
mkCreateCodeRepository ::
  -- | 'codeRepositoryName'
  Types.EntityName ->
  -- | 'gitConfig'
  Types.GitConfig ->
  CreateCodeRepository
mkCreateCodeRepository codeRepositoryName gitConfig =
  CreateCodeRepository' {codeRepositoryName, gitConfig}

-- | The name of the Git repository. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrCodeRepositoryName :: Lens.Lens' CreateCodeRepository Types.EntityName
ccrCodeRepositoryName = Lens.field @"codeRepositoryName"
{-# DEPRECATED ccrCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

-- | Specifies details about the repository, including the URL where the repository is located, the default branch, and credentials to use to access the repository.
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrGitConfig :: Lens.Lens' CreateCodeRepository Types.GitConfig
ccrGitConfig = Lens.field @"gitConfig"
{-# DEPRECATED ccrGitConfig "Use generic-lens or generic-optics with 'gitConfig' instead." #-}

instance Core.FromJSON CreateCodeRepository where
  toJSON CreateCodeRepository {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CodeRepositoryName" Core..= codeRepositoryName),
            Core.Just ("GitConfig" Core..= gitConfig)
          ]
      )

instance Core.AWSRequest CreateCodeRepository where
  type Rs CreateCodeRepository = CreateCodeRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateCodeRepository")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCodeRepositoryResponse'
            Core.<$> (x Core..: "CodeRepositoryArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCodeRepositoryResponse' smart constructor.
data CreateCodeRepositoryResponse = CreateCodeRepositoryResponse'
  { -- | The Amazon Resource Name (ARN) of the new repository.
    codeRepositoryArn :: Types.CodeRepositoryArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCodeRepositoryResponse' value with any optional fields omitted.
mkCreateCodeRepositoryResponse ::
  -- | 'codeRepositoryArn'
  Types.CodeRepositoryArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateCodeRepositoryResponse
mkCreateCodeRepositoryResponse codeRepositoryArn responseStatus =
  CreateCodeRepositoryResponse' {codeRepositoryArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the new repository.
--
-- /Note:/ Consider using 'codeRepositoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrrsCodeRepositoryArn :: Lens.Lens' CreateCodeRepositoryResponse Types.CodeRepositoryArn
ccrrrsCodeRepositoryArn = Lens.field @"codeRepositoryArn"
{-# DEPRECATED ccrrrsCodeRepositoryArn "Use generic-lens or generic-optics with 'codeRepositoryArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrrsResponseStatus :: Lens.Lens' CreateCodeRepositoryResponse Core.Int
ccrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
