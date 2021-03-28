{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about the specified Git repository.
module Network.AWS.SageMaker.DescribeCodeRepository
    (
    -- * Creating a request
      DescribeCodeRepository (..)
    , mkDescribeCodeRepository
    -- ** Request lenses
    , dcrCodeRepositoryName

    -- * Destructuring the response
    , DescribeCodeRepositoryResponse (..)
    , mkDescribeCodeRepositoryResponse
    -- ** Response lenses
    , dcrrrsCodeRepositoryName
    , dcrrrsCodeRepositoryArn
    , dcrrrsCreationTime
    , dcrrrsLastModifiedTime
    , dcrrrsGitConfig
    , dcrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeCodeRepository' smart constructor.
newtype DescribeCodeRepository = DescribeCodeRepository'
  { codeRepositoryName :: Types.EntityName
    -- ^ The name of the Git repository to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCodeRepository' value with any optional fields omitted.
mkDescribeCodeRepository
    :: Types.EntityName -- ^ 'codeRepositoryName'
    -> DescribeCodeRepository
mkDescribeCodeRepository codeRepositoryName
  = DescribeCodeRepository'{codeRepositoryName}

-- | The name of the Git repository to describe.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrCodeRepositoryName :: Lens.Lens' DescribeCodeRepository Types.EntityName
dcrCodeRepositoryName = Lens.field @"codeRepositoryName"
{-# INLINEABLE dcrCodeRepositoryName #-}
{-# DEPRECATED codeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead"  #-}

instance Core.ToQuery DescribeCodeRepository where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCodeRepository where
        toHeaders DescribeCodeRepository{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeCodeRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCodeRepository where
        toJSON DescribeCodeRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CodeRepositoryName" Core..= codeRepositoryName)])

instance Core.AWSRequest DescribeCodeRepository where
        type Rs DescribeCodeRepository = DescribeCodeRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCodeRepositoryResponse' Core.<$>
                   (x Core..: "CodeRepositoryName") Core.<*>
                     x Core..: "CodeRepositoryArn"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "LastModifiedTime"
                     Core.<*> x Core..:? "GitConfig"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCodeRepositoryResponse' smart constructor.
data DescribeCodeRepositoryResponse = DescribeCodeRepositoryResponse'
  { codeRepositoryName :: Types.EntityName
    -- ^ The name of the Git repository.
  , codeRepositoryArn :: Types.CodeRepositoryArn
    -- ^ The Amazon Resource Name (ARN) of the Git repository.
  , creationTime :: Core.NominalDiffTime
    -- ^ The date and time that the repository was created.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ The date and time that the repository was last changed.
  , gitConfig :: Core.Maybe Types.GitConfig
    -- ^ Configuration details about the repository, including the URL where the repository is located, the default branch, and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCodeRepositoryResponse' value with any optional fields omitted.
mkDescribeCodeRepositoryResponse
    :: Types.EntityName -- ^ 'codeRepositoryName'
    -> Types.CodeRepositoryArn -- ^ 'codeRepositoryArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeCodeRepositoryResponse
mkDescribeCodeRepositoryResponse codeRepositoryName
  codeRepositoryArn creationTime lastModifiedTime responseStatus
  = DescribeCodeRepositoryResponse'{codeRepositoryName,
                                    codeRepositoryArn, creationTime, lastModifiedTime,
                                    gitConfig = Core.Nothing, responseStatus}

-- | The name of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsCodeRepositoryName :: Lens.Lens' DescribeCodeRepositoryResponse Types.EntityName
dcrrrsCodeRepositoryName = Lens.field @"codeRepositoryName"
{-# INLINEABLE dcrrrsCodeRepositoryName #-}
{-# DEPRECATED codeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsCodeRepositoryArn :: Lens.Lens' DescribeCodeRepositoryResponse Types.CodeRepositoryArn
dcrrrsCodeRepositoryArn = Lens.field @"codeRepositoryArn"
{-# INLINEABLE dcrrrsCodeRepositoryArn #-}
{-# DEPRECATED codeRepositoryArn "Use generic-lens or generic-optics with 'codeRepositoryArn' instead"  #-}

-- | The date and time that the repository was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsCreationTime :: Lens.Lens' DescribeCodeRepositoryResponse Core.NominalDiffTime
dcrrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dcrrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The date and time that the repository was last changed.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsLastModifiedTime :: Lens.Lens' DescribeCodeRepositoryResponse Core.NominalDiffTime
dcrrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dcrrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Configuration details about the repository, including the URL where the repository is located, the default branch, and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsGitConfig :: Lens.Lens' DescribeCodeRepositoryResponse (Core.Maybe Types.GitConfig)
dcrrrsGitConfig = Lens.field @"gitConfig"
{-# INLINEABLE dcrrrsGitConfig #-}
{-# DEPRECATED gitConfig "Use generic-lens or generic-optics with 'gitConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsResponseStatus :: Lens.Lens' DescribeCodeRepositoryResponse Core.Int
dcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
