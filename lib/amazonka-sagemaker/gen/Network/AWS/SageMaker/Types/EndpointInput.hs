{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.EndpointInput
  ( EndpointInput (..)
  -- * Smart constructor
  , mkEndpointInput
  -- * Lenses
  , eiEndpointName
  , eiLocalPath
  , eiS3DataDistributionType
  , eiS3InputMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EndpointName as Types
import qualified Network.AWS.SageMaker.Types.ProcessingLocalPath as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3InputMode as Types

-- | Input object for the endpoint
--
-- /See:/ 'mkEndpointInput' smart constructor.
data EndpointInput = EndpointInput'
  { endpointName :: Types.EndpointName
    -- ^ An endpoint in customer's account which has enabled @DataCaptureConfig@ enabled.
  , localPath :: Types.ProcessingLocalPath
    -- ^ Path to the filesystem where the endpoint data is available to the container.
  , s3DataDistributionType :: Core.Maybe Types.ProcessingS3DataDistributionType
    -- ^ Whether input data distributed in Amazon S3 is fully replicated or sharded by an S3 key. Defauts to @FullyReplicated@ 
  , s3InputMode :: Core.Maybe Types.ProcessingS3InputMode
    -- ^ Whether the @Pipe@ or @File@ is used as the input mode for transfering data for the monitoring job. @Pipe@ mode is recommended for large datasets. @File@ mode is useful for small files that fit in memory. Defaults to @File@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointInput' value with any optional fields omitted.
mkEndpointInput
    :: Types.EndpointName -- ^ 'endpointName'
    -> Types.ProcessingLocalPath -- ^ 'localPath'
    -> EndpointInput
mkEndpointInput endpointName localPath
  = EndpointInput'{endpointName, localPath,
                   s3DataDistributionType = Core.Nothing, s3InputMode = Core.Nothing}

-- | An endpoint in customer's account which has enabled @DataCaptureConfig@ enabled.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEndpointName :: Lens.Lens' EndpointInput Types.EndpointName
eiEndpointName = Lens.field @"endpointName"
{-# INLINEABLE eiEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | Path to the filesystem where the endpoint data is available to the container.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiLocalPath :: Lens.Lens' EndpointInput Types.ProcessingLocalPath
eiLocalPath = Lens.field @"localPath"
{-# INLINEABLE eiLocalPath #-}
{-# DEPRECATED localPath "Use generic-lens or generic-optics with 'localPath' instead"  #-}

-- | Whether input data distributed in Amazon S3 is fully replicated or sharded by an S3 key. Defauts to @FullyReplicated@ 
--
-- /Note:/ Consider using 's3DataDistributionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3DataDistributionType :: Lens.Lens' EndpointInput (Core.Maybe Types.ProcessingS3DataDistributionType)
eiS3DataDistributionType = Lens.field @"s3DataDistributionType"
{-# INLINEABLE eiS3DataDistributionType #-}
{-# DEPRECATED s3DataDistributionType "Use generic-lens or generic-optics with 's3DataDistributionType' instead"  #-}

-- | Whether the @Pipe@ or @File@ is used as the input mode for transfering data for the monitoring job. @Pipe@ mode is recommended for large datasets. @File@ mode is useful for small files that fit in memory. Defaults to @File@ .
--
-- /Note:/ Consider using 's3InputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3InputMode :: Lens.Lens' EndpointInput (Core.Maybe Types.ProcessingS3InputMode)
eiS3InputMode = Lens.field @"s3InputMode"
{-# INLINEABLE eiS3InputMode #-}
{-# DEPRECATED s3InputMode "Use generic-lens or generic-optics with 's3InputMode' instead"  #-}

instance Core.FromJSON EndpointInput where
        toJSON EndpointInput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointName" Core..= endpointName),
                  Core.Just ("LocalPath" Core..= localPath),
                  ("S3DataDistributionType" Core..=) Core.<$> s3DataDistributionType,
                  ("S3InputMode" Core..=) Core.<$> s3InputMode])

instance Core.FromJSON EndpointInput where
        parseJSON
          = Core.withObject "EndpointInput" Core.$
              \ x ->
                EndpointInput' Core.<$>
                  (x Core..: "EndpointName") Core.<*> x Core..: "LocalPath" Core.<*>
                    x Core..:? "S3DataDistributionType"
                    Core.<*> x Core..:? "S3InputMode"
