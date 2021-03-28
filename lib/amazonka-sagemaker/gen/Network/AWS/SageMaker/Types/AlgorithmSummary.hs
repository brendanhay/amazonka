{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmSummary
  ( AlgorithmSummary (..)
  -- * Smart constructor
  , mkAlgorithmSummary
  -- * Lenses
  , aAlgorithmName
  , aAlgorithmArn
  , aCreationTime
  , aAlgorithmStatus
  , aAlgorithmDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AlgorithmArn as Types
import qualified Network.AWS.SageMaker.Types.AlgorithmStatus as Types
import qualified Network.AWS.SageMaker.Types.EntityDescription as Types
import qualified Network.AWS.SageMaker.Types.EntityName as Types

-- | Provides summary information about an algorithm.
--
-- /See:/ 'mkAlgorithmSummary' smart constructor.
data AlgorithmSummary = AlgorithmSummary'
  { algorithmName :: Types.EntityName
    -- ^ The name of the algorithm that is described by the summary.
  , algorithmArn :: Types.AlgorithmArn
    -- ^ The Amazon Resource Name (ARN) of the algorithm.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the algorithm was created.
  , algorithmStatus :: Types.AlgorithmStatus
    -- ^ The overall status of the algorithm.
  , algorithmDescription :: Core.Maybe Types.EntityDescription
    -- ^ A brief description of the algorithm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AlgorithmSummary' value with any optional fields omitted.
mkAlgorithmSummary
    :: Types.EntityName -- ^ 'algorithmName'
    -> Types.AlgorithmArn -- ^ 'algorithmArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.AlgorithmStatus -- ^ 'algorithmStatus'
    -> AlgorithmSummary
mkAlgorithmSummary algorithmName algorithmArn creationTime
  algorithmStatus
  = AlgorithmSummary'{algorithmName, algorithmArn, creationTime,
                      algorithmStatus, algorithmDescription = Core.Nothing}

-- | The name of the algorithm that is described by the summary.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmName :: Lens.Lens' AlgorithmSummary Types.EntityName
aAlgorithmName = Lens.field @"algorithmName"
{-# INLINEABLE aAlgorithmName #-}
{-# DEPRECATED algorithmName "Use generic-lens or generic-optics with 'algorithmName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the algorithm.
--
-- /Note:/ Consider using 'algorithmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmArn :: Lens.Lens' AlgorithmSummary Types.AlgorithmArn
aAlgorithmArn = Lens.field @"algorithmArn"
{-# INLINEABLE aAlgorithmArn #-}
{-# DEPRECATED algorithmArn "Use generic-lens or generic-optics with 'algorithmArn' instead"  #-}

-- | A timestamp that shows when the algorithm was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' AlgorithmSummary Core.NominalDiffTime
aCreationTime = Lens.field @"creationTime"
{-# INLINEABLE aCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The overall status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmStatus :: Lens.Lens' AlgorithmSummary Types.AlgorithmStatus
aAlgorithmStatus = Lens.field @"algorithmStatus"
{-# INLINEABLE aAlgorithmStatus #-}
{-# DEPRECATED algorithmStatus "Use generic-lens or generic-optics with 'algorithmStatus' instead"  #-}

-- | A brief description of the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmDescription :: Lens.Lens' AlgorithmSummary (Core.Maybe Types.EntityDescription)
aAlgorithmDescription = Lens.field @"algorithmDescription"
{-# INLINEABLE aAlgorithmDescription #-}
{-# DEPRECATED algorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead"  #-}

instance Core.FromJSON AlgorithmSummary where
        parseJSON
          = Core.withObject "AlgorithmSummary" Core.$
              \ x ->
                AlgorithmSummary' Core.<$>
                  (x Core..: "AlgorithmName") Core.<*> x Core..: "AlgorithmArn"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "AlgorithmStatus"
                    Core.<*> x Core..:? "AlgorithmDescription"
