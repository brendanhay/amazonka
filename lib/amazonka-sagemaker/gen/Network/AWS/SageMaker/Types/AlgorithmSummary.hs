{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSummary
  ( AlgorithmSummary (..),

    -- * Smart constructor
    mkAlgorithmSummary,

    -- * Lenses
    aAlgorithmName,
    aAlgorithmArn,
    aCreationTime,
    aAlgorithmStatus,
    aAlgorithmDescription,
  )
where

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
  { -- | The name of the algorithm that is described by the summary.
    algorithmName :: Types.EntityName,
    -- | The Amazon Resource Name (ARN) of the algorithm.
    algorithmArn :: Types.AlgorithmArn,
    -- | A timestamp that shows when the algorithm was created.
    creationTime :: Core.NominalDiffTime,
    -- | The overall status of the algorithm.
    algorithmStatus :: Types.AlgorithmStatus,
    -- | A brief description of the algorithm.
    algorithmDescription :: Core.Maybe Types.EntityDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AlgorithmSummary' value with any optional fields omitted.
mkAlgorithmSummary ::
  -- | 'algorithmName'
  Types.EntityName ->
  -- | 'algorithmArn'
  Types.AlgorithmArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'algorithmStatus'
  Types.AlgorithmStatus ->
  AlgorithmSummary
mkAlgorithmSummary
  algorithmName
  algorithmArn
  creationTime
  algorithmStatus =
    AlgorithmSummary'
      { algorithmName,
        algorithmArn,
        creationTime,
        algorithmStatus,
        algorithmDescription = Core.Nothing
      }

-- | The name of the algorithm that is described by the summary.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmName :: Lens.Lens' AlgorithmSummary Types.EntityName
aAlgorithmName = Lens.field @"algorithmName"
{-# DEPRECATED aAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | The Amazon Resource Name (ARN) of the algorithm.
--
-- /Note:/ Consider using 'algorithmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmArn :: Lens.Lens' AlgorithmSummary Types.AlgorithmArn
aAlgorithmArn = Lens.field @"algorithmArn"
{-# DEPRECATED aAlgorithmArn "Use generic-lens or generic-optics with 'algorithmArn' instead." #-}

-- | A timestamp that shows when the algorithm was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreationTime :: Lens.Lens' AlgorithmSummary Core.NominalDiffTime
aCreationTime = Lens.field @"creationTime"
{-# DEPRECATED aCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The overall status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmStatus :: Lens.Lens' AlgorithmSummary Types.AlgorithmStatus
aAlgorithmStatus = Lens.field @"algorithmStatus"
{-# DEPRECATED aAlgorithmStatus "Use generic-lens or generic-optics with 'algorithmStatus' instead." #-}

-- | A brief description of the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlgorithmDescription :: Lens.Lens' AlgorithmSummary (Core.Maybe Types.EntityDescription)
aAlgorithmDescription = Lens.field @"algorithmDescription"
{-# DEPRECATED aAlgorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead." #-}

instance Core.FromJSON AlgorithmSummary where
  parseJSON =
    Core.withObject "AlgorithmSummary" Core.$
      \x ->
        AlgorithmSummary'
          Core.<$> (x Core..: "AlgorithmName")
          Core.<*> (x Core..: "AlgorithmArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "AlgorithmStatus")
          Core.<*> (x Core..:? "AlgorithmDescription")
