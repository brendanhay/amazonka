{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingInput
  ( ProcessingInput (..),

    -- * Smart constructor
    mkProcessingInput,

    -- * Lenses
    piInputName,
    piS3Input,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ProcessingS3Input as Types
import qualified Network.AWS.SageMaker.Types.String as Types

-- | The inputs for a processing job.
--
-- /See:/ 'mkProcessingInput' smart constructor.
data ProcessingInput = ProcessingInput'
  { -- | The name of the inputs for the processing job.
    inputName :: Types.String,
    -- | The S3 inputs for the processing job.
    s3Input :: Types.ProcessingS3Input
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingInput' value with any optional fields omitted.
mkProcessingInput ::
  -- | 'inputName'
  Types.String ->
  -- | 's3Input'
  Types.ProcessingS3Input ->
  ProcessingInput
mkProcessingInput inputName s3Input =
  ProcessingInput' {inputName, s3Input}

-- | The name of the inputs for the processing job.
--
-- /Note:/ Consider using 'inputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piInputName :: Lens.Lens' ProcessingInput Types.String
piInputName = Lens.field @"inputName"
{-# DEPRECATED piInputName "Use generic-lens or generic-optics with 'inputName' instead." #-}

-- | The S3 inputs for the processing job.
--
-- /Note:/ Consider using 's3Input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piS3Input :: Lens.Lens' ProcessingInput Types.ProcessingS3Input
piS3Input = Lens.field @"s3Input"
{-# DEPRECATED piS3Input "Use generic-lens or generic-optics with 's3Input' instead." #-}

instance Core.FromJSON ProcessingInput where
  toJSON ProcessingInput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InputName" Core..= inputName),
            Core.Just ("S3Input" Core..= s3Input)
          ]
      )

instance Core.FromJSON ProcessingInput where
  parseJSON =
    Core.withObject "ProcessingInput" Core.$
      \x ->
        ProcessingInput'
          Core.<$> (x Core..: "InputName") Core.<*> (x Core..: "S3Input")
