{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CodeSigning
  ( CodeSigning (..)
  -- * Smart constructor
  , mkCodeSigning
  -- * Lenses
  , csAwsSignerJobId
  , csCustomCodeSigning
  , csStartSigningJobParameter
  ) where

import qualified Network.AWS.IoT.Types.AwsSignerJobId as Types
import qualified Network.AWS.IoT.Types.CustomCodeSigning as Types
import qualified Network.AWS.IoT.Types.StartSigningJobParameter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the method to use when code signing a file.
--
-- /See:/ 'mkCodeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { awsSignerJobId :: Core.Maybe Types.AwsSignerJobId
    -- ^ The ID of the AWSSignerJob which was created to sign the file.
  , customCodeSigning :: Core.Maybe Types.CustomCodeSigning
    -- ^ A custom method for code signing a file.
  , startSigningJobParameter :: Core.Maybe Types.StartSigningJobParameter
    -- ^ Describes the code-signing job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeSigning' value with any optional fields omitted.
mkCodeSigning
    :: CodeSigning
mkCodeSigning
  = CodeSigning'{awsSignerJobId = Core.Nothing,
                 customCodeSigning = Core.Nothing,
                 startSigningJobParameter = Core.Nothing}

-- | The ID of the AWSSignerJob which was created to sign the file.
--
-- /Note:/ Consider using 'awsSignerJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAwsSignerJobId :: Lens.Lens' CodeSigning (Core.Maybe Types.AwsSignerJobId)
csAwsSignerJobId = Lens.field @"awsSignerJobId"
{-# INLINEABLE csAwsSignerJobId #-}
{-# DEPRECATED awsSignerJobId "Use generic-lens or generic-optics with 'awsSignerJobId' instead"  #-}

-- | A custom method for code signing a file.
--
-- /Note:/ Consider using 'customCodeSigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomCodeSigning :: Lens.Lens' CodeSigning (Core.Maybe Types.CustomCodeSigning)
csCustomCodeSigning = Lens.field @"customCodeSigning"
{-# INLINEABLE csCustomCodeSigning #-}
{-# DEPRECATED customCodeSigning "Use generic-lens or generic-optics with 'customCodeSigning' instead"  #-}

-- | Describes the code-signing job.
--
-- /Note:/ Consider using 'startSigningJobParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStartSigningJobParameter :: Lens.Lens' CodeSigning (Core.Maybe Types.StartSigningJobParameter)
csStartSigningJobParameter = Lens.field @"startSigningJobParameter"
{-# INLINEABLE csStartSigningJobParameter #-}
{-# DEPRECATED startSigningJobParameter "Use generic-lens or generic-optics with 'startSigningJobParameter' instead"  #-}

instance Core.FromJSON CodeSigning where
        toJSON CodeSigning{..}
          = Core.object
              (Core.catMaybes
                 [("awsSignerJobId" Core..=) Core.<$> awsSignerJobId,
                  ("customCodeSigning" Core..=) Core.<$> customCodeSigning,
                  ("startSigningJobParameter" Core..=) Core.<$>
                    startSigningJobParameter])

instance Core.FromJSON CodeSigning where
        parseJSON
          = Core.withObject "CodeSigning" Core.$
              \ x ->
                CodeSigning' Core.<$>
                  (x Core..:? "awsSignerJobId") Core.<*>
                    x Core..:? "customCodeSigning"
                    Core.<*> x Core..:? "startSigningJobParameter"
