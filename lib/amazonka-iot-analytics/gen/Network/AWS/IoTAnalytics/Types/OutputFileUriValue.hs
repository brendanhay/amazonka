{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.OutputFileUriValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.OutputFileUriValue
  ( OutputFileUriValue (..)
  -- * Smart constructor
  , mkOutputFileUriValue
  -- * Lenses
  , ofuvFileName
  ) where

import qualified Network.AWS.IoTAnalytics.Types.FileName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The value of the variable as a structure that specifies an output file URI.
--
-- /See:/ 'mkOutputFileUriValue' smart constructor.
newtype OutputFileUriValue = OutputFileUriValue'
  { fileName :: Types.FileName
    -- ^ The URI of the location where dataset contents are stored, usually the URI of a file in an S3 bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputFileUriValue' value with any optional fields omitted.
mkOutputFileUriValue
    :: Types.FileName -- ^ 'fileName'
    -> OutputFileUriValue
mkOutputFileUriValue fileName = OutputFileUriValue'{fileName}

-- | The URI of the location where dataset contents are stored, usually the URI of a file in an S3 bucket.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofuvFileName :: Lens.Lens' OutputFileUriValue Types.FileName
ofuvFileName = Lens.field @"fileName"
{-# INLINEABLE ofuvFileName #-}
{-# DEPRECATED fileName "Use generic-lens or generic-optics with 'fileName' instead"  #-}

instance Core.FromJSON OutputFileUriValue where
        toJSON OutputFileUriValue{..}
          = Core.object
              (Core.catMaybes [Core.Just ("fileName" Core..= fileName)])

instance Core.FromJSON OutputFileUriValue where
        parseJSON
          = Core.withObject "OutputFileUriValue" Core.$
              \ x -> OutputFileUriValue' Core.<$> (x Core..: "fileName")
