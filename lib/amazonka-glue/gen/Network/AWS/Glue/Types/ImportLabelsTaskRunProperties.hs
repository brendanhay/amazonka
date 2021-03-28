{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
  ( ImportLabelsTaskRunProperties (..)
  -- * Smart constructor
  , mkImportLabelsTaskRunProperties
  -- * Lenses
  , iltrpInputS3Path
  , iltrpReplace
  ) where

import qualified Network.AWS.Glue.Types.InputS3Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies configuration properties for an importing labels task run.
--
-- /See:/ 'mkImportLabelsTaskRunProperties' smart constructor.
data ImportLabelsTaskRunProperties = ImportLabelsTaskRunProperties'
  { inputS3Path :: Core.Maybe Types.InputS3Path
    -- ^ The Amazon Simple Storage Service (Amazon S3) path from where you will import the labels.
  , replace :: Core.Maybe Core.Bool
    -- ^ Indicates whether to overwrite your existing labels.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportLabelsTaskRunProperties' value with any optional fields omitted.
mkImportLabelsTaskRunProperties
    :: ImportLabelsTaskRunProperties
mkImportLabelsTaskRunProperties
  = ImportLabelsTaskRunProperties'{inputS3Path = Core.Nothing,
                                   replace = Core.Nothing}

-- | The Amazon Simple Storage Service (Amazon S3) path from where you will import the labels.
--
-- /Note:/ Consider using 'inputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iltrpInputS3Path :: Lens.Lens' ImportLabelsTaskRunProperties (Core.Maybe Types.InputS3Path)
iltrpInputS3Path = Lens.field @"inputS3Path"
{-# INLINEABLE iltrpInputS3Path #-}
{-# DEPRECATED inputS3Path "Use generic-lens or generic-optics with 'inputS3Path' instead"  #-}

-- | Indicates whether to overwrite your existing labels.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iltrpReplace :: Lens.Lens' ImportLabelsTaskRunProperties (Core.Maybe Core.Bool)
iltrpReplace = Lens.field @"replace"
{-# INLINEABLE iltrpReplace #-}
{-# DEPRECATED replace "Use generic-lens or generic-optics with 'replace' instead"  #-}

instance Core.FromJSON ImportLabelsTaskRunProperties where
        parseJSON
          = Core.withObject "ImportLabelsTaskRunProperties" Core.$
              \ x ->
                ImportLabelsTaskRunProperties' Core.<$>
                  (x Core..:? "InputS3Path") Core.<*> x Core..:? "Replace"
