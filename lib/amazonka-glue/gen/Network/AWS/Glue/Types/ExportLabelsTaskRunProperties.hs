{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
  ( ExportLabelsTaskRunProperties (..),

    -- * Smart constructor
    mkExportLabelsTaskRunProperties,

    -- * Lenses
    eltrpOutputS3Path,
  )
where

import qualified Network.AWS.Glue.Types.OutputS3Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies configuration properties for an exporting labels task run.
--
-- /See:/ 'mkExportLabelsTaskRunProperties' smart constructor.
newtype ExportLabelsTaskRunProperties = ExportLabelsTaskRunProperties'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where you will export the labels.
    outputS3Path :: Core.Maybe Types.OutputS3Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExportLabelsTaskRunProperties' value with any optional fields omitted.
mkExportLabelsTaskRunProperties ::
  ExportLabelsTaskRunProperties
mkExportLabelsTaskRunProperties =
  ExportLabelsTaskRunProperties' {outputS3Path = Core.Nothing}

-- | The Amazon Simple Storage Service (Amazon S3) path where you will export the labels.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eltrpOutputS3Path :: Lens.Lens' ExportLabelsTaskRunProperties (Core.Maybe Types.OutputS3Path)
eltrpOutputS3Path = Lens.field @"outputS3Path"
{-# DEPRECATED eltrpOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

instance Core.FromJSON ExportLabelsTaskRunProperties where
  parseJSON =
    Core.withObject "ExportLabelsTaskRunProperties" Core.$
      \x ->
        ExportLabelsTaskRunProperties' Core.<$> (x Core..:? "OutputS3Path")
