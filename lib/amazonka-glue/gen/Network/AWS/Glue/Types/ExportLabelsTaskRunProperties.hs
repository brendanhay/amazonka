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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration properties for an exporting labels task run.
--
-- /See:/ 'mkExportLabelsTaskRunProperties' smart constructor.
newtype ExportLabelsTaskRunProperties = ExportLabelsTaskRunProperties'
  { outputS3Path ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportLabelsTaskRunProperties' with the minimum fields required to make a request.
--
-- * 'outputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you will export the labels.
mkExportLabelsTaskRunProperties ::
  ExportLabelsTaskRunProperties
mkExportLabelsTaskRunProperties =
  ExportLabelsTaskRunProperties' {outputS3Path = Lude.Nothing}

-- | The Amazon Simple Storage Service (Amazon S3) path where you will export the labels.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eltrpOutputS3Path :: Lens.Lens' ExportLabelsTaskRunProperties (Lude.Maybe Lude.Text)
eltrpOutputS3Path = Lens.lens (outputS3Path :: ExportLabelsTaskRunProperties -> Lude.Maybe Lude.Text) (\s a -> s {outputS3Path = a} :: ExportLabelsTaskRunProperties)
{-# DEPRECATED eltrpOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

instance Lude.FromJSON ExportLabelsTaskRunProperties where
  parseJSON =
    Lude.withObject
      "ExportLabelsTaskRunProperties"
      ( \x ->
          ExportLabelsTaskRunProperties'
            Lude.<$> (x Lude..:? "OutputS3Path")
      )
