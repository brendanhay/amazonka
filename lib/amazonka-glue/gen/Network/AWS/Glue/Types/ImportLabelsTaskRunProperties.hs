-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
  ( ImportLabelsTaskRunProperties (..),

    -- * Smart constructor
    mkImportLabelsTaskRunProperties,

    -- * Lenses
    iltrpReplace,
    iltrpInputS3Path,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration properties for an importing labels task run.
--
-- /See:/ 'mkImportLabelsTaskRunProperties' smart constructor.
data ImportLabelsTaskRunProperties = ImportLabelsTaskRunProperties'
  { replace ::
      Lude.Maybe Lude.Bool,
    inputS3Path ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportLabelsTaskRunProperties' with the minimum fields required to make a request.
--
-- * 'inputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you will import the labels.
-- * 'replace' - Indicates whether to overwrite your existing labels.
mkImportLabelsTaskRunProperties ::
  ImportLabelsTaskRunProperties
mkImportLabelsTaskRunProperties =
  ImportLabelsTaskRunProperties'
    { replace = Lude.Nothing,
      inputS3Path = Lude.Nothing
    }

-- | Indicates whether to overwrite your existing labels.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iltrpReplace :: Lens.Lens' ImportLabelsTaskRunProperties (Lude.Maybe Lude.Bool)
iltrpReplace = Lens.lens (replace :: ImportLabelsTaskRunProperties -> Lude.Maybe Lude.Bool) (\s a -> s {replace = a} :: ImportLabelsTaskRunProperties)
{-# DEPRECATED iltrpReplace "Use generic-lens or generic-optics with 'replace' instead." #-}

-- | The Amazon Simple Storage Service (Amazon S3) path from where you will import the labels.
--
-- /Note:/ Consider using 'inputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iltrpInputS3Path :: Lens.Lens' ImportLabelsTaskRunProperties (Lude.Maybe Lude.Text)
iltrpInputS3Path = Lens.lens (inputS3Path :: ImportLabelsTaskRunProperties -> Lude.Maybe Lude.Text) (\s a -> s {inputS3Path = a} :: ImportLabelsTaskRunProperties)
{-# DEPRECATED iltrpInputS3Path "Use generic-lens or generic-optics with 'inputS3Path' instead." #-}

instance Lude.FromJSON ImportLabelsTaskRunProperties where
  parseJSON =
    Lude.withObject
      "ImportLabelsTaskRunProperties"
      ( \x ->
          ImportLabelsTaskRunProperties'
            Lude.<$> (x Lude..:? "Replace") Lude.<*> (x Lude..:? "InputS3Path")
      )
