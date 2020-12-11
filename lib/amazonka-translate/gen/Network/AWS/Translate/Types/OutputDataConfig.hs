-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.OutputDataConfig
  ( OutputDataConfig (..),

    -- * Smart constructor
    mkOutputDataConfig,

    -- * Lenses
    odcS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The output configuration properties for a batch translation job.
--
-- /See:/ 'mkOutputDataConfig' smart constructor.
newtype OutputDataConfig = OutputDataConfig' {s3URI :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- * 's3URI' - The URI of the S3 folder that contains a translation job's output file. The folder must be in the same Region as the API endpoint that you are calling.
mkOutputDataConfig ::
  -- | 's3URI'
  Lude.Text ->
  OutputDataConfig
mkOutputDataConfig pS3URI_ = OutputDataConfig' {s3URI = pS3URI_}

-- | The URI of the S3 folder that contains a translation job's output file. The folder must be in the same Region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcS3URI :: Lens.Lens' OutputDataConfig Lude.Text
odcS3URI = Lens.lens (s3URI :: OutputDataConfig -> Lude.Text) (\s a -> s {s3URI = a} :: OutputDataConfig)
{-# DEPRECATED odcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON OutputDataConfig where
  parseJSON =
    Lude.withObject
      "OutputDataConfig"
      (\x -> OutputDataConfig' Lude.<$> (x Lude..: "S3Uri"))

instance Lude.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("S3Uri" Lude..= s3URI)])
