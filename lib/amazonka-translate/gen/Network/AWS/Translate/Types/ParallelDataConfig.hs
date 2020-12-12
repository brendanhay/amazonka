{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataConfig
  ( ParallelDataConfig (..),

    -- * Smart constructor
    mkParallelDataConfig,

    -- * Lenses
    pdcS3URI,
    pdcFormat,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.ParallelDataFormat

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /See:/ 'mkParallelDataConfig' smart constructor.
data ParallelDataConfig = ParallelDataConfig'
  { s3URI :: Lude.Text,
    format :: ParallelDataFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParallelDataConfig' with the minimum fields required to make a request.
--
-- * 'format' - The format of the parallel data input file.
-- * 's3URI' - The URI of the Amazon S3 folder that contains the parallel data input file. The folder must be in the same Region as the API endpoint you are calling.
mkParallelDataConfig ::
  -- | 's3URI'
  Lude.Text ->
  -- | 'format'
  ParallelDataFormat ->
  ParallelDataConfig
mkParallelDataConfig pS3URI_ pFormat_ =
  ParallelDataConfig' {s3URI = pS3URI_, format = pFormat_}

-- | The URI of the Amazon S3 folder that contains the parallel data input file. The folder must be in the same Region as the API endpoint you are calling.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcS3URI :: Lens.Lens' ParallelDataConfig Lude.Text
pdcS3URI = Lens.lens (s3URI :: ParallelDataConfig -> Lude.Text) (\s a -> s {s3URI = a} :: ParallelDataConfig)
{-# DEPRECATED pdcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

-- | The format of the parallel data input file.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcFormat :: Lens.Lens' ParallelDataConfig ParallelDataFormat
pdcFormat = Lens.lens (format :: ParallelDataConfig -> ParallelDataFormat) (\s a -> s {format = a} :: ParallelDataConfig)
{-# DEPRECATED pdcFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.FromJSON ParallelDataConfig where
  parseJSON =
    Lude.withObject
      "ParallelDataConfig"
      ( \x ->
          ParallelDataConfig'
            Lude.<$> (x Lude..: "S3Uri") Lude.<*> (x Lude..: "Format")
      )

instance Lude.ToJSON ParallelDataConfig where
  toJSON ParallelDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3Uri" Lude..= s3URI),
            Lude.Just ("Format" Lude..= format)
          ]
      )
