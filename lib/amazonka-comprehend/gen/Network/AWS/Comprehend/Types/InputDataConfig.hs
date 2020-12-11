-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.InputDataConfig
  ( InputDataConfig (..),

    -- * Smart constructor
    mkInputDataConfig,

    -- * Lenses
    idcInputFormat,
    idcS3URI,
  )
where

import Network.AWS.Comprehend.Types.InputFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The input properties for a topic detection job.
--
-- /See:/ 'mkInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { inputFormat ::
      Lude.Maybe InputFormat,
    s3URI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- * 'inputFormat' - Specifies how the text in an input file should be processed:
--
--
--     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.
--
--
--     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
--
--
-- * 's3URI' - The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files.
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
mkInputDataConfig ::
  -- | 's3URI'
  Lude.Text ->
  InputDataConfig
mkInputDataConfig pS3URI_ =
  InputDataConfig' {inputFormat = Lude.Nothing, s3URI = pS3URI_}

-- | Specifies how the text in an input file should be processed:
--
--
--     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.
--
--
--     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
--
--
--
-- /Note:/ Consider using 'inputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcInputFormat :: Lens.Lens' InputDataConfig (Lude.Maybe InputFormat)
idcInputFormat = Lens.lens (inputFormat :: InputDataConfig -> Lude.Maybe InputFormat) (\s a -> s {inputFormat = a} :: InputDataConfig)
{-# DEPRECATED idcInputFormat "Use generic-lens or generic-optics with 'inputFormat' instead." #-}

-- | The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files.
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcS3URI :: Lens.Lens' InputDataConfig Lude.Text
idcS3URI = Lens.lens (s3URI :: InputDataConfig -> Lude.Text) (\s a -> s {s3URI = a} :: InputDataConfig)
{-# DEPRECATED idcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON InputDataConfig where
  parseJSON =
    Lude.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Lude.<$> (x Lude..:? "InputFormat") Lude.<*> (x Lude..: "S3Uri")
      )

instance Lude.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputFormat" Lude..=) Lude.<$> inputFormat,
            Lude.Just ("S3Uri" Lude..= s3URI)
          ]
      )
