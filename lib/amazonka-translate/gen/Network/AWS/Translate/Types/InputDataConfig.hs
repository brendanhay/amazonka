{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.InputDataConfig
  ( InputDataConfig (..),

    -- * Smart constructor
    mkInputDataConfig,

    -- * Lenses
    idcS3URI,
    idcContentType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The input configuration properties for requesting a batch translation job.
--
-- /See:/ 'mkInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { s3URI :: Lude.Text,
    contentType :: Lude.Text
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
-- * 'contentType' - Describes the format of the data that you submit to Amazon Translate as input. You can specify one of the following multipurpose internet mail extension (MIME) types:
--
--
--     * @text/html@ : The input data consists of one or more HTML files. Amazon Translate translates only the text that resides in the @html@ element in each file.
--
--
--     * @text/plain@ : The input data consists of one or more unformatted text files. Amazon Translate translates every character in this type of input.
--
--
--     * @application/vnd.openxmlformats-officedocument.wordprocessingml.document@ : The input data consists of one or more Word documents (.docx).
--
--
--     * @application/vnd.openxmlformats-officedocument.presentationml.presentation@ : The input data consists of one or more PowerPoint Presentation files (.pptx).
--
--
--     * @application/vnd.openxmlformats-officedocument.spreadsheetml.sheet@ : The input data consists of one or more Excel Workbook files (.xlsx).
--
--
-- /Important:/ If you structure your input data as HTML, ensure that you set this parameter to @text/html@ . By doing so, you cut costs by limiting the translation to the contents of the @html@ element in each file. Otherwise, if you set this parameter to @text/plain@ , your costs will cover the translation of every character.
-- * 's3URI' - The URI of the AWS S3 folder that contains the input file. The folder must be in the same Region as the API endpoint you are calling.
mkInputDataConfig ::
  -- | 's3URI'
  Lude.Text ->
  -- | 'contentType'
  Lude.Text ->
  InputDataConfig
mkInputDataConfig pS3URI_ pContentType_ =
  InputDataConfig' {s3URI = pS3URI_, contentType = pContentType_}

-- | The URI of the AWS S3 folder that contains the input file. The folder must be in the same Region as the API endpoint you are calling.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcS3URI :: Lens.Lens' InputDataConfig Lude.Text
idcS3URI = Lens.lens (s3URI :: InputDataConfig -> Lude.Text) (\s a -> s {s3URI = a} :: InputDataConfig)
{-# DEPRECATED idcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

-- | Describes the format of the data that you submit to Amazon Translate as input. You can specify one of the following multipurpose internet mail extension (MIME) types:
--
--
--     * @text/html@ : The input data consists of one or more HTML files. Amazon Translate translates only the text that resides in the @html@ element in each file.
--
--
--     * @text/plain@ : The input data consists of one or more unformatted text files. Amazon Translate translates every character in this type of input.
--
--
--     * @application/vnd.openxmlformats-officedocument.wordprocessingml.document@ : The input data consists of one or more Word documents (.docx).
--
--
--     * @application/vnd.openxmlformats-officedocument.presentationml.presentation@ : The input data consists of one or more PowerPoint Presentation files (.pptx).
--
--
--     * @application/vnd.openxmlformats-officedocument.spreadsheetml.sheet@ : The input data consists of one or more Excel Workbook files (.xlsx).
--
--
-- /Important:/ If you structure your input data as HTML, ensure that you set this parameter to @text/html@ . By doing so, you cut costs by limiting the translation to the contents of the @html@ element in each file. Otherwise, if you set this parameter to @text/plain@ , your costs will cover the translation of every character.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcContentType :: Lens.Lens' InputDataConfig Lude.Text
idcContentType = Lens.lens (contentType :: InputDataConfig -> Lude.Text) (\s a -> s {contentType = a} :: InputDataConfig)
{-# DEPRECATED idcContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromJSON InputDataConfig where
  parseJSON =
    Lude.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Lude.<$> (x Lude..: "S3Uri") Lude.<*> (x Lude..: "ContentType")
      )

instance Lude.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3Uri" Lude..= s3URI),
            Lude.Just ("ContentType" Lude..= contentType)
          ]
      )
