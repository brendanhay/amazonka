{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.InputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.InputDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The input configuration properties for requesting a batch translation
-- job.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | The URI of the AWS S3 folder that contains the input file. The folder
    -- must be in the same Region as the API endpoint you are calling.
    s3Uri :: Prelude.Text,
    -- | Describes the format of the data that you submit to Amazon Translate as
    -- input. You can specify one of the following multipurpose internet mail
    -- extension (MIME) types:
    --
    -- -   @text\/html@: The input data consists of one or more HTML files.
    --     Amazon Translate translates only the text that resides in the @html@
    --     element in each file.
    --
    -- -   @text\/plain@: The input data consists of one or more unformatted
    --     text files. Amazon Translate translates every character in this type
    --     of input.
    --
    -- -   @application\/vnd.openxmlformats-officedocument.wordprocessingml.document@:
    --     The input data consists of one or more Word documents (.docx).
    --
    -- -   @application\/vnd.openxmlformats-officedocument.presentationml.presentation@:
    --     The input data consists of one or more PowerPoint Presentation files
    --     (.pptx).
    --
    -- -   @application\/vnd.openxmlformats-officedocument.spreadsheetml.sheet@:
    --     The input data consists of one or more Excel Workbook files (.xlsx).
    --
    -- If you structure your input data as HTML, ensure that you set this
    -- parameter to @text\/html@. By doing so, you cut costs by limiting the
    -- translation to the contents of the @html@ element in each file.
    -- Otherwise, if you set this parameter to @text\/plain@, your costs will
    -- cover the translation of every character.
    contentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'inputDataConfig_s3Uri' - The URI of the AWS S3 folder that contains the input file. The folder
-- must be in the same Region as the API endpoint you are calling.
--
-- 'contentType', 'inputDataConfig_contentType' - Describes the format of the data that you submit to Amazon Translate as
-- input. You can specify one of the following multipurpose internet mail
-- extension (MIME) types:
--
-- -   @text\/html@: The input data consists of one or more HTML files.
--     Amazon Translate translates only the text that resides in the @html@
--     element in each file.
--
-- -   @text\/plain@: The input data consists of one or more unformatted
--     text files. Amazon Translate translates every character in this type
--     of input.
--
-- -   @application\/vnd.openxmlformats-officedocument.wordprocessingml.document@:
--     The input data consists of one or more Word documents (.docx).
--
-- -   @application\/vnd.openxmlformats-officedocument.presentationml.presentation@:
--     The input data consists of one or more PowerPoint Presentation files
--     (.pptx).
--
-- -   @application\/vnd.openxmlformats-officedocument.spreadsheetml.sheet@:
--     The input data consists of one or more Excel Workbook files (.xlsx).
--
-- If you structure your input data as HTML, ensure that you set this
-- parameter to @text\/html@. By doing so, you cut costs by limiting the
-- translation to the contents of the @html@ element in each file.
-- Otherwise, if you set this parameter to @text\/plain@, your costs will
-- cover the translation of every character.
newInputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 'contentType'
  Prelude.Text ->
  InputDataConfig
newInputDataConfig pS3Uri_ pContentType_ =
  InputDataConfig'
    { s3Uri = pS3Uri_,
      contentType = pContentType_
    }

-- | The URI of the AWS S3 folder that contains the input file. The folder
-- must be in the same Region as the API endpoint you are calling.
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

-- | Describes the format of the data that you submit to Amazon Translate as
-- input. You can specify one of the following multipurpose internet mail
-- extension (MIME) types:
--
-- -   @text\/html@: The input data consists of one or more HTML files.
--     Amazon Translate translates only the text that resides in the @html@
--     element in each file.
--
-- -   @text\/plain@: The input data consists of one or more unformatted
--     text files. Amazon Translate translates every character in this type
--     of input.
--
-- -   @application\/vnd.openxmlformats-officedocument.wordprocessingml.document@:
--     The input data consists of one or more Word documents (.docx).
--
-- -   @application\/vnd.openxmlformats-officedocument.presentationml.presentation@:
--     The input data consists of one or more PowerPoint Presentation files
--     (.pptx).
--
-- -   @application\/vnd.openxmlformats-officedocument.spreadsheetml.sheet@:
--     The input data consists of one or more Excel Workbook files (.xlsx).
--
-- If you structure your input data as HTML, ensure that you set this
-- parameter to @text\/html@. By doing so, you cut costs by limiting the
-- translation to the contents of the @html@ element in each file.
-- Otherwise, if you set this parameter to @text\/plain@, your costs will
-- cover the translation of every character.
inputDataConfig_contentType :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_contentType = Lens.lens (\InputDataConfig' {contentType} -> contentType) (\s@InputDataConfig' {} a -> s {contentType = a} :: InputDataConfig)

instance Prelude.FromJSON InputDataConfig where
  parseJSON =
    Prelude.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Prelude.<$> (x Prelude..: "S3Uri")
            Prelude.<*> (x Prelude..: "ContentType")
      )

instance Prelude.Hashable InputDataConfig

instance Prelude.NFData InputDataConfig

instance Prelude.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Prelude..= s3Uri),
            Prelude.Just ("ContentType" Prelude..= contentType)
          ]
      )
