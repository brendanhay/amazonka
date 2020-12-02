{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.InputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input configuration properties for requesting a batch translation job.
--
--
--
-- /See:/ 'inputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { _idcS3URI :: !Text,
    _idcContentType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcS3URI' - The URI of the AWS S3 folder that contains the input file. The folder must be in the same Region as the API endpoint you are calling.
--
-- * 'idcContentType' - Describes the format of the data that you submit to Amazon Translate as input. You can specify one of the following multipurpose internet mail extension (MIME) types:     * @text/html@ : The input data consists of one or more HTML files. Amazon Translate translates only the text that resides in the @html@ element in each file.     * @text/plain@ : The input data consists of one or more unformatted text files. Amazon Translate translates every character in this type of input.     * @application/vnd.openxmlformats-officedocument.wordprocessingml.document@ : The input data consists of one or more Word documents (.docx).     * @application/vnd.openxmlformats-officedocument.presentationml.presentation@ : The input data consists of one or more PowerPoint Presentation files (.pptx).     * @application/vnd.openxmlformats-officedocument.spreadsheetml.sheet@ : The input data consists of one or more Excel Workbook files (.xlsx). /Important:/ If you structure your input data as HTML, ensure that you set this parameter to @text/html@ . By doing so, you cut costs by limiting the translation to the contents of the @html@ element in each file. Otherwise, if you set this parameter to @text/plain@ , your costs will cover the translation of every character.
inputDataConfig ::
  -- | 'idcS3URI'
  Text ->
  -- | 'idcContentType'
  Text ->
  InputDataConfig
inputDataConfig pS3URI_ pContentType_ =
  InputDataConfig'
    { _idcS3URI = pS3URI_,
      _idcContentType = pContentType_
    }

-- | The URI of the AWS S3 folder that contains the input file. The folder must be in the same Region as the API endpoint you are calling.
idcS3URI :: Lens' InputDataConfig Text
idcS3URI = lens _idcS3URI (\s a -> s {_idcS3URI = a})

-- | Describes the format of the data that you submit to Amazon Translate as input. You can specify one of the following multipurpose internet mail extension (MIME) types:     * @text/html@ : The input data consists of one or more HTML files. Amazon Translate translates only the text that resides in the @html@ element in each file.     * @text/plain@ : The input data consists of one or more unformatted text files. Amazon Translate translates every character in this type of input.     * @application/vnd.openxmlformats-officedocument.wordprocessingml.document@ : The input data consists of one or more Word documents (.docx).     * @application/vnd.openxmlformats-officedocument.presentationml.presentation@ : The input data consists of one or more PowerPoint Presentation files (.pptx).     * @application/vnd.openxmlformats-officedocument.spreadsheetml.sheet@ : The input data consists of one or more Excel Workbook files (.xlsx). /Important:/ If you structure your input data as HTML, ensure that you set this parameter to @text/html@ . By doing so, you cut costs by limiting the translation to the contents of the @html@ element in each file. Otherwise, if you set this parameter to @text/plain@ , your costs will cover the translation of every character.
idcContentType :: Lens' InputDataConfig Text
idcContentType = lens _idcContentType (\s a -> s {_idcContentType = a})

instance FromJSON InputDataConfig where
  parseJSON =
    withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig' <$> (x .: "S3Uri") <*> (x .: "ContentType")
      )

instance Hashable InputDataConfig

instance NFData InputDataConfig

instance ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    object
      ( catMaybes
          [ Just ("S3Uri" .= _idcS3URI),
            Just ("ContentType" .= _idcContentType)
          ]
      )
