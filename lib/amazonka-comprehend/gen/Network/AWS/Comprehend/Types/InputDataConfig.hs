{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.InputDataConfig where

import Network.AWS.Comprehend.Types.InputFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input properties for a topic detection job.
--
--
--
-- /See:/ 'inputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { _idcInputFormat ::
      !(Maybe InputFormat),
    _idcS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcInputFormat' - Specifies how the text in an input file should be processed:     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
--
-- * 'idcS3URI' - The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files.  For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
inputDataConfig ::
  -- | 'idcS3URI'
  Text ->
  InputDataConfig
inputDataConfig pS3URI_ =
  InputDataConfig' {_idcInputFormat = Nothing, _idcS3URI = pS3URI_}

-- | Specifies how the text in an input file should be processed:     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
idcInputFormat :: Lens' InputDataConfig (Maybe InputFormat)
idcInputFormat = lens _idcInputFormat (\s a -> s {_idcInputFormat = a})

-- | The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files.  For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
idcS3URI :: Lens' InputDataConfig Text
idcS3URI = lens _idcS3URI (\s a -> s {_idcS3URI = a})

instance FromJSON InputDataConfig where
  parseJSON =
    withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig' <$> (x .:? "InputFormat") <*> (x .: "S3Uri")
      )

instance Hashable InputDataConfig

instance NFData InputDataConfig

instance ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    object
      ( catMaybes
          [ ("InputFormat" .=) <$> _idcInputFormat,
            Just ("S3Uri" .= _idcS3URI)
          ]
      )
