{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.InputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The object that contains the Amazon S3 object location and access role required to train and tune your custom language model.
--
--
--
-- /See:/ 'inputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { _idcTuningDataS3URI ::
      !(Maybe Text),
    _idcS3URI :: !Text,
    _idcDataAccessRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcTuningDataS3URI' - The Amazon S3 prefix you specify to access the plain text files that you use to tune your custom language model.
--
-- * 'idcS3URI' - The Amazon S3 prefix you specify to access the plain text files that you use to train your custom language model.
--
-- * 'idcDataAccessRoleARN' - The Amazon Resource Name (ARN) that uniquely identifies the permissions you've given Amazon Transcribe to access your Amazon S3 buckets containing your media files or text data.
inputDataConfig ::
  -- | 'idcS3URI'
  Text ->
  -- | 'idcDataAccessRoleARN'
  Text ->
  InputDataConfig
inputDataConfig pS3URI_ pDataAccessRoleARN_ =
  InputDataConfig'
    { _idcTuningDataS3URI = Nothing,
      _idcS3URI = pS3URI_,
      _idcDataAccessRoleARN = pDataAccessRoleARN_
    }

-- | The Amazon S3 prefix you specify to access the plain text files that you use to tune your custom language model.
idcTuningDataS3URI :: Lens' InputDataConfig (Maybe Text)
idcTuningDataS3URI = lens _idcTuningDataS3URI (\s a -> s {_idcTuningDataS3URI = a})

-- | The Amazon S3 prefix you specify to access the plain text files that you use to train your custom language model.
idcS3URI :: Lens' InputDataConfig Text
idcS3URI = lens _idcS3URI (\s a -> s {_idcS3URI = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the permissions you've given Amazon Transcribe to access your Amazon S3 buckets containing your media files or text data.
idcDataAccessRoleARN :: Lens' InputDataConfig Text
idcDataAccessRoleARN = lens _idcDataAccessRoleARN (\s a -> s {_idcDataAccessRoleARN = a})

instance FromJSON InputDataConfig where
  parseJSON =
    withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            <$> (x .:? "TuningDataS3Uri")
            <*> (x .: "S3Uri")
            <*> (x .: "DataAccessRoleArn")
      )

instance Hashable InputDataConfig

instance NFData InputDataConfig

instance ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    object
      ( catMaybes
          [ ("TuningDataS3Uri" .=) <$> _idcTuningDataS3URI,
            Just ("S3Uri" .= _idcS3URI),
            Just ("DataAccessRoleArn" .= _idcDataAccessRoleARN)
          ]
      )
