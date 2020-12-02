{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.LanguageModel where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.BaseModelName
import Network.AWS.Transcribe.Types.CLMLanguageCode
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.ModelStatus

-- | The structure used to describe a custom language model.
--
--
--
-- /See:/ 'languageModel' smart constructor.
data LanguageModel = LanguageModel'
  { _lmFailureReason ::
      !(Maybe Text),
    _lmLanguageCode :: !(Maybe CLMLanguageCode),
    _lmModelName :: !(Maybe Text),
    _lmLastModifiedTime :: !(Maybe POSIX),
    _lmUpgradeAvailability :: !(Maybe Bool),
    _lmInputDataConfig :: !(Maybe InputDataConfig),
    _lmBaseModelName :: !(Maybe BaseModelName),
    _lmModelStatus :: !(Maybe ModelStatus),
    _lmCreateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LanguageModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmFailureReason' - The reason why the custom language model couldn't be created.
--
-- * 'lmLanguageCode' - The language code you used to create your custom language model.
--
-- * 'lmModelName' - The name of the custom language model.
--
-- * 'lmLastModifiedTime' - The most recent time the custom language model was modified.
--
-- * 'lmUpgradeAvailability' - Whether the base model used for the custom language model is up to date. If this field is @true@ then you are running the most up-to-date version of the base model in your custom language model.
--
-- * 'lmInputDataConfig' - The data access role and Amazon S3 prefixes for the input files used to train the custom language model.
--
-- * 'lmBaseModelName' - The Amazon Transcribe standard language model, or base model used to create the custom language model.
--
-- * 'lmModelStatus' - The creation status of a custom language model. When the status is @COMPLETED@ the model is ready for use.
--
-- * 'lmCreateTime' - The time the custom language model was created.
languageModel ::
  LanguageModel
languageModel =
  LanguageModel'
    { _lmFailureReason = Nothing,
      _lmLanguageCode = Nothing,
      _lmModelName = Nothing,
      _lmLastModifiedTime = Nothing,
      _lmUpgradeAvailability = Nothing,
      _lmInputDataConfig = Nothing,
      _lmBaseModelName = Nothing,
      _lmModelStatus = Nothing,
      _lmCreateTime = Nothing
    }

-- | The reason why the custom language model couldn't be created.
lmFailureReason :: Lens' LanguageModel (Maybe Text)
lmFailureReason = lens _lmFailureReason (\s a -> s {_lmFailureReason = a})

-- | The language code you used to create your custom language model.
lmLanguageCode :: Lens' LanguageModel (Maybe CLMLanguageCode)
lmLanguageCode = lens _lmLanguageCode (\s a -> s {_lmLanguageCode = a})

-- | The name of the custom language model.
lmModelName :: Lens' LanguageModel (Maybe Text)
lmModelName = lens _lmModelName (\s a -> s {_lmModelName = a})

-- | The most recent time the custom language model was modified.
lmLastModifiedTime :: Lens' LanguageModel (Maybe UTCTime)
lmLastModifiedTime = lens _lmLastModifiedTime (\s a -> s {_lmLastModifiedTime = a}) . mapping _Time

-- | Whether the base model used for the custom language model is up to date. If this field is @true@ then you are running the most up-to-date version of the base model in your custom language model.
lmUpgradeAvailability :: Lens' LanguageModel (Maybe Bool)
lmUpgradeAvailability = lens _lmUpgradeAvailability (\s a -> s {_lmUpgradeAvailability = a})

-- | The data access role and Amazon S3 prefixes for the input files used to train the custom language model.
lmInputDataConfig :: Lens' LanguageModel (Maybe InputDataConfig)
lmInputDataConfig = lens _lmInputDataConfig (\s a -> s {_lmInputDataConfig = a})

-- | The Amazon Transcribe standard language model, or base model used to create the custom language model.
lmBaseModelName :: Lens' LanguageModel (Maybe BaseModelName)
lmBaseModelName = lens _lmBaseModelName (\s a -> s {_lmBaseModelName = a})

-- | The creation status of a custom language model. When the status is @COMPLETED@ the model is ready for use.
lmModelStatus :: Lens' LanguageModel (Maybe ModelStatus)
lmModelStatus = lens _lmModelStatus (\s a -> s {_lmModelStatus = a})

-- | The time the custom language model was created.
lmCreateTime :: Lens' LanguageModel (Maybe UTCTime)
lmCreateTime = lens _lmCreateTime (\s a -> s {_lmCreateTime = a}) . mapping _Time

instance FromJSON LanguageModel where
  parseJSON =
    withObject
      "LanguageModel"
      ( \x ->
          LanguageModel'
            <$> (x .:? "FailureReason")
            <*> (x .:? "LanguageCode")
            <*> (x .:? "ModelName")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "UpgradeAvailability")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "BaseModelName")
            <*> (x .:? "ModelStatus")
            <*> (x .:? "CreateTime")
      )

instance Hashable LanguageModel

instance NFData LanguageModel
