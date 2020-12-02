{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobResourceConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration information for labeling jobs.
--
--
--
-- /See:/ 'labelingJobResourceConfig' smart constructor.
newtype LabelingJobResourceConfig = LabelingJobResourceConfig'
  { _ljrcVolumeKMSKeyId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrcVolumeKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:     * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
labelingJobResourceConfig ::
  LabelingJobResourceConfig
labelingJobResourceConfig =
  LabelingJobResourceConfig' {_ljrcVolumeKMSKeyId = Nothing}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance(s) that run the training job. The @VolumeKmsKeyId@ can be any of the following formats:     * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
ljrcVolumeKMSKeyId :: Lens' LabelingJobResourceConfig (Maybe Text)
ljrcVolumeKMSKeyId = lens _ljrcVolumeKMSKeyId (\s a -> s {_ljrcVolumeKMSKeyId = a})

instance FromJSON LabelingJobResourceConfig where
  parseJSON =
    withObject
      "LabelingJobResourceConfig"
      (\x -> LabelingJobResourceConfig' <$> (x .:? "VolumeKmsKeyId"))

instance Hashable LabelingJobResourceConfig

instance NFData LabelingJobResourceConfig

instance ToJSON LabelingJobResourceConfig where
  toJSON LabelingJobResourceConfig' {..} =
    object
      (catMaybes [("VolumeKmsKeyId" .=) <$> _ljrcVolumeKMSKeyId])
