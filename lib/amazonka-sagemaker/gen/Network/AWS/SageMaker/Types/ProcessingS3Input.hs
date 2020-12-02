{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3Input where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingS3CompressionType
import Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
import Network.AWS.SageMaker.Types.ProcessingS3DataType
import Network.AWS.SageMaker.Types.ProcessingS3InputMode

-- | Information about where and how you want to obtain the inputs for an processing job.
--
--
--
-- /See:/ 'processingS3Input' smart constructor.
data ProcessingS3Input = ProcessingS3Input'
  { _psiS3DataDistributionType ::
      !(Maybe ProcessingS3DataDistributionType),
    _psiS3CompressionType ::
      !(Maybe ProcessingS3CompressionType),
    _psiS3URI :: !Text,
    _psiLocalPath :: !Text,
    _psiS3DataType :: !ProcessingS3DataType,
    _psiS3InputMode :: !ProcessingS3InputMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingS3Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psiS3DataDistributionType' - Whether the data stored in Amazon S3 is @FullyReplicated@ or @ShardedByS3Key@ .
--
-- * 'psiS3CompressionType' - Whether to use @Gzip@ compression for Amazon S3 storage.
--
-- * 'psiS3URI' - The URI for the Amazon S3 storage where you want Amazon SageMaker to download the artifacts needed to run a processing job.
--
-- * 'psiLocalPath' - The local path to the Amazon S3 bucket where you want Amazon SageMaker to download the inputs to run a processing job. @LocalPath@ is an absolute path to the input data.
--
-- * 'psiS3DataType' - Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for the processing job. If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for the processing job.
--
-- * 'psiS3InputMode' - Whether to use @File@ or @Pipe@ input mode. In @File@ mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
processingS3Input ::
  -- | 'psiS3URI'
  Text ->
  -- | 'psiLocalPath'
  Text ->
  -- | 'psiS3DataType'
  ProcessingS3DataType ->
  -- | 'psiS3InputMode'
  ProcessingS3InputMode ->
  ProcessingS3Input
processingS3Input pS3URI_ pLocalPath_ pS3DataType_ pS3InputMode_ =
  ProcessingS3Input'
    { _psiS3DataDistributionType = Nothing,
      _psiS3CompressionType = Nothing,
      _psiS3URI = pS3URI_,
      _psiLocalPath = pLocalPath_,
      _psiS3DataType = pS3DataType_,
      _psiS3InputMode = pS3InputMode_
    }

-- | Whether the data stored in Amazon S3 is @FullyReplicated@ or @ShardedByS3Key@ .
psiS3DataDistributionType :: Lens' ProcessingS3Input (Maybe ProcessingS3DataDistributionType)
psiS3DataDistributionType = lens _psiS3DataDistributionType (\s a -> s {_psiS3DataDistributionType = a})

-- | Whether to use @Gzip@ compression for Amazon S3 storage.
psiS3CompressionType :: Lens' ProcessingS3Input (Maybe ProcessingS3CompressionType)
psiS3CompressionType = lens _psiS3CompressionType (\s a -> s {_psiS3CompressionType = a})

-- | The URI for the Amazon S3 storage where you want Amazon SageMaker to download the artifacts needed to run a processing job.
psiS3URI :: Lens' ProcessingS3Input Text
psiS3URI = lens _psiS3URI (\s a -> s {_psiS3URI = a})

-- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to download the inputs to run a processing job. @LocalPath@ is an absolute path to the input data.
psiLocalPath :: Lens' ProcessingS3Input Text
psiLocalPath = lens _psiLocalPath (\s a -> s {_psiLocalPath = a})

-- | Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for the processing job. If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for the processing job.
psiS3DataType :: Lens' ProcessingS3Input ProcessingS3DataType
psiS3DataType = lens _psiS3DataType (\s a -> s {_psiS3DataType = a})

-- | Whether to use @File@ or @Pipe@ input mode. In @File@ mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
psiS3InputMode :: Lens' ProcessingS3Input ProcessingS3InputMode
psiS3InputMode = lens _psiS3InputMode (\s a -> s {_psiS3InputMode = a})

instance FromJSON ProcessingS3Input where
  parseJSON =
    withObject
      "ProcessingS3Input"
      ( \x ->
          ProcessingS3Input'
            <$> (x .:? "S3DataDistributionType")
            <*> (x .:? "S3CompressionType")
            <*> (x .: "S3Uri")
            <*> (x .: "LocalPath")
            <*> (x .: "S3DataType")
            <*> (x .: "S3InputMode")
      )

instance Hashable ProcessingS3Input

instance NFData ProcessingS3Input

instance ToJSON ProcessingS3Input where
  toJSON ProcessingS3Input' {..} =
    object
      ( catMaybes
          [ ("S3DataDistributionType" .=) <$> _psiS3DataDistributionType,
            ("S3CompressionType" .=) <$> _psiS3CompressionType,
            Just ("S3Uri" .= _psiS3URI),
            Just ("LocalPath" .= _psiLocalPath),
            Just ("S3DataType" .= _psiS3DataType),
            Just ("S3InputMode" .= _psiS3InputMode)
          ]
      )
