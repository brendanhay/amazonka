{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingOutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingOutput

-- | The output configuration for the processing job.
--
--
--
-- /See:/ 'processingOutputConfig' smart constructor.
data ProcessingOutputConfig = ProcessingOutputConfig'
  { _pocKMSKeyId ::
      !(Maybe Text),
    _pocOutputs :: ![ProcessingOutput]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
--
-- * 'pocOutputs' - Output configuration information for a processing job.
processingOutputConfig ::
  ProcessingOutputConfig
processingOutputConfig =
  ProcessingOutputConfig'
    { _pocKMSKeyId = Nothing,
      _pocOutputs = mempty
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
pocKMSKeyId :: Lens' ProcessingOutputConfig (Maybe Text)
pocKMSKeyId = lens _pocKMSKeyId (\s a -> s {_pocKMSKeyId = a})

-- | Output configuration information for a processing job.
pocOutputs :: Lens' ProcessingOutputConfig [ProcessingOutput]
pocOutputs = lens _pocOutputs (\s a -> s {_pocOutputs = a}) . _Coerce

instance FromJSON ProcessingOutputConfig where
  parseJSON =
    withObject
      "ProcessingOutputConfig"
      ( \x ->
          ProcessingOutputConfig'
            <$> (x .:? "KmsKeyId") <*> (x .:? "Outputs" .!= mempty)
      )

instance Hashable ProcessingOutputConfig

instance NFData ProcessingOutputConfig

instance ToJSON ProcessingOutputConfig where
  toJSON ProcessingOutputConfig' {..} =
    object
      ( catMaybes
          [ ("KmsKeyId" .=) <$> _pocKMSKeyId,
            Just ("Outputs" .= _pocOutputs)
          ]
      )
