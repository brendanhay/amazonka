{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingS3Output

-- | Describes the results of a processing job.
--
--
--
-- /See:/ 'processingOutput' smart constructor.
data ProcessingOutput = ProcessingOutput'
  { _poOutputName :: !Text,
    _poS3Output :: !ProcessingS3Output
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poOutputName' - The name for the processing job output.
--
-- * 'poS3Output' - Configuration for processing job outputs in Amazon S3.
processingOutput ::
  -- | 'poOutputName'
  Text ->
  -- | 'poS3Output'
  ProcessingS3Output ->
  ProcessingOutput
processingOutput pOutputName_ pS3Output_ =
  ProcessingOutput'
    { _poOutputName = pOutputName_,
      _poS3Output = pS3Output_
    }

-- | The name for the processing job output.
poOutputName :: Lens' ProcessingOutput Text
poOutputName = lens _poOutputName (\s a -> s {_poOutputName = a})

-- | Configuration for processing job outputs in Amazon S3.
poS3Output :: Lens' ProcessingOutput ProcessingS3Output
poS3Output = lens _poS3Output (\s a -> s {_poS3Output = a})

instance FromJSON ProcessingOutput where
  parseJSON =
    withObject
      "ProcessingOutput"
      ( \x ->
          ProcessingOutput' <$> (x .: "OutputName") <*> (x .: "S3Output")
      )

instance Hashable ProcessingOutput

instance NFData ProcessingOutput

instance ToJSON ProcessingOutput where
  toJSON ProcessingOutput' {..} =
    object
      ( catMaybes
          [ Just ("OutputName" .= _poOutputName),
            Just ("S3Output" .= _poS3Output)
          ]
      )
