{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingS3Input

-- | The inputs for a processing job.
--
--
--
-- /See:/ 'processingInput' smart constructor.
data ProcessingInput = ProcessingInput'
  { _piInputName :: !Text,
    _piS3Input :: !ProcessingS3Input
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piInputName' - The name of the inputs for the processing job.
--
-- * 'piS3Input' - The S3 inputs for the processing job.
processingInput ::
  -- | 'piInputName'
  Text ->
  -- | 'piS3Input'
  ProcessingS3Input ->
  ProcessingInput
processingInput pInputName_ pS3Input_ =
  ProcessingInput'
    { _piInputName = pInputName_,
      _piS3Input = pS3Input_
    }

-- | The name of the inputs for the processing job.
piInputName :: Lens' ProcessingInput Text
piInputName = lens _piInputName (\s a -> s {_piInputName = a})

-- | The S3 inputs for the processing job.
piS3Input :: Lens' ProcessingInput ProcessingS3Input
piS3Input = lens _piS3Input (\s a -> s {_piS3Input = a})

instance FromJSON ProcessingInput where
  parseJSON =
    withObject
      "ProcessingInput"
      ( \x ->
          ProcessingInput' <$> (x .: "InputName") <*> (x .: "S3Input")
      )

instance Hashable ProcessingInput

instance NFData ProcessingInput

instance ToJSON ProcessingInput where
  toJSON ProcessingInput' {..} =
    object
      ( catMaybes
          [ Just ("InputName" .= _piInputName),
            Just ("S3Input" .= _piS3Input)
          ]
      )
