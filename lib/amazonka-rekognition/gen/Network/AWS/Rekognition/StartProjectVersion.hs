{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the running of the version of a model. Starting a model takes a while to complete. To check the current state of the model, use 'DescribeProjectVersions' .
--
--
-- Once the model is running, you can detect custom labels in new images by calling 'DetectCustomLabels' .
--
-- This operation requires permissions to perform the @rekognition:StartProjectVersion@ action.
module Network.AWS.Rekognition.StartProjectVersion
  ( -- * Creating a Request
    startProjectVersion,
    StartProjectVersion,

    -- * Request Lenses
    spvProjectVersionARN,
    spvMinInferenceUnits,

    -- * Destructuring the Response
    startProjectVersionResponse,
    StartProjectVersionResponse,

    -- * Response Lenses
    spvrsStatus,
    spvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startProjectVersion' smart constructor.
data StartProjectVersion = StartProjectVersion'
  { _spvProjectVersionARN ::
      !Text,
    _spvMinInferenceUnits :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartProjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spvProjectVersionARN' - The Amazon Resource Name(ARN) of the model version that you want to start.
--
-- * 'spvMinInferenceUnits' - The minimum number of inference units to use. A single inference unit represents 1 hour of processing and can support up to 5 Transaction Pers Second (TPS). Use a higher number to increase the TPS throughput of your model. You are charged for the number of inference units that you use.
startProjectVersion ::
  -- | 'spvProjectVersionARN'
  Text ->
  -- | 'spvMinInferenceUnits'
  Natural ->
  StartProjectVersion
startProjectVersion pProjectVersionARN_ pMinInferenceUnits_ =
  StartProjectVersion'
    { _spvProjectVersionARN = pProjectVersionARN_,
      _spvMinInferenceUnits = _Nat # pMinInferenceUnits_
    }

-- | The Amazon Resource Name(ARN) of the model version that you want to start.
spvProjectVersionARN :: Lens' StartProjectVersion Text
spvProjectVersionARN = lens _spvProjectVersionARN (\s a -> s {_spvProjectVersionARN = a})

-- | The minimum number of inference units to use. A single inference unit represents 1 hour of processing and can support up to 5 Transaction Pers Second (TPS). Use a higher number to increase the TPS throughput of your model. You are charged for the number of inference units that you use.
spvMinInferenceUnits :: Lens' StartProjectVersion Natural
spvMinInferenceUnits = lens _spvMinInferenceUnits (\s a -> s {_spvMinInferenceUnits = a}) . _Nat

instance AWSRequest StartProjectVersion where
  type Rs StartProjectVersion = StartProjectVersionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          StartProjectVersionResponse'
            <$> (x .?> "Status") <*> (pure (fromEnum s))
      )

instance Hashable StartProjectVersion

instance NFData StartProjectVersion

instance ToHeaders StartProjectVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.StartProjectVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartProjectVersion where
  toJSON StartProjectVersion' {..} =
    object
      ( catMaybes
          [ Just ("ProjectVersionArn" .= _spvProjectVersionARN),
            Just ("MinInferenceUnits" .= _spvMinInferenceUnits)
          ]
      )

instance ToPath StartProjectVersion where
  toPath = const "/"

instance ToQuery StartProjectVersion where
  toQuery = const mempty

-- | /See:/ 'startProjectVersionResponse' smart constructor.
data StartProjectVersionResponse = StartProjectVersionResponse'
  { _spvrsStatus ::
      !(Maybe ProjectVersionStatus),
    _spvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartProjectVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spvrsStatus' - The current running status of the model.
--
-- * 'spvrsResponseStatus' - -- | The response status code.
startProjectVersionResponse ::
  -- | 'spvrsResponseStatus'
  Int ->
  StartProjectVersionResponse
startProjectVersionResponse pResponseStatus_ =
  StartProjectVersionResponse'
    { _spvrsStatus = Nothing,
      _spvrsResponseStatus = pResponseStatus_
    }

-- | The current running status of the model.
spvrsStatus :: Lens' StartProjectVersionResponse (Maybe ProjectVersionStatus)
spvrsStatus = lens _spvrsStatus (\s a -> s {_spvrsStatus = a})

-- | -- | The response status code.
spvrsResponseStatus :: Lens' StartProjectVersionResponse Int
spvrsResponseStatus = lens _spvrsResponseStatus (\s a -> s {_spvrsResponseStatus = a})

instance NFData StartProjectVersionResponse
