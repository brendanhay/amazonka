{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataCaptureConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCaptureConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CaptureContentTypeHeader
import Network.AWS.SageMaker.Types.CaptureOption

-- |
--
--
--
-- /See:/ 'dataCaptureConfig' smart constructor.
data DataCaptureConfig = DataCaptureConfig'
  { _dccCaptureContentTypeHeader ::
      !(Maybe CaptureContentTypeHeader),
    _dccKMSKeyId :: !(Maybe Text),
    _dccEnableCapture :: !(Maybe Bool),
    _dccInitialSamplingPercentage :: !Nat,
    _dccDestinationS3URI :: !Text,
    _dccCaptureOptions :: !(List1 CaptureOption)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataCaptureConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccCaptureContentTypeHeader' -
--
-- * 'dccKMSKeyId' -
--
-- * 'dccEnableCapture' -
--
-- * 'dccInitialSamplingPercentage' -
--
-- * 'dccDestinationS3URI' -
--
-- * 'dccCaptureOptions' -
dataCaptureConfig ::
  -- | 'dccInitialSamplingPercentage'
  Natural ->
  -- | 'dccDestinationS3URI'
  Text ->
  -- | 'dccCaptureOptions'
  NonEmpty CaptureOption ->
  DataCaptureConfig
dataCaptureConfig
  pInitialSamplingPercentage_
  pDestinationS3URI_
  pCaptureOptions_ =
    DataCaptureConfig'
      { _dccCaptureContentTypeHeader = Nothing,
        _dccKMSKeyId = Nothing,
        _dccEnableCapture = Nothing,
        _dccInitialSamplingPercentage = _Nat # pInitialSamplingPercentage_,
        _dccDestinationS3URI = pDestinationS3URI_,
        _dccCaptureOptions = _List1 # pCaptureOptions_
      }

-- |
dccCaptureContentTypeHeader :: Lens' DataCaptureConfig (Maybe CaptureContentTypeHeader)
dccCaptureContentTypeHeader = lens _dccCaptureContentTypeHeader (\s a -> s {_dccCaptureContentTypeHeader = a})

-- |
dccKMSKeyId :: Lens' DataCaptureConfig (Maybe Text)
dccKMSKeyId = lens _dccKMSKeyId (\s a -> s {_dccKMSKeyId = a})

-- |
dccEnableCapture :: Lens' DataCaptureConfig (Maybe Bool)
dccEnableCapture = lens _dccEnableCapture (\s a -> s {_dccEnableCapture = a})

-- |
dccInitialSamplingPercentage :: Lens' DataCaptureConfig Natural
dccInitialSamplingPercentage = lens _dccInitialSamplingPercentage (\s a -> s {_dccInitialSamplingPercentage = a}) . _Nat

-- |
dccDestinationS3URI :: Lens' DataCaptureConfig Text
dccDestinationS3URI = lens _dccDestinationS3URI (\s a -> s {_dccDestinationS3URI = a})

-- |
dccCaptureOptions :: Lens' DataCaptureConfig (NonEmpty CaptureOption)
dccCaptureOptions = lens _dccCaptureOptions (\s a -> s {_dccCaptureOptions = a}) . _List1

instance FromJSON DataCaptureConfig where
  parseJSON =
    withObject
      "DataCaptureConfig"
      ( \x ->
          DataCaptureConfig'
            <$> (x .:? "CaptureContentTypeHeader")
            <*> (x .:? "KmsKeyId")
            <*> (x .:? "EnableCapture")
            <*> (x .: "InitialSamplingPercentage")
            <*> (x .: "DestinationS3Uri")
            <*> (x .: "CaptureOptions")
      )

instance Hashable DataCaptureConfig

instance NFData DataCaptureConfig

instance ToJSON DataCaptureConfig where
  toJSON DataCaptureConfig' {..} =
    object
      ( catMaybes
          [ ("CaptureContentTypeHeader" .=) <$> _dccCaptureContentTypeHeader,
            ("KmsKeyId" .=) <$> _dccKMSKeyId,
            ("EnableCapture" .=) <$> _dccEnableCapture,
            Just
              ("InitialSamplingPercentage" .= _dccInitialSamplingPercentage),
            Just ("DestinationS3Uri" .= _dccDestinationS3URI),
            Just ("CaptureOptions" .= _dccCaptureOptions)
          ]
      )
