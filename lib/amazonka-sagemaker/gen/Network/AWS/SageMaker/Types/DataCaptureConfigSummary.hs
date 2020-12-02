{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataCaptureConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCaptureConfigSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CaptureStatus

-- |
--
--
--
-- /See:/ 'dataCaptureConfigSummary' smart constructor.
data DataCaptureConfigSummary = DataCaptureConfigSummary'
  { _dccsEnableCapture ::
      !Bool,
    _dccsCaptureStatus :: !CaptureStatus,
    _dccsCurrentSamplingPercentage :: !Nat,
    _dccsDestinationS3URI :: !Text,
    _dccsKMSKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataCaptureConfigSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccsEnableCapture' -
--
-- * 'dccsCaptureStatus' -
--
-- * 'dccsCurrentSamplingPercentage' -
--
-- * 'dccsDestinationS3URI' -
--
-- * 'dccsKMSKeyId' -
dataCaptureConfigSummary ::
  -- | 'dccsEnableCapture'
  Bool ->
  -- | 'dccsCaptureStatus'
  CaptureStatus ->
  -- | 'dccsCurrentSamplingPercentage'
  Natural ->
  -- | 'dccsDestinationS3URI'
  Text ->
  -- | 'dccsKMSKeyId'
  Text ->
  DataCaptureConfigSummary
dataCaptureConfigSummary
  pEnableCapture_
  pCaptureStatus_
  pCurrentSamplingPercentage_
  pDestinationS3URI_
  pKMSKeyId_ =
    DataCaptureConfigSummary'
      { _dccsEnableCapture = pEnableCapture_,
        _dccsCaptureStatus = pCaptureStatus_,
        _dccsCurrentSamplingPercentage =
          _Nat # pCurrentSamplingPercentage_,
        _dccsDestinationS3URI = pDestinationS3URI_,
        _dccsKMSKeyId = pKMSKeyId_
      }

-- |
dccsEnableCapture :: Lens' DataCaptureConfigSummary Bool
dccsEnableCapture = lens _dccsEnableCapture (\s a -> s {_dccsEnableCapture = a})

-- |
dccsCaptureStatus :: Lens' DataCaptureConfigSummary CaptureStatus
dccsCaptureStatus = lens _dccsCaptureStatus (\s a -> s {_dccsCaptureStatus = a})

-- |
dccsCurrentSamplingPercentage :: Lens' DataCaptureConfigSummary Natural
dccsCurrentSamplingPercentage = lens _dccsCurrentSamplingPercentage (\s a -> s {_dccsCurrentSamplingPercentage = a}) . _Nat

-- |
dccsDestinationS3URI :: Lens' DataCaptureConfigSummary Text
dccsDestinationS3URI = lens _dccsDestinationS3URI (\s a -> s {_dccsDestinationS3URI = a})

-- |
dccsKMSKeyId :: Lens' DataCaptureConfigSummary Text
dccsKMSKeyId = lens _dccsKMSKeyId (\s a -> s {_dccsKMSKeyId = a})

instance FromJSON DataCaptureConfigSummary where
  parseJSON =
    withObject
      "DataCaptureConfigSummary"
      ( \x ->
          DataCaptureConfigSummary'
            <$> (x .: "EnableCapture")
            <*> (x .: "CaptureStatus")
            <*> (x .: "CurrentSamplingPercentage")
            <*> (x .: "DestinationS3Uri")
            <*> (x .: "KmsKeyId")
      )

instance Hashable DataCaptureConfigSummary

instance NFData DataCaptureConfigSummary
