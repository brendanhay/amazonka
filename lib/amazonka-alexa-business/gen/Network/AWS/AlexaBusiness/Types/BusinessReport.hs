{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReport where

import Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Usage report with specified parameters.
--
--
--
-- /See:/ 'businessReport' smart constructor.
data BusinessReport = BusinessReport'
  { _brStatus ::
      !(Maybe BusinessReportStatus),
    _brFailureCode :: !(Maybe BusinessReportFailureCode),
    _brDeliveryTime :: !(Maybe POSIX),
    _brDownloadURL :: !(Maybe Text),
    _brS3Location :: !(Maybe BusinessReportS3Location)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BusinessReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brStatus' - The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
--
-- * 'brFailureCode' - The failure code.
--
-- * 'brDeliveryTime' - The time of report delivery.
--
-- * 'brDownloadURL' - The download link where a user can download the report.
--
-- * 'brS3Location' - The S3 location of the output reports.
businessReport ::
  BusinessReport
businessReport =
  BusinessReport'
    { _brStatus = Nothing,
      _brFailureCode = Nothing,
      _brDeliveryTime = Nothing,
      _brDownloadURL = Nothing,
      _brS3Location = Nothing
    }

-- | The status of the report generation execution (RUNNING, SUCCEEDED, or FAILED).
brStatus :: Lens' BusinessReport (Maybe BusinessReportStatus)
brStatus = lens _brStatus (\s a -> s {_brStatus = a})

-- | The failure code.
brFailureCode :: Lens' BusinessReport (Maybe BusinessReportFailureCode)
brFailureCode = lens _brFailureCode (\s a -> s {_brFailureCode = a})

-- | The time of report delivery.
brDeliveryTime :: Lens' BusinessReport (Maybe UTCTime)
brDeliveryTime = lens _brDeliveryTime (\s a -> s {_brDeliveryTime = a}) . mapping _Time

-- | The download link where a user can download the report.
brDownloadURL :: Lens' BusinessReport (Maybe Text)
brDownloadURL = lens _brDownloadURL (\s a -> s {_brDownloadURL = a})

-- | The S3 location of the output reports.
brS3Location :: Lens' BusinessReport (Maybe BusinessReportS3Location)
brS3Location = lens _brS3Location (\s a -> s {_brS3Location = a})

instance FromJSON BusinessReport where
  parseJSON =
    withObject
      "BusinessReport"
      ( \x ->
          BusinessReport'
            <$> (x .:? "Status")
            <*> (x .:? "FailureCode")
            <*> (x .:? "DeliveryTime")
            <*> (x .:? "DownloadUrl")
            <*> (x .:? "S3Location")
      )

instance Hashable BusinessReport

instance NFData BusinessReport
