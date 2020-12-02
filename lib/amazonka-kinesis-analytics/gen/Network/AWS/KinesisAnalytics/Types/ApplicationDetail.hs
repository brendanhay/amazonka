{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationDetail where

import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.
--
--
--
-- /See:/ 'applicationDetail' smart constructor.
data ApplicationDetail = ApplicationDetail'
  { _adApplicationDescription ::
      !(Maybe Text),
    _adOutputDescriptions :: !(Maybe [OutputDescription]),
    _adCloudWatchLoggingOptionDescriptions ::
      !(Maybe [CloudWatchLoggingOptionDescription]),
    _adReferenceDataSourceDescriptions ::
      !(Maybe [ReferenceDataSourceDescription]),
    _adInputDescriptions :: !(Maybe [InputDescription]),
    _adApplicationCode :: !(Maybe Text),
    _adCreateTimestamp :: !(Maybe POSIX),
    _adLastUpdateTimestamp :: !(Maybe POSIX),
    _adApplicationName :: !Text,
    _adApplicationARN :: !Text,
    _adApplicationStatus :: !ApplicationStatus,
    _adApplicationVersionId :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adApplicationDescription' - Description of the application.
--
-- * 'adOutputDescriptions' - Describes the application output configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- * 'adCloudWatchLoggingOptionDescriptions' - Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
-- * 'adReferenceDataSourceDescriptions' - Describes reference data sources configured for the application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- * 'adInputDescriptions' - Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- * 'adApplicationCode' - Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
--
-- * 'adCreateTimestamp' - Time stamp when the application version was created.
--
-- * 'adLastUpdateTimestamp' - Time stamp when the application was last updated.
--
-- * 'adApplicationName' - Name of the application.
--
-- * 'adApplicationARN' - ARN of the application.
--
-- * 'adApplicationStatus' - Status of the application.
--
-- * 'adApplicationVersionId' - Provides the current application version.
applicationDetail ::
  -- | 'adApplicationName'
  Text ->
  -- | 'adApplicationARN'
  Text ->
  -- | 'adApplicationStatus'
  ApplicationStatus ->
  -- | 'adApplicationVersionId'
  Natural ->
  ApplicationDetail
applicationDetail
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_
  pApplicationVersionId_ =
    ApplicationDetail'
      { _adApplicationDescription = Nothing,
        _adOutputDescriptions = Nothing,
        _adCloudWatchLoggingOptionDescriptions = Nothing,
        _adReferenceDataSourceDescriptions = Nothing,
        _adInputDescriptions = Nothing,
        _adApplicationCode = Nothing,
        _adCreateTimestamp = Nothing,
        _adLastUpdateTimestamp = Nothing,
        _adApplicationName = pApplicationName_,
        _adApplicationARN = pApplicationARN_,
        _adApplicationStatus = pApplicationStatus_,
        _adApplicationVersionId = _Nat # pApplicationVersionId_
      }

-- | Description of the application.
adApplicationDescription :: Lens' ApplicationDetail (Maybe Text)
adApplicationDescription = lens _adApplicationDescription (\s a -> s {_adApplicationDescription = a})

-- | Describes the application output configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
adOutputDescriptions :: Lens' ApplicationDetail [OutputDescription]
adOutputDescriptions = lens _adOutputDescriptions (\s a -> s {_adOutputDescriptions = a}) . _Default . _Coerce

-- | Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
adCloudWatchLoggingOptionDescriptions :: Lens' ApplicationDetail [CloudWatchLoggingOptionDescription]
adCloudWatchLoggingOptionDescriptions = lens _adCloudWatchLoggingOptionDescriptions (\s a -> s {_adCloudWatchLoggingOptionDescriptions = a}) . _Default . _Coerce

-- | Describes reference data sources configured for the application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
adReferenceDataSourceDescriptions :: Lens' ApplicationDetail [ReferenceDataSourceDescription]
adReferenceDataSourceDescriptions = lens _adReferenceDataSourceDescriptions (\s a -> s {_adReferenceDataSourceDescriptions = a}) . _Default . _Coerce

-- | Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
adInputDescriptions :: Lens' ApplicationDetail [InputDescription]
adInputDescriptions = lens _adInputDescriptions (\s a -> s {_adInputDescriptions = a}) . _Default . _Coerce

-- | Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
adApplicationCode :: Lens' ApplicationDetail (Maybe Text)
adApplicationCode = lens _adApplicationCode (\s a -> s {_adApplicationCode = a})

-- | Time stamp when the application version was created.
adCreateTimestamp :: Lens' ApplicationDetail (Maybe UTCTime)
adCreateTimestamp = lens _adCreateTimestamp (\s a -> s {_adCreateTimestamp = a}) . mapping _Time

-- | Time stamp when the application was last updated.
adLastUpdateTimestamp :: Lens' ApplicationDetail (Maybe UTCTime)
adLastUpdateTimestamp = lens _adLastUpdateTimestamp (\s a -> s {_adLastUpdateTimestamp = a}) . mapping _Time

-- | Name of the application.
adApplicationName :: Lens' ApplicationDetail Text
adApplicationName = lens _adApplicationName (\s a -> s {_adApplicationName = a})

-- | ARN of the application.
adApplicationARN :: Lens' ApplicationDetail Text
adApplicationARN = lens _adApplicationARN (\s a -> s {_adApplicationARN = a})

-- | Status of the application.
adApplicationStatus :: Lens' ApplicationDetail ApplicationStatus
adApplicationStatus = lens _adApplicationStatus (\s a -> s {_adApplicationStatus = a})

-- | Provides the current application version.
adApplicationVersionId :: Lens' ApplicationDetail Natural
adApplicationVersionId = lens _adApplicationVersionId (\s a -> s {_adApplicationVersionId = a}) . _Nat

instance FromJSON ApplicationDetail where
  parseJSON =
    withObject
      "ApplicationDetail"
      ( \x ->
          ApplicationDetail'
            <$> (x .:? "ApplicationDescription")
            <*> (x .:? "OutputDescriptions" .!= mempty)
            <*> (x .:? "CloudWatchLoggingOptionDescriptions" .!= mempty)
            <*> (x .:? "ReferenceDataSourceDescriptions" .!= mempty)
            <*> (x .:? "InputDescriptions" .!= mempty)
            <*> (x .:? "ApplicationCode")
            <*> (x .:? "CreateTimestamp")
            <*> (x .:? "LastUpdateTimestamp")
            <*> (x .: "ApplicationName")
            <*> (x .: "ApplicationARN")
            <*> (x .: "ApplicationStatus")
            <*> (x .: "ApplicationVersionId")
      )

instance Hashable ApplicationDetail

instance NFData ApplicationDetail
