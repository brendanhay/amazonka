{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription where

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
import Network.AWS.ElasticBeanstalk.Types.S3Location
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of an application version.
--
--
--
-- /See:/ 'applicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { _avdStatus ::
      !( Maybe
           ApplicationVersionStatus
       ),
    _avdSourceBundle ::
      !(Maybe S3Location),
    _avdDateUpdated ::
      !(Maybe ISO8601),
    _avdDateCreated ::
      !(Maybe ISO8601),
    _avdVersionLabel ::
      !(Maybe Text),
    _avdSourceBuildInformation ::
      !(Maybe SourceBuildInformation),
    _avdApplicationName ::
      !(Maybe Text),
    _avdApplicationVersionARN ::
      !(Maybe Text),
    _avdBuildARN :: !(Maybe Text),
    _avdDescription ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdStatus' - The processing status of the application version. Reflects the state of the application version during its creation. Many of the values are only applicable if you specified @True@ for the @Process@ parameter of the @CreateApplicationVersion@ action. The following list describes the possible values.     * @Unprocessed@ – Application version wasn't pre-processed or validated. Elastic Beanstalk will validate configuration files during deployment of the application version to an environment.     * @Processing@ – Elastic Beanstalk is currently processing the application version.     * @Building@ – Application version is currently undergoing an AWS CodeBuild build.     * @Processed@ – Elastic Beanstalk was successfully pre-processed and validated.     * @Failed@ – Either the AWS CodeBuild build failed or configuration files didn't pass validation. This application version isn't usable.
--
-- * 'avdSourceBundle' - The storage location of the application version's source bundle in Amazon S3.
--
-- * 'avdDateUpdated' - The last modified date of the application version.
--
-- * 'avdDateCreated' - The creation date of the application version.
--
-- * 'avdVersionLabel' - A unique identifier for the application version.
--
-- * 'avdSourceBuildInformation' - If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
--
-- * 'avdApplicationName' - The name of the application to which the application version belongs.
--
-- * 'avdApplicationVersionARN' - The Amazon Resource Name (ARN) of the application version.
--
-- * 'avdBuildARN' - Reference to the artifact from the AWS CodeBuild build.
--
-- * 'avdDescription' - The description of the application version.
applicationVersionDescription ::
  ApplicationVersionDescription
applicationVersionDescription =
  ApplicationVersionDescription'
    { _avdStatus = Nothing,
      _avdSourceBundle = Nothing,
      _avdDateUpdated = Nothing,
      _avdDateCreated = Nothing,
      _avdVersionLabel = Nothing,
      _avdSourceBuildInformation = Nothing,
      _avdApplicationName = Nothing,
      _avdApplicationVersionARN = Nothing,
      _avdBuildARN = Nothing,
      _avdDescription = Nothing
    }

-- | The processing status of the application version. Reflects the state of the application version during its creation. Many of the values are only applicable if you specified @True@ for the @Process@ parameter of the @CreateApplicationVersion@ action. The following list describes the possible values.     * @Unprocessed@ – Application version wasn't pre-processed or validated. Elastic Beanstalk will validate configuration files during deployment of the application version to an environment.     * @Processing@ – Elastic Beanstalk is currently processing the application version.     * @Building@ – Application version is currently undergoing an AWS CodeBuild build.     * @Processed@ – Elastic Beanstalk was successfully pre-processed and validated.     * @Failed@ – Either the AWS CodeBuild build failed or configuration files didn't pass validation. This application version isn't usable.
avdStatus :: Lens' ApplicationVersionDescription (Maybe ApplicationVersionStatus)
avdStatus = lens _avdStatus (\s a -> s {_avdStatus = a})

-- | The storage location of the application version's source bundle in Amazon S3.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\s a -> s {_avdSourceBundle = a})

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateUpdated = lens _avdDateUpdated (\s a -> s {_avdDateUpdated = a}) . mapping _Time

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateCreated = lens _avdDateCreated (\s a -> s {_avdDateCreated = a}) . mapping _Time

-- | A unique identifier for the application version.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\s a -> s {_avdVersionLabel = a})

-- | If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
avdSourceBuildInformation :: Lens' ApplicationVersionDescription (Maybe SourceBuildInformation)
avdSourceBuildInformation = lens _avdSourceBuildInformation (\s a -> s {_avdSourceBuildInformation = a})

-- | The name of the application to which the application version belongs.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName = lens _avdApplicationName (\s a -> s {_avdApplicationName = a})

-- | The Amazon Resource Name (ARN) of the application version.
avdApplicationVersionARN :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationVersionARN = lens _avdApplicationVersionARN (\s a -> s {_avdApplicationVersionARN = a})

-- | Reference to the artifact from the AWS CodeBuild build.
avdBuildARN :: Lens' ApplicationVersionDescription (Maybe Text)
avdBuildARN = lens _avdBuildARN (\s a -> s {_avdBuildARN = a})

-- | The description of the application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\s a -> s {_avdDescription = a})

instance FromXML ApplicationVersionDescription where
  parseXML x =
    ApplicationVersionDescription'
      <$> (x .@? "Status")
      <*> (x .@? "SourceBundle")
      <*> (x .@? "DateUpdated")
      <*> (x .@? "DateCreated")
      <*> (x .@? "VersionLabel")
      <*> (x .@? "SourceBuildInformation")
      <*> (x .@? "ApplicationName")
      <*> (x .@? "ApplicationVersionArn")
      <*> (x .@? "BuildArn")
      <*> (x .@? "Description")

instance Hashable ApplicationVersionDescription

instance NFData ApplicationVersionDescription
