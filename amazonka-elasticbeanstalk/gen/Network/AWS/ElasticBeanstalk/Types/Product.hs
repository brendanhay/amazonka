{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Product where

import Network.AWS.ElasticBeanstalk.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of an application.
--
--
--
-- /See:/ 'applicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { _adApplicationARN          :: !(Maybe Text)
  , _adVersions                :: !(Maybe [Text])
  , _adDateUpdated             :: !(Maybe ISO8601)
  , _adDateCreated             :: !(Maybe ISO8601)
  , _adApplicationName         :: !(Maybe Text)
  , _adConfigurationTemplates  :: !(Maybe [Text])
  , _adResourceLifecycleConfig :: !(Maybe ApplicationResourceLifecycleConfig)
  , _adDescription             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adApplicationARN' - The Amazon Resource Name (ARN) of the application.
--
-- * 'adVersions' - The names of the versions for this application.
--
-- * 'adDateUpdated' - The date when the application was last modified.
--
-- * 'adDateCreated' - The date when the application was created.
--
-- * 'adApplicationName' - The name of the application.
--
-- * 'adConfigurationTemplates' - The names of the configuration templates associated with this application.
--
-- * 'adResourceLifecycleConfig' - The lifecycle settings for the application.
--
-- * 'adDescription' - User-defined description of the application.
applicationDescription
    :: ApplicationDescription
applicationDescription =
  ApplicationDescription'
    { _adApplicationARN = Nothing
    , _adVersions = Nothing
    , _adDateUpdated = Nothing
    , _adDateCreated = Nothing
    , _adApplicationName = Nothing
    , _adConfigurationTemplates = Nothing
    , _adResourceLifecycleConfig = Nothing
    , _adDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) of the application.
adApplicationARN :: Lens' ApplicationDescription (Maybe Text)
adApplicationARN = lens _adApplicationARN (\ s a -> s{_adApplicationARN = a})

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\ s a -> s{_adVersions = a}) . _Default . _Coerce

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\ s a -> s{_adDateUpdated = a}) . mapping _Time

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\ s a -> s{_adDateCreated = a}) . mapping _Time

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName = lens _adApplicationName (\ s a -> s{_adApplicationName = a})

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates = lens _adConfigurationTemplates (\ s a -> s{_adConfigurationTemplates = a}) . _Default . _Coerce

-- | The lifecycle settings for the application.
adResourceLifecycleConfig :: Lens' ApplicationDescription (Maybe ApplicationResourceLifecycleConfig)
adResourceLifecycleConfig = lens _adResourceLifecycleConfig (\ s a -> s{_adResourceLifecycleConfig = a})

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\ s a -> s{_adDescription = a})

instance FromXML ApplicationDescription where
        parseXML x
          = ApplicationDescription' <$>
              (x .@? "ApplicationArn") <*>
                (x .@? "Versions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "ApplicationName")
                <*>
                (x .@? "ConfigurationTemplates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "ResourceLifecycleConfig")
                <*> (x .@? "Description")

instance Hashable ApplicationDescription where

instance NFData ApplicationDescription where

-- | Result message containing a single description of an application.
--
--
--
-- /See:/ 'applicationDescriptionMessage' smart constructor.
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { _admApplication :: Maybe ApplicationDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admApplication' - The 'ApplicationDescription' of the application.
applicationDescriptionMessage
    :: ApplicationDescriptionMessage
applicationDescriptionMessage =
  ApplicationDescriptionMessage' {_admApplication = Nothing}


-- | The 'ApplicationDescription' of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\ s a -> s{_admApplication = a})

instance FromXML ApplicationDescriptionMessage where
        parseXML x
          = ApplicationDescriptionMessage' <$>
              (x .@? "Application")

instance Hashable ApplicationDescriptionMessage where

instance NFData ApplicationDescriptionMessage where

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
--
--
-- /See:/ 'applicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { _amRequestCount :: !(Maybe Int)
  , _amLatency      :: !(Maybe Latency)
  , _amStatusCodes  :: !(Maybe StatusCodes)
  , _amDuration     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amRequestCount' - Average number of requests handled by the web server per second over the last 10 seconds.
--
-- * 'amLatency' - Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
--
-- * 'amStatusCodes' - Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
--
-- * 'amDuration' - The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
applicationMetrics
    :: ApplicationMetrics
applicationMetrics =
  ApplicationMetrics'
    { _amRequestCount = Nothing
    , _amLatency = Nothing
    , _amStatusCodes = Nothing
    , _amDuration = Nothing
    }


-- | Average number of requests handled by the web server per second over the last 10 seconds.
amRequestCount :: Lens' ApplicationMetrics (Maybe Int)
amRequestCount = lens _amRequestCount (\ s a -> s{_amRequestCount = a})

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
amLatency :: Lens' ApplicationMetrics (Maybe Latency)
amLatency = lens _amLatency (\ s a -> s{_amLatency = a})

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
amStatusCodes :: Lens' ApplicationMetrics (Maybe StatusCodes)
amStatusCodes = lens _amStatusCodes (\ s a -> s{_amStatusCodes = a})

-- | The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
amDuration :: Lens' ApplicationMetrics (Maybe Int)
amDuration = lens _amDuration (\ s a -> s{_amDuration = a})

instance FromXML ApplicationMetrics where
        parseXML x
          = ApplicationMetrics' <$>
              (x .@? "RequestCount") <*> (x .@? "Latency") <*>
                (x .@? "StatusCodes")
                <*> (x .@? "Duration")

instance Hashable ApplicationMetrics where

instance NFData ApplicationMetrics where

-- | The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.
--
--
--
-- /See:/ 'applicationResourceLifecycleConfig' smart constructor.
data ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig'
  { _arlcVersionLifecycleConfig :: !(Maybe ApplicationVersionLifecycleConfig)
  , _arlcServiceRole            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationResourceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arlcVersionLifecycleConfig' - The application version lifecycle configuration.
--
-- * 'arlcServiceRole' - The ARN of an IAM service role that Elastic Beanstalk has permission to assume. The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
applicationResourceLifecycleConfig
    :: ApplicationResourceLifecycleConfig
applicationResourceLifecycleConfig =
  ApplicationResourceLifecycleConfig'
    {_arlcVersionLifecycleConfig = Nothing, _arlcServiceRole = Nothing}


-- | The application version lifecycle configuration.
arlcVersionLifecycleConfig :: Lens' ApplicationResourceLifecycleConfig (Maybe ApplicationVersionLifecycleConfig)
arlcVersionLifecycleConfig = lens _arlcVersionLifecycleConfig (\ s a -> s{_arlcVersionLifecycleConfig = a})

-- | The ARN of an IAM service role that Elastic Beanstalk has permission to assume. The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
arlcServiceRole :: Lens' ApplicationResourceLifecycleConfig (Maybe Text)
arlcServiceRole = lens _arlcServiceRole (\ s a -> s{_arlcServiceRole = a})

instance FromXML ApplicationResourceLifecycleConfig
         where
        parseXML x
          = ApplicationResourceLifecycleConfig' <$>
              (x .@? "VersionLifecycleConfig") <*>
                (x .@? "ServiceRole")

instance Hashable ApplicationResourceLifecycleConfig
         where

instance NFData ApplicationResourceLifecycleConfig
         where

instance ToQuery ApplicationResourceLifecycleConfig
         where
        toQuery ApplicationResourceLifecycleConfig'{..}
          = mconcat
              ["VersionLifecycleConfig" =:
                 _arlcVersionLifecycleConfig,
               "ServiceRole" =: _arlcServiceRole]

-- | Describes the properties of an application version.
--
--
--
-- /See:/ 'applicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { _avdStatus                 :: !(Maybe ApplicationVersionStatus)
  , _avdSourceBundle           :: !(Maybe S3Location)
  , _avdDateUpdated            :: !(Maybe ISO8601)
  , _avdDateCreated            :: !(Maybe ISO8601)
  , _avdVersionLabel           :: !(Maybe Text)
  , _avdSourceBuildInformation :: !(Maybe SourceBuildInformation)
  , _avdApplicationName        :: !(Maybe Text)
  , _avdApplicationVersionARN  :: !(Maybe Text)
  , _avdBuildARN               :: !(Maybe Text)
  , _avdDescription            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdStatus' - The processing status of the application version. Reflects the state of the application version during its creation. Many of the values are only applicable if you specified @True@ for the @Process@ parameter of the @CreateApplicationVersion@ action. The following list describes the possible values.     * @Unprocessed@
