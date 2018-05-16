{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.Product where

import Network.AWS.Lens
import Network.AWS.MQ.Types.Sum
import Network.AWS.Prelude

-- | Returns information about all brokers.
--
-- /See:/ 'brokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { _biConsoleURL :: !(Maybe Text)
  , _biEndpoints  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BrokerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biConsoleURL' - The URL of the broker's ActiveMQ Web Console.
--
-- * 'biEndpoints' - The broker's wire-level protocol endpoints.
brokerInstance
    :: BrokerInstance
brokerInstance =
  BrokerInstance' {_biConsoleURL = Nothing, _biEndpoints = Nothing}


-- | The URL of the broker's ActiveMQ Web Console.
biConsoleURL :: Lens' BrokerInstance (Maybe Text)
biConsoleURL = lens _biConsoleURL (\ s a -> s{_biConsoleURL = a})

-- | The broker's wire-level protocol endpoints.
biEndpoints :: Lens' BrokerInstance [Text]
biEndpoints = lens _biEndpoints (\ s a -> s{_biEndpoints = a}) . _Default . _Coerce

instance FromJSON BrokerInstance where
        parseJSON
          = withObject "BrokerInstance"
              (\ x ->
                 BrokerInstance' <$>
                   (x .:? "consoleURL") <*>
                     (x .:? "endpoints" .!= mempty))

instance Hashable BrokerInstance where

instance NFData BrokerInstance where

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /See:/ 'brokerSummary' smart constructor.
data BrokerSummary = BrokerSummary'
  { _bsBrokerName       :: !(Maybe Text)
  , _bsBrokerState      :: !(Maybe BrokerState)
  , _bsDeploymentMode   :: !(Maybe DeploymentMode)
  , _bsBrokerId         :: !(Maybe Text)
  , _bsBrokerARN        :: !(Maybe Text)
  , _bsHostInstanceType :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BrokerSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsBrokerName' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- * 'bsBrokerState' - The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
--
-- * 'bsDeploymentMode' - Required. The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
--
-- * 'bsBrokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- * 'bsBrokerARN' - The Amazon Resource Name (ARN) of the broker.
--
-- * 'bsHostInstanceType' - The broker's instance type. Possible values: mq.t2.micro, mq.m4.large
brokerSummary
    :: BrokerSummary
brokerSummary =
  BrokerSummary'
    { _bsBrokerName = Nothing
    , _bsBrokerState = Nothing
    , _bsDeploymentMode = Nothing
    , _bsBrokerId = Nothing
    , _bsBrokerARN = Nothing
    , _bsHostInstanceType = Nothing
    }


-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
bsBrokerName :: Lens' BrokerSummary (Maybe Text)
bsBrokerName = lens _bsBrokerName (\ s a -> s{_bsBrokerName = a})

-- | The status of the broker. Possible values: CREATION_IN_PROGRESS, CREATION_FAILED, DELETION_IN_PROGRESS, RUNNING, REBOOT_IN_PROGRESS
bsBrokerState :: Lens' BrokerSummary (Maybe BrokerState)
bsBrokerState = lens _bsBrokerState (\ s a -> s{_bsBrokerState = a})

-- | Required. The deployment mode of the broker. Possible values: SINGLE_INSTANCE, ACTIVE_STANDBY_MULTI_AZ SINGLE_INSTANCE creates a single-instance broker in a single Availability Zone. ACTIVE_STANDBY_MULTI_AZ creates an active/standby broker for high availability.
bsDeploymentMode :: Lens' BrokerSummary (Maybe DeploymentMode)
bsDeploymentMode = lens _bsDeploymentMode (\ s a -> s{_bsDeploymentMode = a})

-- | The unique ID that Amazon MQ generates for the broker.
bsBrokerId :: Lens' BrokerSummary (Maybe Text)
bsBrokerId = lens _bsBrokerId (\ s a -> s{_bsBrokerId = a})

-- | The Amazon Resource Name (ARN) of the broker.
bsBrokerARN :: Lens' BrokerSummary (Maybe Text)
bsBrokerARN = lens _bsBrokerARN (\ s a -> s{_bsBrokerARN = a})

-- | The broker's instance type. Possible values: mq.t2.micro, mq.m4.large
bsHostInstanceType :: Lens' BrokerSummary (Maybe Text)
bsHostInstanceType = lens _bsHostInstanceType (\ s a -> s{_bsHostInstanceType = a})

instance FromJSON BrokerSummary where
        parseJSON
          = withObject "BrokerSummary"
              (\ x ->
                 BrokerSummary' <$>
                   (x .:? "brokerName") <*> (x .:? "brokerState") <*>
                     (x .:? "deploymentMode")
                     <*> (x .:? "brokerId")
                     <*> (x .:? "brokerArn")
                     <*> (x .:? "hostInstanceType"))

instance Hashable BrokerSummary where

instance NFData BrokerSummary where

-- | Returns information about all configurations.
--
-- /See:/ 'configuration' smart constructor.
data Configuration = Configuration'
  { _cEngineVersion  :: !(Maybe Text)
  , _cARN            :: !(Maybe Text)
  , _cLatestRevision :: !(Maybe ConfigurationRevision)
  , _cName           :: !(Maybe Text)
  , _cId             :: !(Maybe Text)
  , _cDescription    :: !(Maybe Text)
  , _cEngineType     :: !(Maybe EngineType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cEngineVersion' - Required. The version of the broker engine.
--
-- * 'cARN' - Required. The ARN of the configuration.
--
-- * 'cLatestRevision' - Required. The latest revision of the configuration.
--
-- * 'cName' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- * 'cId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'cDescription' - Required. The description of the configuration.
--
-- * 'cEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
configuration
    :: Configuration
configuration =
  Configuration'
    { _cEngineVersion = Nothing
    , _cARN = Nothing
    , _cLatestRevision = Nothing
    , _cName = Nothing
    , _cId = Nothing
    , _cDescription = Nothing
    , _cEngineType = Nothing
    }


-- | Required. The version of the broker engine.
cEngineVersion :: Lens' Configuration (Maybe Text)
cEngineVersion = lens _cEngineVersion (\ s a -> s{_cEngineVersion = a})

-- | Required. The ARN of the configuration.
cARN :: Lens' Configuration (Maybe Text)
cARN = lens _cARN (\ s a -> s{_cARN = a})

-- | Required. The latest revision of the configuration.
cLatestRevision :: Lens' Configuration (Maybe ConfigurationRevision)
cLatestRevision = lens _cLatestRevision (\ s a -> s{_cLatestRevision = a})

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
cName :: Lens' Configuration (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | Required. The unique ID that Amazon MQ generates for the configuration.
cId :: Lens' Configuration (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | Required. The description of the configuration.
cDescription :: Lens' Configuration (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
cEngineType :: Lens' Configuration (Maybe EngineType)
cEngineType = lens _cEngineType (\ s a -> s{_cEngineType = a})

instance FromJSON Configuration where
        parseJSON
          = withObject "Configuration"
              (\ x ->
                 Configuration' <$>
                   (x .:? "engineVersion") <*> (x .:? "arn") <*>
                     (x .:? "latestRevision")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "description")
                     <*> (x .:? "engineType"))

instance Hashable Configuration where

instance NFData Configuration where

-- | A list of information about the configuration.
--
-- /See:/ 'configurationId' smart constructor.
data ConfigurationId = ConfigurationId'
  { _ciId       :: !(Maybe Text)
  , _ciRevision :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'ciRevision' - The Universally Unique Identifier (UUID) of the request.
configurationId
    :: ConfigurationId
configurationId = ConfigurationId' {_ciId = Nothing, _ciRevision = Nothing}


-- | Required. The unique ID that Amazon MQ generates for the configuration.
ciId :: Lens' ConfigurationId (Maybe Text)
ciId = lens _ciId (\ s a -> s{_ciId = a})

-- | The Universally Unique Identifier (UUID) of the request.
ciRevision :: Lens' ConfigurationId (Maybe Int)
ciRevision = lens _ciRevision (\ s a -> s{_ciRevision = a})

instance FromJSON ConfigurationId where
        parseJSON
          = withObject "ConfigurationId"
              (\ x ->
                 ConfigurationId' <$>
                   (x .:? "id") <*> (x .:? "revision"))

instance Hashable ConfigurationId where

instance NFData ConfigurationId where

instance ToJSON ConfigurationId where
        toJSON ConfigurationId'{..}
          = object
              (catMaybes
                 [("id" .=) <$> _ciId,
                  ("revision" .=) <$> _ciRevision])

-- | Returns information about the specified configuration revision.
--
-- /See:/ 'configurationRevision' smart constructor.
data ConfigurationRevision = ConfigurationRevision'
  { _crRevision    :: !(Maybe Int)
  , _crDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crRevision' - Required. The revision of the configuration.
--
-- * 'crDescription' - The description of the configuration revision.
configurationRevision
    :: ConfigurationRevision
configurationRevision =
  ConfigurationRevision' {_crRevision = Nothing, _crDescription = Nothing}


-- | Required. The revision of the configuration.
crRevision :: Lens' ConfigurationRevision (Maybe Int)
crRevision = lens _crRevision (\ s a -> s{_crRevision = a})

-- | The description of the configuration revision.
crDescription :: Lens' ConfigurationRevision (Maybe Text)
crDescription = lens _crDescription (\ s a -> s{_crDescription = a})

instance FromJSON ConfigurationRevision where
        parseJSON
          = withObject "ConfigurationRevision"
              (\ x ->
                 ConfigurationRevision' <$>
                   (x .:? "revision") <*> (x .:? "description"))

instance Hashable ConfigurationRevision where

instance NFData ConfigurationRevision where

-- | Broker configuration information
--
-- /See:/ 'configurations' smart constructor.
data Configurations = Configurations'
  { _cPending :: !(Maybe ConfigurationId)
  , _cHistory :: !(Maybe [ConfigurationId])
  , _cCurrent :: !(Maybe ConfigurationId)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Configurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPending' - The pending configuration of the broker.
--
-- * 'cHistory' - The history of configurations applied to the broker.
--
-- * 'cCurrent' - The current configuration of the broker.
configurations
    :: Configurations
configurations =
  Configurations'
    {_cPending = Nothing, _cHistory = Nothing, _cCurrent = Nothing}


-- | The pending configuration of the broker.
cPending :: Lens' Configurations (Maybe ConfigurationId)
cPending = lens _cPending (\ s a -> s{_cPending = a})

-- | The history of configurations applied to the broker.
cHistory :: Lens' Configurations [ConfigurationId]
cHistory = lens _cHistory (\ s a -> s{_cHistory = a}) . _Default . _Coerce

-- | The current configuration of the broker.
cCurrent :: Lens' Configurations (Maybe ConfigurationId)
cCurrent = lens _cCurrent (\ s a -> s{_cCurrent = a})

instance FromJSON Configurations where
        parseJSON
          = withObject "Configurations"
              (\ x ->
                 Configurations' <$>
                   (x .:? "pending") <*> (x .:? "history" .!= mempty)
                     <*> (x .:? "current"))

instance Hashable Configurations where

instance NFData Configurations where

-- | Returns information about the XML element or attribute that was sanitized in the configuration.
--
-- /See:/ 'sanitizationWarning' smart constructor.
data SanitizationWarning = SanitizationWarning'
  { _swReason        :: !(Maybe SanitizationWarningReason)
  , _swAttributeName :: !(Maybe Text)
  , _swElementName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SanitizationWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swReason' - Required. The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.
--
-- * 'swAttributeName' - The name of the XML attribute that has been sanitized.
--
-- * 'swElementName' - The name of the XML element that has been sanitized.
sanitizationWarning
    :: SanitizationWarning
sanitizationWarning =
  SanitizationWarning'
    {_swReason = Nothing, _swAttributeName = Nothing, _swElementName = Nothing}


-- | Required. The reason for which the XML elements or attributes were sanitized. Possible values: DISALLOWED_ELEMENT_REMOVED, DISALLOWED_ATTRIBUTE_REMOVED, INVALID_ATTRIBUTE_VALUE_REMOVED DISALLOWED_ELEMENT_REMOVED shows that the provided element isn't allowed and has been removed. DISALLOWED_ATTRIBUTE_REMOVED shows that the provided attribute isn't allowed and has been removed. INVALID_ATTRIBUTE_VALUE_REMOVED shows that the provided value for the attribute isn't allowed and has been removed.
swReason :: Lens' SanitizationWarning (Maybe SanitizationWarningReason)
swReason = lens _swReason (\ s a -> s{_swReason = a})

-- | The name of the XML attribute that has been sanitized.
swAttributeName :: Lens' SanitizationWarning (Maybe Text)
swAttributeName = lens _swAttributeName (\ s a -> s{_swAttributeName = a})

-- | The name of the XML element that has been sanitized.
swElementName :: Lens' SanitizationWarning (Maybe Text)
swElementName = lens _swElementName (\ s a -> s{_swElementName = a})

instance FromJSON SanitizationWarning where
        parseJSON
          = withObject "SanitizationWarning"
              (\ x ->
                 SanitizationWarning' <$>
                   (x .:? "reason") <*> (x .:? "attributeName") <*>
                     (x .:? "elementName"))

instance Hashable SanitizationWarning where

instance NFData SanitizationWarning where

-- | An ActiveMQ user associated with the broker.
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uGroups        :: !(Maybe [Text])
  , _uConsoleAccess :: !(Maybe Bool)
  , _uUsername      :: !(Maybe Text)
  , _uPassword      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'uConsoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- * 'uUsername' - Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'uPassword' - Required. The password of the ActiveMQ user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
user
    :: User
user =
  User'
    { _uGroups = Nothing
    , _uConsoleAccess = Nothing
    , _uUsername = Nothing
    , _uPassword = Nothing
    }


-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
uGroups :: Lens' User [Text]
uGroups = lens _uGroups (\ s a -> s{_uGroups = a}) . _Default . _Coerce

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
uConsoleAccess :: Lens' User (Maybe Bool)
uConsoleAccess = lens _uConsoleAccess (\ s a -> s{_uConsoleAccess = a})

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
uUsername :: Lens' User (Maybe Text)
uUsername = lens _uUsername (\ s a -> s{_uUsername = a})

-- | Required. The password of the ActiveMQ user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
uPassword :: Lens' User (Maybe Text)
uPassword = lens _uPassword (\ s a -> s{_uPassword = a})

instance Hashable User where

instance NFData User where

instance ToJSON User where
        toJSON User'{..}
          = object
              (catMaybes
                 [("groups" .=) <$> _uGroups,
                  ("consoleAccess" .=) <$> _uConsoleAccess,
                  ("username" .=) <$> _uUsername,
                  ("password" .=) <$> _uPassword])

-- | Returns information about the status of the changes pending for the ActiveMQ user.
--
-- /See:/ 'userPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { _upcGroups        :: !(Maybe [Text])
  , _upcConsoleAccess :: !(Maybe Bool)
  , _upcPendingChange :: !(Maybe ChangeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserPendingChanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'upcConsoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- * 'upcPendingChange' - Required. The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
userPendingChanges
    :: UserPendingChanges
userPendingChanges =
  UserPendingChanges'
    { _upcGroups = Nothing
    , _upcConsoleAccess = Nothing
    , _upcPendingChange = Nothing
    }


-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
upcGroups :: Lens' UserPendingChanges [Text]
upcGroups = lens _upcGroups (\ s a -> s{_upcGroups = a}) . _Default . _Coerce

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
upcConsoleAccess :: Lens' UserPendingChanges (Maybe Bool)
upcConsoleAccess = lens _upcConsoleAccess (\ s a -> s{_upcConsoleAccess = a})

-- | Required. The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
upcPendingChange :: Lens' UserPendingChanges (Maybe ChangeType)
upcPendingChange = lens _upcPendingChange (\ s a -> s{_upcPendingChange = a})

instance FromJSON UserPendingChanges where
        parseJSON
          = withObject "UserPendingChanges"
              (\ x ->
                 UserPendingChanges' <$>
                   (x .:? "groups" .!= mempty) <*>
                     (x .:? "consoleAccess")
                     <*> (x .:? "pendingChange"))

instance Hashable UserPendingChanges where

instance NFData UserPendingChanges where

-- | Returns a list of all ActiveMQ users.
--
-- /See:/ 'userSummary' smart constructor.
data UserSummary = UserSummary'
  { _usUsername      :: !(Maybe Text)
  , _usPendingChange :: !(Maybe ChangeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usUsername' - Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'usPendingChange' - The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
userSummary
    :: UserSummary
userSummary = UserSummary' {_usUsername = Nothing, _usPendingChange = Nothing}


-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
usUsername :: Lens' UserSummary (Maybe Text)
usUsername = lens _usUsername (\ s a -> s{_usUsername = a})

-- | The type of change pending for the ActiveMQ user. Possible values: CREATE, UPDATE, DELETE
usPendingChange :: Lens' UserSummary (Maybe ChangeType)
usPendingChange = lens _usPendingChange (\ s a -> s{_usPendingChange = a})

instance FromJSON UserSummary where
        parseJSON
          = withObject "UserSummary"
              (\ x ->
                 UserSummary' <$>
                   (x .:? "username") <*> (x .:? "pendingChange"))

instance Hashable UserSummary where

instance NFData UserSummary where

-- | The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.
--
-- /See:/ 'weeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { _wstTimeOfDay :: !(Maybe Text)
  , _wstTimeZone  :: !(Maybe Text)
  , _wstDayOfWeek :: !(Maybe DayOfWeek)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WeeklyStartTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wstTimeOfDay' - Required. The time, in 24-hour format.
--
-- * 'wstTimeZone' - The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
--
-- * 'wstDayOfWeek' - Required. The day of the week. Possible values: MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
weeklyStartTime
    :: WeeklyStartTime
weeklyStartTime =
  WeeklyStartTime'
    {_wstTimeOfDay = Nothing, _wstTimeZone = Nothing, _wstDayOfWeek = Nothing}


-- | Required. The time, in 24-hour format.
wstTimeOfDay :: Lens' WeeklyStartTime (Maybe Text)
wstTimeOfDay = lens _wstTimeOfDay (\ s a -> s{_wstTimeOfDay = a})

-- | The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
wstTimeZone :: Lens' WeeklyStartTime (Maybe Text)
wstTimeZone = lens _wstTimeZone (\ s a -> s{_wstTimeZone = a})

-- | Required. The day of the week. Possible values: MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
wstDayOfWeek :: Lens' WeeklyStartTime (Maybe DayOfWeek)
wstDayOfWeek = lens _wstDayOfWeek (\ s a -> s{_wstDayOfWeek = a})

instance FromJSON WeeklyStartTime where
        parseJSON
          = withObject "WeeklyStartTime"
              (\ x ->
                 WeeklyStartTime' <$>
                   (x .:? "timeOfDay") <*> (x .:? "timeZone") <*>
                     (x .:? "dayOfWeek"))

instance Hashable WeeklyStartTime where

instance NFData WeeklyStartTime where

instance ToJSON WeeklyStartTime where
        toJSON WeeklyStartTime'{..}
          = object
              (catMaybes
                 [("timeOfDay" .=) <$> _wstTimeOfDay,
                  ("timeZone" .=) <$> _wstTimeZone,
                  ("dayOfWeek" .=) <$> _wstDayOfWeek])
