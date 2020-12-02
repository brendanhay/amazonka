{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.Sum

-- | An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.
--
--
--
-- /See:/ 'activation' smart constructor.
data Activation = Activation'
  { _aExpired             :: !(Maybe Bool)
  , _aDefaultInstanceName :: !(Maybe Text)
  , _aActivationId        :: !(Maybe Text)
  , _aCreatedDate         :: !(Maybe POSIX)
  , _aRegistrationLimit   :: !(Maybe Nat)
  , _aExpirationDate      :: !(Maybe POSIX)
  , _aDescription         :: !(Maybe Text)
  , _aRegistrationsCount  :: !(Maybe Nat)
  , _aIAMRole             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Activation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aExpired' - Whether or not the activation is expired.
--
-- * 'aDefaultInstanceName' - A name for the managed instance when it is created.
--
-- * 'aActivationId' - The ID created by Systems Manager when you submitted the activation.
--
-- * 'aCreatedDate' - The date the activation was created.
--
-- * 'aRegistrationLimit' - The maximum number of managed instances that can be registered using this activation.
--
-- * 'aExpirationDate' - The date when this activation can no longer be used to register managed instances.
--
-- * 'aDescription' - A user defined description of the activation.
--
-- * 'aRegistrationsCount' - The number of managed instances already registered with this activation.
--
-- * 'aIAMRole' - The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
activation
    :: Activation
activation =
  Activation'
    { _aExpired = Nothing
    , _aDefaultInstanceName = Nothing
    , _aActivationId = Nothing
    , _aCreatedDate = Nothing
    , _aRegistrationLimit = Nothing
    , _aExpirationDate = Nothing
    , _aDescription = Nothing
    , _aRegistrationsCount = Nothing
    , _aIAMRole = Nothing
    }


-- | Whether or not the activation is expired.
aExpired :: Lens' Activation (Maybe Bool)
aExpired = lens _aExpired (\ s a -> s{_aExpired = a})

-- | A name for the managed instance when it is created.
aDefaultInstanceName :: Lens' Activation (Maybe Text)
aDefaultInstanceName = lens _aDefaultInstanceName (\ s a -> s{_aDefaultInstanceName = a})

-- | The ID created by Systems Manager when you submitted the activation.
aActivationId :: Lens' Activation (Maybe Text)
aActivationId = lens _aActivationId (\ s a -> s{_aActivationId = a})

-- | The date the activation was created.
aCreatedDate :: Lens' Activation (Maybe UTCTime)
aCreatedDate = lens _aCreatedDate (\ s a -> s{_aCreatedDate = a}) . mapping _Time

-- | The maximum number of managed instances that can be registered using this activation.
aRegistrationLimit :: Lens' Activation (Maybe Natural)
aRegistrationLimit = lens _aRegistrationLimit (\ s a -> s{_aRegistrationLimit = a}) . mapping _Nat

-- | The date when this activation can no longer be used to register managed instances.
aExpirationDate :: Lens' Activation (Maybe UTCTime)
aExpirationDate = lens _aExpirationDate (\ s a -> s{_aExpirationDate = a}) . mapping _Time

-- | A user defined description of the activation.
aDescription :: Lens' Activation (Maybe Text)
aDescription = lens _aDescription (\ s a -> s{_aDescription = a})

-- | The number of managed instances already registered with this activation.
aRegistrationsCount :: Lens' Activation (Maybe Natural)
aRegistrationsCount = lens _aRegistrationsCount (\ s a -> s{_aRegistrationsCount = a}) . mapping _Nat

-- | The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
aIAMRole :: Lens' Activation (Maybe Text)
aIAMRole = lens _aIAMRole (\ s a -> s{_aIAMRole = a})

instance FromJSON Activation where
        parseJSON
          = withObject "Activation"
              (\ x ->
                 Activation' <$>
                   (x .:? "Expired") <*> (x .:? "DefaultInstanceName")
                     <*> (x .:? "ActivationId")
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "RegistrationLimit")
                     <*> (x .:? "ExpirationDate")
                     <*> (x .:? "Description")
                     <*> (x .:? "RegistrationsCount")
                     <*> (x .:? "IamRole"))

instance Hashable Activation where

instance NFData Activation where

-- | Describes an association of a Systems Manager document and an instance.
--
--
--
-- /See:/ 'association' smart constructor.
data Association = Association'
  { _aAssociationId      :: !(Maybe Text)
  , _aInstanceId         :: !(Maybe Text)
  , _aOverview           :: !(Maybe AssociationOverview)
  , _aLastExecutionDate  :: !(Maybe POSIX)
  , _aScheduleExpression :: !(Maybe Text)
  , _aName               :: !(Maybe Text)
  , _aTargets            :: !(Maybe [Target])
  , _aDocumentVersion    :: !(Maybe Text)
  , _aAssociationVersion :: !(Maybe Text)
  , _aAssociationName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAssociationId' - The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
--
-- * 'aInstanceId' - The ID of the instance.
--
-- * 'aOverview' - Information about the association.
--
-- * 'aLastExecutionDate' - The date on which the association was last run.
--
-- * 'aScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'aName' - The name of the Systems Manager document.
--
-- * 'aTargets' - The instances targeted by the request to create an association.
--
-- * 'aDocumentVersion' - The version of the document used in the association.
--
-- * 'aAssociationVersion' - The association version.
--
-- * 'aAssociationName' - The association name.
association
    :: Association
association =
  Association'
    { _aAssociationId = Nothing
    , _aInstanceId = Nothing
    , _aOverview = Nothing
    , _aLastExecutionDate = Nothing
    , _aScheduleExpression = Nothing
    , _aName = Nothing
    , _aTargets = Nothing
    , _aDocumentVersion = Nothing
    , _aAssociationVersion = Nothing
    , _aAssociationName = Nothing
    }


-- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
aAssociationId :: Lens' Association (Maybe Text)
aAssociationId = lens _aAssociationId (\ s a -> s{_aAssociationId = a})

-- | The ID of the instance.
aInstanceId :: Lens' Association (Maybe Text)
aInstanceId = lens _aInstanceId (\ s a -> s{_aInstanceId = a})

-- | Information about the association.
aOverview :: Lens' Association (Maybe AssociationOverview)
aOverview = lens _aOverview (\ s a -> s{_aOverview = a})

-- | The date on which the association was last run.
aLastExecutionDate :: Lens' Association (Maybe UTCTime)
aLastExecutionDate = lens _aLastExecutionDate (\ s a -> s{_aLastExecutionDate = a}) . mapping _Time

-- | A cron expression that specifies a schedule when the association runs.
aScheduleExpression :: Lens' Association (Maybe Text)
aScheduleExpression = lens _aScheduleExpression (\ s a -> s{_aScheduleExpression = a})

-- | The name of the Systems Manager document.
aName :: Lens' Association (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | The instances targeted by the request to create an association.
aTargets :: Lens' Association [Target]
aTargets = lens _aTargets (\ s a -> s{_aTargets = a}) . _Default . _Coerce

-- | The version of the document used in the association.
aDocumentVersion :: Lens' Association (Maybe Text)
aDocumentVersion = lens _aDocumentVersion (\ s a -> s{_aDocumentVersion = a})

-- | The association version.
aAssociationVersion :: Lens' Association (Maybe Text)
aAssociationVersion = lens _aAssociationVersion (\ s a -> s{_aAssociationVersion = a})

-- | The association name.
aAssociationName :: Lens' Association (Maybe Text)
aAssociationName = lens _aAssociationName (\ s a -> s{_aAssociationName = a})

instance FromJSON Association where
        parseJSON
          = withObject "Association"
              (\ x ->
                 Association' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Overview")
                     <*> (x .:? "LastExecutionDate")
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AssociationVersion")
                     <*> (x .:? "AssociationName"))

instance Hashable Association where

instance NFData Association where

-- | Describes the parameters for a document.
--
--
--
-- /See:/ 'associationDescription' smart constructor.
data AssociationDescription = AssociationDescription'
  { _adAssociationId               :: !(Maybe Text)
  , _adInstanceId                  :: !(Maybe Text)
  , _adStatus                      :: !(Maybe AssociationStatus)
  , _adLastSuccessfulExecutionDate :: !(Maybe POSIX)
  , _adOverview                    :: !(Maybe AssociationOverview)
  , _adLastUpdateAssociationDate   :: !(Maybe POSIX)
  , _adDate                        :: !(Maybe POSIX)
  , _adLastExecutionDate           :: !(Maybe POSIX)
  , _adScheduleExpression          :: !(Maybe Text)
  , _adName                        :: !(Maybe Text)
  , _adOutputLocation              :: !(Maybe InstanceAssociationOutputLocation)
  , _adTargets                     :: !(Maybe [Target])
  , _adParameters                  :: !(Maybe (Map Text [Text]))
  , _adDocumentVersion             :: !(Maybe Text)
  , _adAssociationVersion          :: !(Maybe Text)
  , _adAssociationName             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAssociationId' - The association ID.
--
-- * 'adInstanceId' - The ID of the instance.
--
-- * 'adStatus' - The association status.
--
-- * 'adLastSuccessfulExecutionDate' - The last date on which the association was successfully run.
--
-- * 'adOverview' - Information about the association.
--
-- * 'adLastUpdateAssociationDate' - The date when the association was last updated.
--
-- * 'adDate' - The date when the association was made.
--
-- * 'adLastExecutionDate' - The date on which the association was last run.
--
-- * 'adScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'adName' - The name of the Systems Manager document.
--
-- * 'adOutputLocation' - An Amazon S3 bucket where you want to store the output details of the request.
--
-- * 'adTargets' - The instances targeted by the request.
--
-- * 'adParameters' - A description of the parameters for a document.
--
-- * 'adDocumentVersion' - The document version.
--
-- * 'adAssociationVersion' - The association version.
--
-- * 'adAssociationName' - The association name.
associationDescription
    :: AssociationDescription
associationDescription =
  AssociationDescription'
    { _adAssociationId = Nothing
    , _adInstanceId = Nothing
    , _adStatus = Nothing
    , _adLastSuccessfulExecutionDate = Nothing
    , _adOverview = Nothing
    , _adLastUpdateAssociationDate = Nothing
    , _adDate = Nothing
    , _adLastExecutionDate = Nothing
    , _adScheduleExpression = Nothing
    , _adName = Nothing
    , _adOutputLocation = Nothing
    , _adTargets = Nothing
    , _adParameters = Nothing
    , _adDocumentVersion = Nothing
    , _adAssociationVersion = Nothing
    , _adAssociationName = Nothing
    }


-- | The association ID.
adAssociationId :: Lens' AssociationDescription (Maybe Text)
adAssociationId = lens _adAssociationId (\ s a -> s{_adAssociationId = a})

-- | The ID of the instance.
adInstanceId :: Lens' AssociationDescription (Maybe Text)
adInstanceId = lens _adInstanceId (\ s a -> s{_adInstanceId = a})

-- | The association status.
adStatus :: Lens' AssociationDescription (Maybe AssociationStatus)
adStatus = lens _adStatus (\ s a -> s{_adStatus = a})

-- | The last date on which the association was successfully run.
adLastSuccessfulExecutionDate :: Lens' AssociationDescription (Maybe UTCTime)
adLastSuccessfulExecutionDate = lens _adLastSuccessfulExecutionDate (\ s a -> s{_adLastSuccessfulExecutionDate = a}) . mapping _Time

-- | Information about the association.
adOverview :: Lens' AssociationDescription (Maybe AssociationOverview)
adOverview = lens _adOverview (\ s a -> s{_adOverview = a})

-- | The date when the association was last updated.
adLastUpdateAssociationDate :: Lens' AssociationDescription (Maybe UTCTime)
adLastUpdateAssociationDate = lens _adLastUpdateAssociationDate (\ s a -> s{_adLastUpdateAssociationDate = a}) . mapping _Time

-- | The date when the association was made.
adDate :: Lens' AssociationDescription (Maybe UTCTime)
adDate = lens _adDate (\ s a -> s{_adDate = a}) . mapping _Time

-- | The date on which the association was last run.
adLastExecutionDate :: Lens' AssociationDescription (Maybe UTCTime)
adLastExecutionDate = lens _adLastExecutionDate (\ s a -> s{_adLastExecutionDate = a}) . mapping _Time

-- | A cron expression that specifies a schedule when the association runs.
adScheduleExpression :: Lens' AssociationDescription (Maybe Text)
adScheduleExpression = lens _adScheduleExpression (\ s a -> s{_adScheduleExpression = a})

-- | The name of the Systems Manager document.
adName :: Lens' AssociationDescription (Maybe Text)
adName = lens _adName (\ s a -> s{_adName = a})

-- | An Amazon S3 bucket where you want to store the output details of the request.
adOutputLocation :: Lens' AssociationDescription (Maybe InstanceAssociationOutputLocation)
adOutputLocation = lens _adOutputLocation (\ s a -> s{_adOutputLocation = a})

-- | The instances targeted by the request.
adTargets :: Lens' AssociationDescription [Target]
adTargets = lens _adTargets (\ s a -> s{_adTargets = a}) . _Default . _Coerce

-- | A description of the parameters for a document.
adParameters :: Lens' AssociationDescription (HashMap Text [Text])
adParameters = lens _adParameters (\ s a -> s{_adParameters = a}) . _Default . _Map

-- | The document version.
adDocumentVersion :: Lens' AssociationDescription (Maybe Text)
adDocumentVersion = lens _adDocumentVersion (\ s a -> s{_adDocumentVersion = a})

-- | The association version.
adAssociationVersion :: Lens' AssociationDescription (Maybe Text)
adAssociationVersion = lens _adAssociationVersion (\ s a -> s{_adAssociationVersion = a})

-- | The association name.
adAssociationName :: Lens' AssociationDescription (Maybe Text)
adAssociationName = lens _adAssociationName (\ s a -> s{_adAssociationName = a})

instance FromJSON AssociationDescription where
        parseJSON
          = withObject "AssociationDescription"
              (\ x ->
                 AssociationDescription' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Status")
                     <*> (x .:? "LastSuccessfulExecutionDate")
                     <*> (x .:? "Overview")
                     <*> (x .:? "LastUpdateAssociationDate")
                     <*> (x .:? "Date")
                     <*> (x .:? "LastExecutionDate")
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "OutputLocation")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AssociationVersion")
                     <*> (x .:? "AssociationName"))

instance Hashable AssociationDescription where

instance NFData AssociationDescription where

-- | Describes a filter.
--
--
--
-- /See:/ 'associationFilter' smart constructor.
data AssociationFilter = AssociationFilter'
  { _afKey   :: !AssociationFilterKey
  , _afValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afKey' - The name of the filter.
--
-- * 'afValue' - The filter value.
associationFilter
    :: AssociationFilterKey -- ^ 'afKey'
    -> Text -- ^ 'afValue'
    -> AssociationFilter
associationFilter pKey_ pValue_ =
  AssociationFilter' {_afKey = pKey_, _afValue = pValue_}


-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\ s a -> s{_afKey = a})

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\ s a -> s{_afValue = a})

instance Hashable AssociationFilter where

instance NFData AssociationFilter where

instance ToJSON AssociationFilter where
        toJSON AssociationFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _afKey), Just ("value" .= _afValue)])

-- | Information about the association.
--
--
--
-- /See:/ 'associationOverview' smart constructor.
data AssociationOverview = AssociationOverview'
  { _aoDetailedStatus                   :: !(Maybe Text)
  , _aoStatus                           :: !(Maybe Text)
  , _aoAssociationStatusAggregatedCount :: !(Maybe (Map Text Int))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociationOverview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoDetailedStatus' - A detailed status of the association.
--
-- * 'aoStatus' - The status of the association. Status can be: Pending, Success, or Failed.
--
-- * 'aoAssociationStatusAggregatedCount' - Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
associationOverview
    :: AssociationOverview
associationOverview =
  AssociationOverview'
    { _aoDetailedStatus = Nothing
    , _aoStatus = Nothing
    , _aoAssociationStatusAggregatedCount = Nothing
    }


-- | A detailed status of the association.
aoDetailedStatus :: Lens' AssociationOverview (Maybe Text)
aoDetailedStatus = lens _aoDetailedStatus (\ s a -> s{_aoDetailedStatus = a})

-- | The status of the association. Status can be: Pending, Success, or Failed.
aoStatus :: Lens' AssociationOverview (Maybe Text)
aoStatus = lens _aoStatus (\ s a -> s{_aoStatus = a})

-- | Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
aoAssociationStatusAggregatedCount :: Lens' AssociationOverview (HashMap Text Int)
aoAssociationStatusAggregatedCount = lens _aoAssociationStatusAggregatedCount (\ s a -> s{_aoAssociationStatusAggregatedCount = a}) . _Default . _Map

instance FromJSON AssociationOverview where
        parseJSON
          = withObject "AssociationOverview"
              (\ x ->
                 AssociationOverview' <$>
                   (x .:? "DetailedStatus") <*> (x .:? "Status") <*>
                     (x .:? "AssociationStatusAggregatedCount" .!=
                        mempty))

instance Hashable AssociationOverview where

instance NFData AssociationOverview where

-- | Describes an association status.
--
--
--
-- /See:/ 'associationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { _asAdditionalInfo :: !(Maybe Text)
  , _asDate           :: !POSIX
  , _asName           :: !AssociationStatusName
  , _asMessage        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAdditionalInfo' - A user-defined string.
--
-- * 'asDate' - The date when the status changed.
--
-- * 'asName' - The status.
--
-- * 'asMessage' - The reason for the status.
associationStatus
    :: UTCTime -- ^ 'asDate'
    -> AssociationStatusName -- ^ 'asName'
    -> Text -- ^ 'asMessage'
    -> AssociationStatus
associationStatus pDate_ pName_ pMessage_ =
  AssociationStatus'
    { _asAdditionalInfo = Nothing
    , _asDate = _Time # pDate_
    , _asName = pName_
    , _asMessage = pMessage_
    }


-- | A user-defined string.
asAdditionalInfo :: Lens' AssociationStatus (Maybe Text)
asAdditionalInfo = lens _asAdditionalInfo (\ s a -> s{_asAdditionalInfo = a})

-- | The date when the status changed.
asDate :: Lens' AssociationStatus UTCTime
asDate = lens _asDate (\ s a -> s{_asDate = a}) . _Time

-- | The status.
asName :: Lens' AssociationStatus AssociationStatusName
asName = lens _asName (\ s a -> s{_asName = a})

-- | The reason for the status.
asMessage :: Lens' AssociationStatus Text
asMessage = lens _asMessage (\ s a -> s{_asMessage = a})

instance FromJSON AssociationStatus where
        parseJSON
          = withObject "AssociationStatus"
              (\ x ->
                 AssociationStatus' <$>
                   (x .:? "AdditionalInfo") <*> (x .: "Date") <*>
                     (x .: "Name")
                     <*> (x .: "Message"))

instance Hashable AssociationStatus where

instance NFData AssociationStatus where

instance ToJSON AssociationStatus where
        toJSON AssociationStatus'{..}
          = object
              (catMaybes
                 [("AdditionalInfo" .=) <$> _asAdditionalInfo,
                  Just ("Date" .= _asDate), Just ("Name" .= _asName),
                  Just ("Message" .= _asMessage)])

-- | Information about the association version.
--
--
--
-- /See:/ 'associationVersionInfo' smart constructor.
data AssociationVersionInfo = AssociationVersionInfo'
  { _aviAssociationId      :: !(Maybe Text)
  , _aviCreatedDate        :: !(Maybe POSIX)
  , _aviScheduleExpression :: !(Maybe Text)
  , _aviName               :: !(Maybe Text)
  , _aviOutputLocation     :: !(Maybe InstanceAssociationOutputLocation)
  , _aviTargets            :: !(Maybe [Target])
  , _aviParameters         :: !(Maybe (Map Text [Text]))
  , _aviDocumentVersion    :: !(Maybe Text)
  , _aviAssociationVersion :: !(Maybe Text)
  , _aviAssociationName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociationVersionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aviAssociationId' - The ID created by the system when the association was created.
--
-- * 'aviCreatedDate' - The date the association version was created.
--
-- * 'aviScheduleExpression' - The cron or rate schedule specified for the association when the association version was created.
--
-- * 'aviName' - The name specified when the association was created.
--
-- * 'aviOutputLocation' - The location in Amazon S3 specified for the association when the association version was created.
--
-- * 'aviTargets' - The targets specified for the association when the association version was created.
--
-- * 'aviParameters' - Parameters specified when the association version was created.
--
-- * 'aviDocumentVersion' - The version of a Systems Manager document used when the association version was created.
--
-- * 'aviAssociationVersion' - The association version.
--
-- * 'aviAssociationName' - The name specified for the association version when the association version was created.
associationVersionInfo
    :: AssociationVersionInfo
associationVersionInfo =
  AssociationVersionInfo'
    { _aviAssociationId = Nothing
    , _aviCreatedDate = Nothing
    , _aviScheduleExpression = Nothing
    , _aviName = Nothing
    , _aviOutputLocation = Nothing
    , _aviTargets = Nothing
    , _aviParameters = Nothing
    , _aviDocumentVersion = Nothing
    , _aviAssociationVersion = Nothing
    , _aviAssociationName = Nothing
    }


-- | The ID created by the system when the association was created.
aviAssociationId :: Lens' AssociationVersionInfo (Maybe Text)
aviAssociationId = lens _aviAssociationId (\ s a -> s{_aviAssociationId = a})

-- | The date the association version was created.
aviCreatedDate :: Lens' AssociationVersionInfo (Maybe UTCTime)
aviCreatedDate = lens _aviCreatedDate (\ s a -> s{_aviCreatedDate = a}) . mapping _Time

-- | The cron or rate schedule specified for the association when the association version was created.
aviScheduleExpression :: Lens' AssociationVersionInfo (Maybe Text)
aviScheduleExpression = lens _aviScheduleExpression (\ s a -> s{_aviScheduleExpression = a})

-- | The name specified when the association was created.
aviName :: Lens' AssociationVersionInfo (Maybe Text)
aviName = lens _aviName (\ s a -> s{_aviName = a})

-- | The location in Amazon S3 specified for the association when the association version was created.
aviOutputLocation :: Lens' AssociationVersionInfo (Maybe InstanceAssociationOutputLocation)
aviOutputLocation = lens _aviOutputLocation (\ s a -> s{_aviOutputLocation = a})

-- | The targets specified for the association when the association version was created.
aviTargets :: Lens' AssociationVersionInfo [Target]
aviTargets = lens _aviTargets (\ s a -> s{_aviTargets = a}) . _Default . _Coerce

-- | Parameters specified when the association version was created.
aviParameters :: Lens' AssociationVersionInfo (HashMap Text [Text])
aviParameters = lens _aviParameters (\ s a -> s{_aviParameters = a}) . _Default . _Map

-- | The version of a Systems Manager document used when the association version was created.
aviDocumentVersion :: Lens' AssociationVersionInfo (Maybe Text)
aviDocumentVersion = lens _aviDocumentVersion (\ s a -> s{_aviDocumentVersion = a})

-- | The association version.
aviAssociationVersion :: Lens' AssociationVersionInfo (Maybe Text)
aviAssociationVersion = lens _aviAssociationVersion (\ s a -> s{_aviAssociationVersion = a})

-- | The name specified for the association version when the association version was created.
aviAssociationName :: Lens' AssociationVersionInfo (Maybe Text)
aviAssociationName = lens _aviAssociationName (\ s a -> s{_aviAssociationName = a})

instance FromJSON AssociationVersionInfo where
        parseJSON
          = withObject "AssociationVersionInfo"
              (\ x ->
                 AssociationVersionInfo' <$>
                   (x .:? "AssociationId") <*> (x .:? "CreatedDate") <*>
                     (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "OutputLocation")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AssociationVersion")
                     <*> (x .:? "AssociationName"))

instance Hashable AssociationVersionInfo where

instance NFData AssociationVersionInfo where

-- | Detailed information about the current state of an individual Automation execution.
--
--
--
-- /See:/ 'automationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { _aeCurrentStepName             :: !(Maybe Text)
  , _aeTargetParameterName         :: !(Maybe Text)
  , _aeExecutedBy                  :: !(Maybe Text)
  , _aeDocumentName                :: !(Maybe Text)
  , _aeExecutionEndTime            :: !(Maybe POSIX)
  , _aeFailureMessage              :: !(Maybe Text)
  , _aeMode                        :: !(Maybe ExecutionMode)
  , _aeStepExecutionsTruncated     :: !(Maybe Bool)
  , _aeAutomationExecutionStatus   :: !(Maybe AutomationExecutionStatus)
  , _aeParentAutomationExecutionId :: !(Maybe Text)
  , _aeOutputs                     :: !(Maybe (Map Text [Text]))
  , _aeMaxErrors                   :: !(Maybe Text)
  , _aeExecutionStartTime          :: !(Maybe POSIX)
  , _aeCurrentAction               :: !(Maybe Text)
  , _aeTargets                     :: !(Maybe [Target])
  , _aeResolvedTargets             :: !(Maybe ResolvedTargets)
  , _aeParameters                  :: !(Maybe (Map Text [Text]))
  , _aeDocumentVersion             :: !(Maybe Text)
  , _aeAutomationExecutionId       :: !(Maybe Text)
  , _aeStepExecutions              :: !(Maybe [StepExecution])
  , _aeMaxConcurrency              :: !(Maybe Text)
  , _aeTarget                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutomationExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeCurrentStepName' - The name of the currently executing step.
--
-- * 'aeTargetParameterName' - The parameter name.
--
-- * 'aeExecutedBy' - The Amazon Resource Name (ARN) of the user who executed the automation.
--
-- * 'aeDocumentName' - The name of the Automation document used during the execution.
--
-- * 'aeExecutionEndTime' - The time the execution finished.
--
-- * 'aeFailureMessage' - A message describing why an execution has failed, if the status is set to Failed.
--
-- * 'aeMode' - The automation execution mode.
--
-- * 'aeStepExecutionsTruncated' - A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
--
-- * 'aeAutomationExecutionStatus' - The execution status of the Automation.
--
-- * 'aeParentAutomationExecutionId' - The AutomationExecutionId of the parent automation.
--
-- * 'aeOutputs' - The list of execution outputs as defined in the automation document.
--
-- * 'aeMaxErrors' - The MaxErrors value specified by the user when the execution started.
--
-- * 'aeExecutionStartTime' - The time the execution started.
--
-- * 'aeCurrentAction' - The action of the currently executing step.
--
-- * 'aeTargets' - The specified targets.
--
-- * 'aeResolvedTargets' - A list of resolved targets in the rate control execution.
--
-- * 'aeParameters' - The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
--
-- * 'aeDocumentVersion' - The version of the document to use during execution.
--
-- * 'aeAutomationExecutionId' - The execution ID.
--
-- * 'aeStepExecutions' - A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are executed in order.
--
-- * 'aeMaxConcurrency' - The MaxConcurrency value specified by the user when the execution started.
--
-- * 'aeTarget' - The target of the execution.
automationExecution
    :: AutomationExecution
automationExecution =
  AutomationExecution'
    { _aeCurrentStepName = Nothing
    , _aeTargetParameterName = Nothing
    , _aeExecutedBy = Nothing
    , _aeDocumentName = Nothing
    , _aeExecutionEndTime = Nothing
    , _aeFailureMessage = Nothing
    , _aeMode = Nothing
    , _aeStepExecutionsTruncated = Nothing
    , _aeAutomationExecutionStatus = Nothing
    , _aeParentAutomationExecutionId = Nothing
    , _aeOutputs = Nothing
    , _aeMaxErrors = Nothing
    , _aeExecutionStartTime = Nothing
    , _aeCurrentAction = Nothing
    , _aeTargets = Nothing
    , _aeResolvedTargets = Nothing
    , _aeParameters = Nothing
    , _aeDocumentVersion = Nothing
    , _aeAutomationExecutionId = Nothing
    , _aeStepExecutions = Nothing
    , _aeMaxConcurrency = Nothing
    , _aeTarget = Nothing
    }


-- | The name of the currently executing step.
aeCurrentStepName :: Lens' AutomationExecution (Maybe Text)
aeCurrentStepName = lens _aeCurrentStepName (\ s a -> s{_aeCurrentStepName = a})

-- | The parameter name.
aeTargetParameterName :: Lens' AutomationExecution (Maybe Text)
aeTargetParameterName = lens _aeTargetParameterName (\ s a -> s{_aeTargetParameterName = a})

-- | The Amazon Resource Name (ARN) of the user who executed the automation.
aeExecutedBy :: Lens' AutomationExecution (Maybe Text)
aeExecutedBy = lens _aeExecutedBy (\ s a -> s{_aeExecutedBy = a})

-- | The name of the Automation document used during the execution.
aeDocumentName :: Lens' AutomationExecution (Maybe Text)
aeDocumentName = lens _aeDocumentName (\ s a -> s{_aeDocumentName = a})

-- | The time the execution finished.
aeExecutionEndTime :: Lens' AutomationExecution (Maybe UTCTime)
aeExecutionEndTime = lens _aeExecutionEndTime (\ s a -> s{_aeExecutionEndTime = a}) . mapping _Time

-- | A message describing why an execution has failed, if the status is set to Failed.
aeFailureMessage :: Lens' AutomationExecution (Maybe Text)
aeFailureMessage = lens _aeFailureMessage (\ s a -> s{_aeFailureMessage = a})

-- | The automation execution mode.
aeMode :: Lens' AutomationExecution (Maybe ExecutionMode)
aeMode = lens _aeMode (\ s a -> s{_aeMode = a})

-- | A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
aeStepExecutionsTruncated :: Lens' AutomationExecution (Maybe Bool)
aeStepExecutionsTruncated = lens _aeStepExecutionsTruncated (\ s a -> s{_aeStepExecutionsTruncated = a})

-- | The execution status of the Automation.
aeAutomationExecutionStatus :: Lens' AutomationExecution (Maybe AutomationExecutionStatus)
aeAutomationExecutionStatus = lens _aeAutomationExecutionStatus (\ s a -> s{_aeAutomationExecutionStatus = a})

-- | The AutomationExecutionId of the parent automation.
aeParentAutomationExecutionId :: Lens' AutomationExecution (Maybe Text)
aeParentAutomationExecutionId = lens _aeParentAutomationExecutionId (\ s a -> s{_aeParentAutomationExecutionId = a})

-- | The list of execution outputs as defined in the automation document.
aeOutputs :: Lens' AutomationExecution (HashMap Text [Text])
aeOutputs = lens _aeOutputs (\ s a -> s{_aeOutputs = a}) . _Default . _Map

-- | The MaxErrors value specified by the user when the execution started.
aeMaxErrors :: Lens' AutomationExecution (Maybe Text)
aeMaxErrors = lens _aeMaxErrors (\ s a -> s{_aeMaxErrors = a})

-- | The time the execution started.
aeExecutionStartTime :: Lens' AutomationExecution (Maybe UTCTime)
aeExecutionStartTime = lens _aeExecutionStartTime (\ s a -> s{_aeExecutionStartTime = a}) . mapping _Time

-- | The action of the currently executing step.
aeCurrentAction :: Lens' AutomationExecution (Maybe Text)
aeCurrentAction = lens _aeCurrentAction (\ s a -> s{_aeCurrentAction = a})

-- | The specified targets.
aeTargets :: Lens' AutomationExecution [Target]
aeTargets = lens _aeTargets (\ s a -> s{_aeTargets = a}) . _Default . _Coerce

-- | A list of resolved targets in the rate control execution.
aeResolvedTargets :: Lens' AutomationExecution (Maybe ResolvedTargets)
aeResolvedTargets = lens _aeResolvedTargets (\ s a -> s{_aeResolvedTargets = a})

-- | The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
aeParameters :: Lens' AutomationExecution (HashMap Text [Text])
aeParameters = lens _aeParameters (\ s a -> s{_aeParameters = a}) . _Default . _Map

-- | The version of the document to use during execution.
aeDocumentVersion :: Lens' AutomationExecution (Maybe Text)
aeDocumentVersion = lens _aeDocumentVersion (\ s a -> s{_aeDocumentVersion = a})

-- | The execution ID.
aeAutomationExecutionId :: Lens' AutomationExecution (Maybe Text)
aeAutomationExecutionId = lens _aeAutomationExecutionId (\ s a -> s{_aeAutomationExecutionId = a})

-- | A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are executed in order.
aeStepExecutions :: Lens' AutomationExecution [StepExecution]
aeStepExecutions = lens _aeStepExecutions (\ s a -> s{_aeStepExecutions = a}) . _Default . _Coerce

-- | The MaxConcurrency value specified by the user when the execution started.
aeMaxConcurrency :: Lens' AutomationExecution (Maybe Text)
aeMaxConcurrency = lens _aeMaxConcurrency (\ s a -> s{_aeMaxConcurrency = a})

-- | The target of the execution.
aeTarget :: Lens' AutomationExecution (Maybe Text)
aeTarget = lens _aeTarget (\ s a -> s{_aeTarget = a})

instance FromJSON AutomationExecution where
        parseJSON
          = withObject "AutomationExecution"
              (\ x ->
                 AutomationExecution' <$>
                   (x .:? "CurrentStepName") <*>
                     (x .:? "TargetParameterName")
                     <*> (x .:? "ExecutedBy")
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "ExecutionEndTime")
                     <*> (x .:? "FailureMessage")
                     <*> (x .:? "Mode")
                     <*> (x .:? "StepExecutionsTruncated")
                     <*> (x .:? "AutomationExecutionStatus")
                     <*> (x .:? "ParentAutomationExecutionId")
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "MaxErrors")
                     <*> (x .:? "ExecutionStartTime")
                     <*> (x .:? "CurrentAction")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "ResolvedTargets")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AutomationExecutionId")
                     <*> (x .:? "StepExecutions" .!= mempty)
                     <*> (x .:? "MaxConcurrency")
                     <*> (x .:? "Target"))

instance Hashable AutomationExecution where

instance NFData AutomationExecution where

-- | A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.
--
--
--
-- /See:/ 'automationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { _aefKey    :: !AutomationExecutionFilterKey
  , _aefValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutomationExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aefKey' - One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter.
--
-- * 'aefValues' - The values used to limit the execution information associated with the filter's key.
automationExecutionFilter
    :: AutomationExecutionFilterKey -- ^ 'aefKey'
    -> NonEmpty Text -- ^ 'aefValues'
    -> AutomationExecutionFilter
automationExecutionFilter pKey_ pValues_ =
  AutomationExecutionFilter' {_aefKey = pKey_, _aefValues = _List1 # pValues_}


-- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter.
aefKey :: Lens' AutomationExecutionFilter AutomationExecutionFilterKey
aefKey = lens _aefKey (\ s a -> s{_aefKey = a})

-- | The values used to limit the execution information associated with the filter's key.
aefValues :: Lens' AutomationExecutionFilter (NonEmpty Text)
aefValues = lens _aefValues (\ s a -> s{_aefValues = a}) . _List1

instance Hashable AutomationExecutionFilter where

instance NFData AutomationExecutionFilter where

instance ToJSON AutomationExecutionFilter where
        toJSON AutomationExecutionFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _aefKey),
                  Just ("Values" .= _aefValues)])

-- | Details about a specific Automation execution.
--
--
--
-- /See:/ 'automationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
  { _aemCurrentStepName             :: !(Maybe Text)
  , _aemTargetParameterName         :: !(Maybe Text)
  , _aemLogFile                     :: !(Maybe Text)
  , _aemExecutedBy                  :: !(Maybe Text)
  , _aemDocumentName                :: !(Maybe Text)
  , _aemExecutionEndTime            :: !(Maybe POSIX)
  , _aemFailureMessage              :: !(Maybe Text)
  , _aemMode                        :: !(Maybe ExecutionMode)
  , _aemAutomationExecutionStatus   :: !(Maybe AutomationExecutionStatus)
  , _aemParentAutomationExecutionId :: !(Maybe Text)
  , _aemOutputs                     :: !(Maybe (Map Text [Text]))
  , _aemMaxErrors                   :: !(Maybe Text)
  , _aemExecutionStartTime          :: !(Maybe POSIX)
  , _aemCurrentAction               :: !(Maybe Text)
  , _aemTargets                     :: !(Maybe [Target])
  , _aemResolvedTargets             :: !(Maybe ResolvedTargets)
  , _aemDocumentVersion             :: !(Maybe Text)
  , _aemAutomationExecutionId       :: !(Maybe Text)
  , _aemMaxConcurrency              :: !(Maybe Text)
  , _aemTarget                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutomationExecutionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aemCurrentStepName' - The name of the currently executing step.
--
-- * 'aemTargetParameterName' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemLogFile' - An Amazon S3 bucket where execution information is stored.
--
-- * 'aemExecutedBy' - The IAM role ARN of the user who executed the Automation.
--
-- * 'aemDocumentName' - The name of the Automation document used during execution.
--
-- * 'aemExecutionEndTime' - The time the execution finished. This is not populated if the execution is still in progress.
--
-- * 'aemFailureMessage' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemMode' - The Automation execution mode.
--
-- * 'aemAutomationExecutionStatus' - The status of the execution. Valid values include: Running, Succeeded, Failed, Timed out, or Cancelled.
--
-- * 'aemParentAutomationExecutionId' - The ExecutionId of the parent Automation.
--
-- * 'aemOutputs' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemMaxErrors' - The MaxErrors value specified by the user when starting the Automation.
--
-- * 'aemExecutionStartTime' - The time the execution started.>
--
-- * 'aemCurrentAction' - The action of the currently executing step.
--
-- * 'aemTargets' - The targets defined by the user when starting the Automation.
--
-- * 'aemResolvedTargets' - A list of targets that resolved during the execution.
--
-- * 'aemDocumentVersion' - The document version used during the execution.
--
-- * 'aemAutomationExecutionId' - The execution ID.
--
-- * 'aemMaxConcurrency' - The MaxConcurrency value specified by the user when starting the Automation.
--
-- * 'aemTarget' - The list of execution outputs as defined in the Automation document.
automationExecutionMetadata
    :: AutomationExecutionMetadata
automationExecutionMetadata =
  AutomationExecutionMetadata'
    { _aemCurrentStepName = Nothing
    , _aemTargetParameterName = Nothing
    , _aemLogFile = Nothing
    , _aemExecutedBy = Nothing
    , _aemDocumentName = Nothing
    , _aemExecutionEndTime = Nothing
    , _aemFailureMessage = Nothing
    , _aemMode = Nothing
    , _aemAutomationExecutionStatus = Nothing
    , _aemParentAutomationExecutionId = Nothing
    , _aemOutputs = Nothing
    , _aemMaxErrors = Nothing
    , _aemExecutionStartTime = Nothing
    , _aemCurrentAction = Nothing
    , _aemTargets = Nothing
    , _aemResolvedTargets = Nothing
    , _aemDocumentVersion = Nothing
    , _aemAutomationExecutionId = Nothing
    , _aemMaxConcurrency = Nothing
    , _aemTarget = Nothing
    }


-- | The name of the currently executing step.
aemCurrentStepName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemCurrentStepName = lens _aemCurrentStepName (\ s a -> s{_aemCurrentStepName = a})

-- | The list of execution outputs as defined in the Automation document.
aemTargetParameterName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemTargetParameterName = lens _aemTargetParameterName (\ s a -> s{_aemTargetParameterName = a})

-- | An Amazon S3 bucket where execution information is stored.
aemLogFile :: Lens' AutomationExecutionMetadata (Maybe Text)
aemLogFile = lens _aemLogFile (\ s a -> s{_aemLogFile = a})

-- | The IAM role ARN of the user who executed the Automation.
aemExecutedBy :: Lens' AutomationExecutionMetadata (Maybe Text)
aemExecutedBy = lens _aemExecutedBy (\ s a -> s{_aemExecutedBy = a})

-- | The name of the Automation document used during execution.
aemDocumentName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemDocumentName = lens _aemDocumentName (\ s a -> s{_aemDocumentName = a})

-- | The time the execution finished. This is not populated if the execution is still in progress.
aemExecutionEndTime :: Lens' AutomationExecutionMetadata (Maybe UTCTime)
aemExecutionEndTime = lens _aemExecutionEndTime (\ s a -> s{_aemExecutionEndTime = a}) . mapping _Time

-- | The list of execution outputs as defined in the Automation document.
aemFailureMessage :: Lens' AutomationExecutionMetadata (Maybe Text)
aemFailureMessage = lens _aemFailureMessage (\ s a -> s{_aemFailureMessage = a})

-- | The Automation execution mode.
aemMode :: Lens' AutomationExecutionMetadata (Maybe ExecutionMode)
aemMode = lens _aemMode (\ s a -> s{_aemMode = a})

-- | The status of the execution. Valid values include: Running, Succeeded, Failed, Timed out, or Cancelled.
aemAutomationExecutionStatus :: Lens' AutomationExecutionMetadata (Maybe AutomationExecutionStatus)
aemAutomationExecutionStatus = lens _aemAutomationExecutionStatus (\ s a -> s{_aemAutomationExecutionStatus = a})

-- | The ExecutionId of the parent Automation.
aemParentAutomationExecutionId :: Lens' AutomationExecutionMetadata (Maybe Text)
aemParentAutomationExecutionId = lens _aemParentAutomationExecutionId (\ s a -> s{_aemParentAutomationExecutionId = a})

-- | The list of execution outputs as defined in the Automation document.
aemOutputs :: Lens' AutomationExecutionMetadata (HashMap Text [Text])
aemOutputs = lens _aemOutputs (\ s a -> s{_aemOutputs = a}) . _Default . _Map

-- | The MaxErrors value specified by the user when starting the Automation.
aemMaxErrors :: Lens' AutomationExecutionMetadata (Maybe Text)
aemMaxErrors = lens _aemMaxErrors (\ s a -> s{_aemMaxErrors = a})

-- | The time the execution started.>
aemExecutionStartTime :: Lens' AutomationExecutionMetadata (Maybe UTCTime)
aemExecutionStartTime = lens _aemExecutionStartTime (\ s a -> s{_aemExecutionStartTime = a}) . mapping _Time

-- | The action of the currently executing step.
aemCurrentAction :: Lens' AutomationExecutionMetadata (Maybe Text)
aemCurrentAction = lens _aemCurrentAction (\ s a -> s{_aemCurrentAction = a})

-- | The targets defined by the user when starting the Automation.
aemTargets :: Lens' AutomationExecutionMetadata [Target]
aemTargets = lens _aemTargets (\ s a -> s{_aemTargets = a}) . _Default . _Coerce

-- | A list of targets that resolved during the execution.
aemResolvedTargets :: Lens' AutomationExecutionMetadata (Maybe ResolvedTargets)
aemResolvedTargets = lens _aemResolvedTargets (\ s a -> s{_aemResolvedTargets = a})

-- | The document version used during the execution.
aemDocumentVersion :: Lens' AutomationExecutionMetadata (Maybe Text)
aemDocumentVersion = lens _aemDocumentVersion (\ s a -> s{_aemDocumentVersion = a})

-- | The execution ID.
aemAutomationExecutionId :: Lens' AutomationExecutionMetadata (Maybe Text)
aemAutomationExecutionId = lens _aemAutomationExecutionId (\ s a -> s{_aemAutomationExecutionId = a})

-- | The MaxConcurrency value specified by the user when starting the Automation.
aemMaxConcurrency :: Lens' AutomationExecutionMetadata (Maybe Text)
aemMaxConcurrency = lens _aemMaxConcurrency (\ s a -> s{_aemMaxConcurrency = a})

-- | The list of execution outputs as defined in the Automation document.
aemTarget :: Lens' AutomationExecutionMetadata (Maybe Text)
aemTarget = lens _aemTarget (\ s a -> s{_aemTarget = a})

instance FromJSON AutomationExecutionMetadata where
        parseJSON
          = withObject "AutomationExecutionMetadata"
              (\ x ->
                 AutomationExecutionMetadata' <$>
                   (x .:? "CurrentStepName") <*>
                     (x .:? "TargetParameterName")
                     <*> (x .:? "LogFile")
                     <*> (x .:? "ExecutedBy")
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "ExecutionEndTime")
                     <*> (x .:? "FailureMessage")
                     <*> (x .:? "Mode")
                     <*> (x .:? "AutomationExecutionStatus")
                     <*> (x .:? "ParentAutomationExecutionId")
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "MaxErrors")
                     <*> (x .:? "ExecutionStartTime")
                     <*> (x .:? "CurrentAction")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "ResolvedTargets")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AutomationExecutionId")
                     <*> (x .:? "MaxConcurrency")
                     <*> (x .:? "Target"))

instance Hashable AutomationExecutionMetadata where

instance NFData AutomationExecutionMetadata where

-- | Describes a command request.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
  { _cStatus             :: !(Maybe CommandStatus)
  , _cExpiresAfter       :: !(Maybe POSIX)
  , _cNotificationConfig :: !(Maybe NotificationConfig)
  , _cTargetCount        :: !(Maybe Int)
  , _cOutputS3KeyPrefix  :: !(Maybe Text)
  , _cDocumentName       :: !(Maybe Text)
  , _cErrorCount         :: !(Maybe Int)
  , _cStatusDetails      :: !(Maybe Text)
  , _cMaxErrors          :: !(Maybe Text)
  , _cInstanceIds        :: !(Maybe [Text])
  , _cOutputS3Region     :: !(Maybe Text)
  , _cTargets            :: !(Maybe [Target])
  , _cCommandId          :: !(Maybe Text)
  , _cParameters         :: !(Maybe (Map Text [Text]))
  , _cDocumentVersion    :: !(Maybe Text)
  , _cComment            :: !(Maybe Text)
  , _cCompletedCount     :: !(Maybe Int)
  , _cOutputS3BucketName :: !(Maybe Text)
  , _cMaxConcurrency     :: !(Maybe Text)
  , _cRequestedDateTime  :: !(Maybe POSIX)
  , _cServiceRole        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the command.
--
-- * 'cExpiresAfter' - If this time is reached and the command has not already started executing, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
--
-- * 'cNotificationConfig' - Configurations for sending notifications about command status changes.
--
-- * 'cTargetCount' - The number of targets for the command.
--
-- * 'cOutputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- * 'cDocumentName' - The name of the document requested for execution.
--
-- * 'cErrorCount' - The number of targets for which the status is Failed or Execution Timed Out.
--
-- * 'cStatusDetails' - A detailed status of the command execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to any instances.     * In Progress: The command has been sent to at least one instance but has not reached a final state on all instances.     * Success: The command successfully executed on all invocations. This is a terminal state.     * Delivery Timed Out: The value of MaxErrors or more command invocations shows a status of Delivery Timed Out. This is a terminal state.     * Execution Timed Out: The value of MaxErrors or more command invocations shows a status of Execution Timed Out. This is a terminal state.     * Failed: The value of MaxErrors or more command invocations shows a status of Failed. This is a terminal state.     * Incomplete: The command was attempted on all instances and one or more invocations does not have a value of Success but not enough invocations failed for the status to be Failed. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Rate Exceeded: The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before executing it on any instance. This is a terminal state.
--
-- * 'cMaxErrors' - The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Executing a Command Using Systems Manager Run Command> .
--
-- * 'cInstanceIds' - The instance IDs against which this command was requested.
--
-- * 'cOutputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
--
-- * 'cTargets' - An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
--
-- * 'cCommandId' - A unique identifier for this command.
--
-- * 'cParameters' - The parameter values to be inserted in the document when executing the command.
--
-- * 'cDocumentVersion' - The SSM document version.
--
-- * 'cComment' - User-specified information about the command, such as a brief description of what the command should do.
--
-- * 'cCompletedCount' - The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
--
-- * 'cOutputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
--
-- * 'cMaxConcurrency' - The maximum number of instances that are allowed to execute the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Executing a Command Using Systems Manager Run Command> .
--
-- * 'cRequestedDateTime' - The date and time the command was requested.
--
-- * 'cServiceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
command
    :: Command
command =
  Command'
    { _cStatus = Nothing
    , _cExpiresAfter = Nothing
    , _cNotificationConfig = Nothing
    , _cTargetCount = Nothing
    , _cOutputS3KeyPrefix = Nothing
    , _cDocumentName = Nothing
    , _cErrorCount = Nothing
    , _cStatusDetails = Nothing
    , _cMaxErrors = Nothing
    , _cInstanceIds = Nothing
    , _cOutputS3Region = Nothing
    , _cTargets = Nothing
    , _cCommandId = Nothing
    , _cParameters = Nothing
    , _cDocumentVersion = Nothing
    , _cComment = Nothing
    , _cCompletedCount = Nothing
    , _cOutputS3BucketName = Nothing
    , _cMaxConcurrency = Nothing
    , _cRequestedDateTime = Nothing
    , _cServiceRole = Nothing
    }


-- | The status of the command.
cStatus :: Lens' Command (Maybe CommandStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | If this time is reached and the command has not already started executing, it will not run. Calculated based on the ExpiresAfter user input provided as part of the SendCommand API.
cExpiresAfter :: Lens' Command (Maybe UTCTime)
cExpiresAfter = lens _cExpiresAfter (\ s a -> s{_cExpiresAfter = a}) . mapping _Time

-- | Configurations for sending notifications about command status changes.
cNotificationConfig :: Lens' Command (Maybe NotificationConfig)
cNotificationConfig = lens _cNotificationConfig (\ s a -> s{_cNotificationConfig = a})

-- | The number of targets for the command.
cTargetCount :: Lens' Command (Maybe Int)
cTargetCount = lens _cTargetCount (\ s a -> s{_cTargetCount = a})

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cOutputS3KeyPrefix :: Lens' Command (Maybe Text)
cOutputS3KeyPrefix = lens _cOutputS3KeyPrefix (\ s a -> s{_cOutputS3KeyPrefix = a})

-- | The name of the document requested for execution.
cDocumentName :: Lens' Command (Maybe Text)
cDocumentName = lens _cDocumentName (\ s a -> s{_cDocumentName = a})

-- | The number of targets for which the status is Failed or Execution Timed Out.
cErrorCount :: Lens' Command (Maybe Int)
cErrorCount = lens _cErrorCount (\ s a -> s{_cErrorCount = a})

-- | A detailed status of the command execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to any instances.     * In Progress: The command has been sent to at least one instance but has not reached a final state on all instances.     * Success: The command successfully executed on all invocations. This is a terminal state.     * Delivery Timed Out: The value of MaxErrors or more command invocations shows a status of Delivery Timed Out. This is a terminal state.     * Execution Timed Out: The value of MaxErrors or more command invocations shows a status of Execution Timed Out. This is a terminal state.     * Failed: The value of MaxErrors or more command invocations shows a status of Failed. This is a terminal state.     * Incomplete: The command was attempted on all instances and one or more invocations does not have a value of Success but not enough invocations failed for the status to be Failed. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Rate Exceeded: The number of instances targeted by the command exceeded the account limit for pending invocations. The system has canceled the command before executing it on any instance. This is a terminal state.
cStatusDetails :: Lens' Command (Maybe Text)
cStatusDetails = lens _cStatusDetails (\ s a -> s{_cStatusDetails = a})

-- | The maximum number of errors allowed before the system stops sending the command to additional targets. You can specify a number of errors, such as 10, or a percentage or errors, such as 10%. The default value is 0. For more information about how to use MaxErrors, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Executing a Command Using Systems Manager Run Command> .
cMaxErrors :: Lens' Command (Maybe Text)
cMaxErrors = lens _cMaxErrors (\ s a -> s{_cMaxErrors = a})

-- | The instance IDs against which this command was requested.
cInstanceIds :: Lens' Command [Text]
cInstanceIds = lens _cInstanceIds (\ s a -> s{_cInstanceIds = a}) . _Default . _Coerce

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
cOutputS3Region :: Lens' Command (Maybe Text)
cOutputS3Region = lens _cOutputS3Region (\ s a -> s{_cOutputS3Region = a})

-- | An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call.
cTargets :: Lens' Command [Target]
cTargets = lens _cTargets (\ s a -> s{_cTargets = a}) . _Default . _Coerce

-- | A unique identifier for this command.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\ s a -> s{_cCommandId = a})

-- | The parameter values to be inserted in the document when executing the command.
cParameters :: Lens' Command (HashMap Text [Text])
cParameters = lens _cParameters (\ s a -> s{_cParameters = a}) . _Default . _Map

-- | The SSM document version.
cDocumentVersion :: Lens' Command (Maybe Text)
cDocumentVersion = lens _cDocumentVersion (\ s a -> s{_cDocumentVersion = a})

-- | User-specified information about the command, such as a brief description of what the command should do.
cComment :: Lens' Command (Maybe Text)
cComment = lens _cComment (\ s a -> s{_cComment = a})

-- | The number of targets for which the command invocation reached a terminal state. Terminal states include the following: Success, Failed, Execution Timed Out, Delivery Timed Out, Canceled, Terminated, or Undeliverable.
cCompletedCount :: Lens' Command (Maybe Int)
cCompletedCount = lens _cCompletedCount (\ s a -> s{_cCompletedCount = a})

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command.
cOutputS3BucketName :: Lens' Command (Maybe Text)
cOutputS3BucketName = lens _cOutputS3BucketName (\ s a -> s{_cOutputS3BucketName = a})

-- | The maximum number of instances that are allowed to execute the command at the same time. You can specify a number of instances, such as 10, or a percentage of instances, such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/run-command.html Executing a Command Using Systems Manager Run Command> .
cMaxConcurrency :: Lens' Command (Maybe Text)
cMaxConcurrency = lens _cMaxConcurrency (\ s a -> s{_cMaxConcurrency = a})

-- | The date and time the command was requested.
cRequestedDateTime :: Lens' Command (Maybe UTCTime)
cRequestedDateTime = lens _cRequestedDateTime (\ s a -> s{_cRequestedDateTime = a}) . mapping _Time

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes.
cServiceRole :: Lens' Command (Maybe Text)
cServiceRole = lens _cServiceRole (\ s a -> s{_cServiceRole = a})

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Status") <*> (x .:? "ExpiresAfter") <*>
                     (x .:? "NotificationConfig")
                     <*> (x .:? "TargetCount")
                     <*> (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "ErrorCount")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "MaxErrors")
                     <*> (x .:? "InstanceIds" .!= mempty)
                     <*> (x .:? "OutputS3Region")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "CommandId")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "Comment")
                     <*> (x .:? "CompletedCount")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "MaxConcurrency")
                     <*> (x .:? "RequestedDateTime")
                     <*> (x .:? "ServiceRole"))

instance Hashable Command where

instance NFData Command where

-- | Describes a command filter.
--
--
--
-- /See:/ 'commandFilter' smart constructor.
data CommandFilter = CommandFilter'
  { _cfKey   :: !CommandFilterKey
  , _cfValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommandFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfKey' - The name of the filter.
--
-- * 'cfValue' - The filter value.
commandFilter
    :: CommandFilterKey -- ^ 'cfKey'
    -> Text -- ^ 'cfValue'
    -> CommandFilter
commandFilter pKey_ pValue_ =
  CommandFilter' {_cfKey = pKey_, _cfValue = pValue_}


-- | The name of the filter.
cfKey :: Lens' CommandFilter CommandFilterKey
cfKey = lens _cfKey (\ s a -> s{_cfKey = a})

-- | The filter value.
cfValue :: Lens' CommandFilter Text
cfValue = lens _cfValue (\ s a -> s{_cfValue = a})

instance Hashable CommandFilter where

instance NFData CommandFilter where

instance ToJSON CommandFilter where
        toJSON CommandFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _cfKey), Just ("value" .= _cfValue)])

-- | An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you executed.
--
--
--
-- /See:/ 'commandInvocation' smart constructor.
data CommandInvocation = CommandInvocation'
  { _comInstanceId         :: !(Maybe Text)
  , _comStatus             :: !(Maybe CommandInvocationStatus)
  , _comNotificationConfig :: !(Maybe NotificationConfig)
  , _comCommandPlugins     :: !(Maybe [CommandPlugin])
  , _comDocumentName       :: !(Maybe Text)
  , _comStandardErrorURL   :: !(Maybe Text)
  , _comStatusDetails      :: !(Maybe Text)
  , _comStandardOutputURL  :: !(Maybe Text)
  , _comCommandId          :: !(Maybe Text)
  , _comDocumentVersion    :: !(Maybe Text)
  , _comComment            :: !(Maybe Text)
  , _comTraceOutput        :: !(Maybe Text)
  , _comInstanceName       :: !(Maybe Text)
  , _comRequestedDateTime  :: !(Maybe POSIX)
  , _comServiceRole        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommandInvocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'comInstanceId' - The instance ID in which this invocation was requested.
--
-- * 'comStatus' - Whether or not the invocation succeeded, failed, or is pending.
--
-- * 'comNotificationConfig' - Configurations for sending notifications about command status changes on a per instance basis.
--
-- * 'comCommandPlugins' - Undocumented member.
--
-- * 'comDocumentName' - The document name that was requested for execution.
--
-- * 'comStandardErrorURL' - The URL to the plugin's StdErr file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
--
-- * 'comStatusDetails' - A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'comStandardOutputURL' - The URL to the plugin's StdOut file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
--
-- * 'comCommandId' - The command against which this invocation was requested.
--
-- * 'comDocumentVersion' - The SSM document version.
--
-- * 'comComment' - User-specified information about the command, such as a brief description of what the command should do.
--
-- * 'comTraceOutput' - Gets the trace output sent by the agent.
--
-- * 'comInstanceName' - The name of the invocation target. For Amazon EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
--
-- * 'comRequestedDateTime' - The time and date the request was sent to this instance.
--
-- * 'comServiceRole' - The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
commandInvocation
    :: CommandInvocation
commandInvocation =
  CommandInvocation'
    { _comInstanceId = Nothing
    , _comStatus = Nothing
    , _comNotificationConfig = Nothing
    , _comCommandPlugins = Nothing
    , _comDocumentName = Nothing
    , _comStandardErrorURL = Nothing
    , _comStatusDetails = Nothing
    , _comStandardOutputURL = Nothing
    , _comCommandId = Nothing
    , _comDocumentVersion = Nothing
    , _comComment = Nothing
    , _comTraceOutput = Nothing
    , _comInstanceName = Nothing
    , _comRequestedDateTime = Nothing
    , _comServiceRole = Nothing
    }


-- | The instance ID in which this invocation was requested.
comInstanceId :: Lens' CommandInvocation (Maybe Text)
comInstanceId = lens _comInstanceId (\ s a -> s{_comInstanceId = a})

-- | Whether or not the invocation succeeded, failed, or is pending.
comStatus :: Lens' CommandInvocation (Maybe CommandInvocationStatus)
comStatus = lens _comStatus (\ s a -> s{_comStatus = a})

-- | Configurations for sending notifications about command status changes on a per instance basis.
comNotificationConfig :: Lens' CommandInvocation (Maybe NotificationConfig)
comNotificationConfig = lens _comNotificationConfig (\ s a -> s{_comNotificationConfig = a})

-- | Undocumented member.
comCommandPlugins :: Lens' CommandInvocation [CommandPlugin]
comCommandPlugins = lens _comCommandPlugins (\ s a -> s{_comCommandPlugins = a}) . _Default . _Coerce

-- | The document name that was requested for execution.
comDocumentName :: Lens' CommandInvocation (Maybe Text)
comDocumentName = lens _comDocumentName (\ s a -> s{_comDocumentName = a})

-- | The URL to the plugin's StdErr file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, StandardErrorUrl is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
comStandardErrorURL :: Lens' CommandInvocation (Maybe Text)
comStandardErrorURL = lens _comStandardErrorURL (\ s a -> s{_comStandardErrorURL = a})

-- | A detailed status of the command execution for each invocation (each instance targeted by the command). StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist or might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit and don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
comStatusDetails :: Lens' CommandInvocation (Maybe Text)
comStatusDetails = lens _comStatusDetails (\ s a -> s{_comStatusDetails = a})

-- | The URL to the plugin's StdOut file in Amazon S3, if the Amazon S3 bucket was defined for the parent command. For an invocation, StandardOutputUrl is populated if there is just one plugin defined for the command, and the Amazon S3 bucket was defined for the command.
comStandardOutputURL :: Lens' CommandInvocation (Maybe Text)
comStandardOutputURL = lens _comStandardOutputURL (\ s a -> s{_comStandardOutputURL = a})

-- | The command against which this invocation was requested.
comCommandId :: Lens' CommandInvocation (Maybe Text)
comCommandId = lens _comCommandId (\ s a -> s{_comCommandId = a})

-- | The SSM document version.
comDocumentVersion :: Lens' CommandInvocation (Maybe Text)
comDocumentVersion = lens _comDocumentVersion (\ s a -> s{_comDocumentVersion = a})

-- | User-specified information about the command, such as a brief description of what the command should do.
comComment :: Lens' CommandInvocation (Maybe Text)
comComment = lens _comComment (\ s a -> s{_comComment = a})

-- | Gets the trace output sent by the agent.
comTraceOutput :: Lens' CommandInvocation (Maybe Text)
comTraceOutput = lens _comTraceOutput (\ s a -> s{_comTraceOutput = a})

-- | The name of the invocation target. For Amazon EC2 instances this is the value for the aws:Name tag. For on-premises instances, this is the name of the instance.
comInstanceName :: Lens' CommandInvocation (Maybe Text)
comInstanceName = lens _comInstanceName (\ s a -> s{_comInstanceName = a})

-- | The time and date the request was sent to this instance.
comRequestedDateTime :: Lens' CommandInvocation (Maybe UTCTime)
comRequestedDateTime = lens _comRequestedDateTime (\ s a -> s{_comRequestedDateTime = a}) . mapping _Time

-- | The IAM service role that Run Command uses to act on your behalf when sending notifications about command status changes on a per instance basis.
comServiceRole :: Lens' CommandInvocation (Maybe Text)
comServiceRole = lens _comServiceRole (\ s a -> s{_comServiceRole = a})

instance FromJSON CommandInvocation where
        parseJSON
          = withObject "CommandInvocation"
              (\ x ->
                 CommandInvocation' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "NotificationConfig")
                     <*> (x .:? "CommandPlugins" .!= mempty)
                     <*> (x .:? "DocumentName")
                     <*> (x .:? "StandardErrorUrl")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "StandardOutputUrl")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "Comment")
                     <*> (x .:? "TraceOutput")
                     <*> (x .:? "InstanceName")
                     <*> (x .:? "RequestedDateTime")
                     <*> (x .:? "ServiceRole"))

instance Hashable CommandInvocation where

instance NFData CommandInvocation where

-- | Describes plugin details.
--
--
--
-- /See:/ 'commandPlugin' smart constructor.
data CommandPlugin = CommandPlugin'
  { _cpStatus                 :: !(Maybe CommandPluginStatus)
  , _cpResponseStartDateTime  :: !(Maybe POSIX)
  , _cpOutputS3KeyPrefix      :: !(Maybe Text)
  , _cpStandardErrorURL       :: !(Maybe Text)
  , _cpResponseCode           :: !(Maybe Int)
  , _cpStatusDetails          :: !(Maybe Text)
  , _cpOutput                 :: !(Maybe Text)
  , _cpStandardOutputURL      :: !(Maybe Text)
  , _cpName                   :: !(Maybe Text)
  , _cpOutputS3Region         :: !(Maybe Text)
  , _cpOutputS3BucketName     :: !(Maybe Text)
  , _cpResponseFinishDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommandPlugin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpStatus' - The status of this plugin. You can execute a document with multiple plugins.
--
-- * 'cpResponseStartDateTime' - The time the plugin started executing.
--
-- * 'cpOutputS3KeyPrefix' - The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript  test_folder is the name of the Amazon S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-1234567876543 is the instance ID; awsrunShellScript is the name of the plugin.
--
-- * 'cpStandardErrorURL' - The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
--
-- * 'cpResponseCode' - A numeric response code generated after executing the plugin.
--
-- * 'cpStatusDetails' - A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
--
-- * 'cpOutput' - Output of the plugin execution.
--
-- * 'cpStandardOutputURL' - The URL for the complete text written by the plugin to stdout in Amazon S3. If the Amazon S3 bucket for the command was not specified, then this string is empty.
--
-- * 'cpName' - The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
--
-- * 'cpOutputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
--
-- * 'cpOutputS3BucketName' - The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript  test_folder is the name of the Amazon S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-1234567876543 is the instance ID; awsrunShellScript is the name of the plugin.
--
-- * 'cpResponseFinishDateTime' - The time the plugin stopped executing. Could stop prematurely if, for example, a cancel command was sent.
commandPlugin
    :: CommandPlugin
commandPlugin =
  CommandPlugin'
    { _cpStatus = Nothing
    , _cpResponseStartDateTime = Nothing
    , _cpOutputS3KeyPrefix = Nothing
    , _cpStandardErrorURL = Nothing
    , _cpResponseCode = Nothing
    , _cpStatusDetails = Nothing
    , _cpOutput = Nothing
    , _cpStandardOutputURL = Nothing
    , _cpName = Nothing
    , _cpOutputS3Region = Nothing
    , _cpOutputS3BucketName = Nothing
    , _cpResponseFinishDateTime = Nothing
    }


-- | The status of this plugin. You can execute a document with multiple plugins.
cpStatus :: Lens' CommandPlugin (Maybe CommandPluginStatus)
cpStatus = lens _cpStatus (\ s a -> s{_cpStatus = a})

-- | The time the plugin started executing.
cpResponseStartDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseStartDateTime = lens _cpResponseStartDateTime (\ s a -> s{_cpResponseStartDateTime = a}) . mapping _Time

-- | The S3 directory path inside the bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript  test_folder is the name of the Amazon S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-1234567876543 is the instance ID; awsrunShellScript is the name of the plugin.
cpOutputS3KeyPrefix :: Lens' CommandPlugin (Maybe Text)
cpOutputS3KeyPrefix = lens _cpOutputS3KeyPrefix (\ s a -> s{_cpOutputS3KeyPrefix = a})

-- | The URL for the complete text written by the plugin to stderr. If execution is not yet complete, then this string is empty.
cpStandardErrorURL :: Lens' CommandPlugin (Maybe Text)
cpStandardErrorURL = lens _cpStandardErrorURL (\ s a -> s{_cpStandardErrorURL = a})

-- | A numeric response code generated after executing the plugin.
cpResponseCode :: Lens' CommandPlugin (Maybe Int)
cpResponseCode = lens _cpResponseCode (\ s a -> s{_cpResponseCode = a})

-- | A detailed status of the plugin execution. StatusDetails includes more information than Status because it includes states resulting from error and concurrency control parameters. StatusDetails can show different results than Status. For more information about these statuses, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-about-status.html Run Command Status> . StatusDetails can be one of the following values:     * Pending: The command has not been sent to the instance.     * In Progress: The command has been sent to the instance but has not reached a terminal state.     * Success: The execution of the command or plugin was successfully completed. This is a terminal state.     * Delivery Timed Out: The command was not delivered to the instance before the delivery timeout expired. Delivery timeouts do not count against the parent command's MaxErrors limit, but they do contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Execution Timed Out: Command execution started on the instance, but the execution was not complete before the execution timeout expired. Execution timeouts count against the MaxErrors limit of the parent command. This is a terminal state.     * Failed: The command was not successful on the instance. For a plugin, this indicates that the result code was not zero. For a command invocation, this indicates that the result code for one or more plugins was not zero. Invocation failures count against the MaxErrors limit of the parent command. This is a terminal state.     * Canceled: The command was terminated before it was completed. This is a terminal state.     * Undeliverable: The command can't be delivered to the instance. The instance might not exist, or it might not be responding. Undeliverable invocations don't count against the parent command's MaxErrors limit, and they don't contribute to whether the parent command status is Success or Incomplete. This is a terminal state.     * Terminated: The parent command exceeded its MaxErrors limit and subsequent command invocations were canceled by the system. This is a terminal state.
cpStatusDetails :: Lens' CommandPlugin (Maybe Text)
cpStatusDetails = lens _cpStatusDetails (\ s a -> s{_cpStatusDetails = a})

-- | Output of the plugin execution.
cpOutput :: Lens' CommandPlugin (Maybe Text)
cpOutput = lens _cpOutput (\ s a -> s{_cpOutput = a})

-- | The URL for the complete text written by the plugin to stdout in Amazon S3. If the Amazon S3 bucket for the command was not specified, then this string is empty.
cpStandardOutputURL :: Lens' CommandPlugin (Maybe Text)
cpStandardOutputURL = lens _cpStandardOutputURL (\ s a -> s{_cpStandardOutputURL = a})

-- | The name of the plugin. Must be one of the following: aws:updateAgent, aws:domainjoin, aws:applications, aws:runPowerShellScript, aws:psmodule, aws:cloudWatch, aws:runShellScript, or aws:updateSSMAgent.
cpName :: Lens' CommandPlugin (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
cpOutputS3Region :: Lens' CommandPlugin (Maybe Text)
cpOutputS3Region = lens _cpOutputS3Region (\ s a -> s{_cpOutputS3Region = a})

-- | The S3 bucket where the responses to the command executions should be stored. This was requested when issuing the command. For example, in the following response: test_folder/ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix/i-1234567876543/awsrunShellScript  test_folder is the name of the Amazon S3 bucket; ab19cb99-a030-46dd-9dfc-8eSAMPLEPre-Fix is the name of the S3 prefix; i-1234567876543 is the instance ID; awsrunShellScript is the name of the plugin.
cpOutputS3BucketName :: Lens' CommandPlugin (Maybe Text)
cpOutputS3BucketName = lens _cpOutputS3BucketName (\ s a -> s{_cpOutputS3BucketName = a})

-- | The time the plugin stopped executing. Could stop prematurely if, for example, a cancel command was sent.
cpResponseFinishDateTime :: Lens' CommandPlugin (Maybe UTCTime)
cpResponseFinishDateTime = lens _cpResponseFinishDateTime (\ s a -> s{_cpResponseFinishDateTime = a}) . mapping _Time

instance FromJSON CommandPlugin where
        parseJSON
          = withObject "CommandPlugin"
              (\ x ->
                 CommandPlugin' <$>
                   (x .:? "Status") <*> (x .:? "ResponseStartDateTime")
                     <*> (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "StandardErrorUrl")
                     <*> (x .:? "ResponseCode")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "Output")
                     <*> (x .:? "StandardOutputUrl")
                     <*> (x .:? "Name")
                     <*> (x .:? "OutputS3Region")
                     <*> (x .:? "OutputS3BucketName")
                     <*> (x .:? "ResponseFinishDateTime"))

instance Hashable CommandPlugin where

instance NFData CommandPlugin where

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
--
--
-- /See:/ 'complianceExecutionSummary' smart constructor.
data ComplianceExecutionSummary = ComplianceExecutionSummary'
  { _cesExecutionId   :: !(Maybe Text)
  , _cesExecutionType :: !(Maybe Text)
  , _cesExecutionTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesExecutionId' - An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
--
-- * 'cesExecutionType' - The type of execution. For example, @Command@ is a valid execution type.
--
-- * 'cesExecutionTime' - The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
complianceExecutionSummary
    :: UTCTime -- ^ 'cesExecutionTime'
    -> ComplianceExecutionSummary
complianceExecutionSummary pExecutionTime_ =
  ComplianceExecutionSummary'
    { _cesExecutionId = Nothing
    , _cesExecutionType = Nothing
    , _cesExecutionTime = _Time # pExecutionTime_
    }


-- | An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
cesExecutionId :: Lens' ComplianceExecutionSummary (Maybe Text)
cesExecutionId = lens _cesExecutionId (\ s a -> s{_cesExecutionId = a})

-- | The type of execution. For example, @Command@ is a valid execution type.
cesExecutionType :: Lens' ComplianceExecutionSummary (Maybe Text)
cesExecutionType = lens _cesExecutionType (\ s a -> s{_cesExecutionType = a})

-- | The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
cesExecutionTime :: Lens' ComplianceExecutionSummary UTCTime
cesExecutionTime = lens _cesExecutionTime (\ s a -> s{_cesExecutionTime = a}) . _Time

instance FromJSON ComplianceExecutionSummary where
        parseJSON
          = withObject "ComplianceExecutionSummary"
              (\ x ->
                 ComplianceExecutionSummary' <$>
                   (x .:? "ExecutionId") <*> (x .:? "ExecutionType") <*>
                     (x .: "ExecutionTime"))

instance Hashable ComplianceExecutionSummary where

instance NFData ComplianceExecutionSummary where

instance ToJSON ComplianceExecutionSummary where
        toJSON ComplianceExecutionSummary'{..}
          = object
              (catMaybes
                 [("ExecutionId" .=) <$> _cesExecutionId,
                  ("ExecutionType" .=) <$> _cesExecutionType,
                  Just ("ExecutionTime" .= _cesExecutionTime)])

-- | Information about the compliance as defined by the resource type. For example, for a patch resource type, @Items@ includes information about the PatchSeverity, Classification, etc.
--
--
--
-- /See:/ 'complianceItem' smart constructor.
data ComplianceItem = ComplianceItem'
  { _ciStatus           :: !(Maybe ComplianceStatus)
  , _ciResourceId       :: !(Maybe Text)
  , _ciResourceType     :: !(Maybe Text)
  , _ciSeverity         :: !(Maybe ComplianceSeverity)
  , _ciExecutionSummary :: !(Maybe ComplianceExecutionSummary)
  , _ciDetails          :: !(Maybe (Map Text Text))
  , _ciId               :: !(Maybe Text)
  , _ciComplianceType   :: !(Maybe Text)
  , _ciTitle            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciStatus' - The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
--
-- * 'ciResourceId' - An ID for the resource. For a managed instance, this is the instance ID.
--
-- * 'ciResourceType' - The type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- * 'ciSeverity' - The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- * 'ciExecutionSummary' - A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
--
-- * 'ciDetails' - A "Key": "Value" tag combination for the compliance item.
--
-- * 'ciId' - An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
--
-- * 'ciComplianceType' - The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
--
-- * 'ciTitle' - A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
complianceItem
    :: ComplianceItem
complianceItem =
  ComplianceItem'
    { _ciStatus = Nothing
    , _ciResourceId = Nothing
    , _ciResourceType = Nothing
    , _ciSeverity = Nothing
    , _ciExecutionSummary = Nothing
    , _ciDetails = Nothing
    , _ciId = Nothing
    , _ciComplianceType = Nothing
    , _ciTitle = Nothing
    }


-- | The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
ciStatus :: Lens' ComplianceItem (Maybe ComplianceStatus)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a})

-- | An ID for the resource. For a managed instance, this is the instance ID.
ciResourceId :: Lens' ComplianceItem (Maybe Text)
ciResourceId = lens _ciResourceId (\ s a -> s{_ciResourceId = a})

-- | The type of resource. @ManagedInstance@ is currently the only supported resource type.
ciResourceType :: Lens' ComplianceItem (Maybe Text)
ciResourceType = lens _ciResourceType (\ s a -> s{_ciResourceType = a})

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
ciSeverity :: Lens' ComplianceItem (Maybe ComplianceSeverity)
ciSeverity = lens _ciSeverity (\ s a -> s{_ciSeverity = a})

-- | A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
ciExecutionSummary :: Lens' ComplianceItem (Maybe ComplianceExecutionSummary)
ciExecutionSummary = lens _ciExecutionSummary (\ s a -> s{_ciExecutionSummary = a})

-- | A "Key": "Value" tag combination for the compliance item.
ciDetails :: Lens' ComplianceItem (HashMap Text Text)
ciDetails = lens _ciDetails (\ s a -> s{_ciDetails = a}) . _Default . _Map

-- | An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
ciId :: Lens' ComplianceItem (Maybe Text)
ciId = lens _ciId (\ s a -> s{_ciId = a})

-- | The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
ciComplianceType :: Lens' ComplianceItem (Maybe Text)
ciComplianceType = lens _ciComplianceType (\ s a -> s{_ciComplianceType = a})

-- | A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
ciTitle :: Lens' ComplianceItem (Maybe Text)
ciTitle = lens _ciTitle (\ s a -> s{_ciTitle = a})

instance FromJSON ComplianceItem where
        parseJSON
          = withObject "ComplianceItem"
              (\ x ->
                 ComplianceItem' <$>
                   (x .:? "Status") <*> (x .:? "ResourceId") <*>
                     (x .:? "ResourceType")
                     <*> (x .:? "Severity")
                     <*> (x .:? "ExecutionSummary")
                     <*> (x .:? "Details" .!= mempty)
                     <*> (x .:? "Id")
                     <*> (x .:? "ComplianceType")
                     <*> (x .:? "Title"))

instance Hashable ComplianceItem where

instance NFData ComplianceItem where

-- | Information about a compliance item.
--
--
--
-- /See:/ 'complianceItemEntry' smart constructor.
data ComplianceItemEntry = ComplianceItemEntry'
  { _cieDetails  :: !(Maybe (Map Text Text))
  , _cieId       :: !(Maybe Text)
  , _cieTitle    :: !(Maybe Text)
  , _cieSeverity :: !ComplianceSeverity
  , _cieStatus   :: !ComplianceStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceItemEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cieDetails' - A "Key": "Value" tag combination for the compliance item.
--
-- * 'cieId' - The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
--
-- * 'cieTitle' - The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
--
-- * 'cieSeverity' - The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- * 'cieStatus' - The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
complianceItemEntry
    :: ComplianceSeverity -- ^ 'cieSeverity'
    -> ComplianceStatus -- ^ 'cieStatus'
    -> ComplianceItemEntry
complianceItemEntry pSeverity_ pStatus_ =
  ComplianceItemEntry'
    { _cieDetails = Nothing
    , _cieId = Nothing
    , _cieTitle = Nothing
    , _cieSeverity = pSeverity_
    , _cieStatus = pStatus_
    }


-- | A "Key": "Value" tag combination for the compliance item.
cieDetails :: Lens' ComplianceItemEntry (HashMap Text Text)
cieDetails = lens _cieDetails (\ s a -> s{_cieDetails = a}) . _Default . _Map

-- | The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
cieId :: Lens' ComplianceItemEntry (Maybe Text)
cieId = lens _cieId (\ s a -> s{_cieId = a})

-- | The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
cieTitle :: Lens' ComplianceItemEntry (Maybe Text)
cieTitle = lens _cieTitle (\ s a -> s{_cieTitle = a})

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
cieSeverity :: Lens' ComplianceItemEntry ComplianceSeverity
cieSeverity = lens _cieSeverity (\ s a -> s{_cieSeverity = a})

-- | The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
cieStatus :: Lens' ComplianceItemEntry ComplianceStatus
cieStatus = lens _cieStatus (\ s a -> s{_cieStatus = a})

instance Hashable ComplianceItemEntry where

instance NFData ComplianceItemEntry where

instance ToJSON ComplianceItemEntry where
        toJSON ComplianceItemEntry'{..}
          = object
              (catMaybes
                 [("Details" .=) <$> _cieDetails,
                  ("Id" .=) <$> _cieId, ("Title" .=) <$> _cieTitle,
                  Just ("Severity" .= _cieSeverity),
                  Just ("Status" .= _cieStatus)])

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'complianceStringFilter' smart constructor.
data ComplianceStringFilter = ComplianceStringFilter'
  { _csfValues :: !(Maybe (List1 Text))
  , _csfKey    :: !(Maybe Text)
  , _csfType   :: !(Maybe ComplianceQueryOperatorType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfValues' - The value for which to search.
--
-- * 'csfKey' - The name of the filter.
--
-- * 'csfType' - The type of comparison that should be performed for the value: Equal, NotEqual, BeginWith, LessThan, or GreaterThan.
complianceStringFilter
    :: ComplianceStringFilter
complianceStringFilter =
  ComplianceStringFilter'
    {_csfValues = Nothing, _csfKey = Nothing, _csfType = Nothing}


-- | The value for which to search.
csfValues :: Lens' ComplianceStringFilter (Maybe (NonEmpty Text))
csfValues = lens _csfValues (\ s a -> s{_csfValues = a}) . mapping _List1

-- | The name of the filter.
csfKey :: Lens' ComplianceStringFilter (Maybe Text)
csfKey = lens _csfKey (\ s a -> s{_csfKey = a})

-- | The type of comparison that should be performed for the value: Equal, NotEqual, BeginWith, LessThan, or GreaterThan.
csfType :: Lens' ComplianceStringFilter (Maybe ComplianceQueryOperatorType)
csfType = lens _csfType (\ s a -> s{_csfType = a})

instance Hashable ComplianceStringFilter where

instance NFData ComplianceStringFilter where

instance ToJSON ComplianceStringFilter where
        toJSON ComplianceStringFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _csfValues,
                  ("Key" .=) <$> _csfKey, ("Type" .=) <$> _csfType])

-- | A summary of compliance information by compliance type.
--
--
--
-- /See:/ 'complianceSummaryItem' smart constructor.
data ComplianceSummaryItem = ComplianceSummaryItem'
  { _csiNonCompliantSummary :: !(Maybe NonCompliantSummary)
  , _csiCompliantSummary    :: !(Maybe CompliantSummary)
  , _csiComplianceType      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ComplianceSummaryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiNonCompliantSummary' - A list of NON_COMPLIANT items for the specified compliance type.
--
-- * 'csiCompliantSummary' - A list of COMPLIANT items for the specified compliance type.
--
-- * 'csiComplianceType' - The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
complianceSummaryItem
    :: ComplianceSummaryItem
complianceSummaryItem =
  ComplianceSummaryItem'
    { _csiNonCompliantSummary = Nothing
    , _csiCompliantSummary = Nothing
    , _csiComplianceType = Nothing
    }


-- | A list of NON_COMPLIANT items for the specified compliance type.
csiNonCompliantSummary :: Lens' ComplianceSummaryItem (Maybe NonCompliantSummary)
csiNonCompliantSummary = lens _csiNonCompliantSummary (\ s a -> s{_csiNonCompliantSummary = a})

-- | A list of COMPLIANT items for the specified compliance type.
csiCompliantSummary :: Lens' ComplianceSummaryItem (Maybe CompliantSummary)
csiCompliantSummary = lens _csiCompliantSummary (\ s a -> s{_csiCompliantSummary = a})

-- | The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
csiComplianceType :: Lens' ComplianceSummaryItem (Maybe Text)
csiComplianceType = lens _csiComplianceType (\ s a -> s{_csiComplianceType = a})

instance FromJSON ComplianceSummaryItem where
        parseJSON
          = withObject "ComplianceSummaryItem"
              (\ x ->
                 ComplianceSummaryItem' <$>
                   (x .:? "NonCompliantSummary") <*>
                     (x .:? "CompliantSummary")
                     <*> (x .:? "ComplianceType"))

instance Hashable ComplianceSummaryItem where

instance NFData ComplianceSummaryItem where

-- | A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.
--
--
--
-- /See:/ 'compliantSummary' smart constructor.
data CompliantSummary = CompliantSummary'
  { _csCompliantCount  :: !(Maybe Int)
  , _csSeveritySummary :: !(Maybe SeveritySummary)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompliantSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCompliantCount' - The total number of resources that are compliant.
--
-- * 'csSeveritySummary' - A summary of the compliance severity by compliance type.
compliantSummary
    :: CompliantSummary
compliantSummary =
  CompliantSummary' {_csCompliantCount = Nothing, _csSeveritySummary = Nothing}


-- | The total number of resources that are compliant.
csCompliantCount :: Lens' CompliantSummary (Maybe Int)
csCompliantCount = lens _csCompliantCount (\ s a -> s{_csCompliantCount = a})

-- | A summary of the compliance severity by compliance type.
csSeveritySummary :: Lens' CompliantSummary (Maybe SeveritySummary)
csSeveritySummary = lens _csSeveritySummary (\ s a -> s{_csSeveritySummary = a})

instance FromJSON CompliantSummary where
        parseJSON
          = withObject "CompliantSummary"
              (\ x ->
                 CompliantSummary' <$>
                   (x .:? "CompliantCount") <*>
                     (x .:? "SeveritySummary"))

instance Hashable CompliantSummary where

instance NFData CompliantSummary where

-- | Describes the association of a Systems Manager document and an instance.
--
--
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
  { _cabreInstanceId         :: !(Maybe Text)
  , _cabreScheduleExpression :: !(Maybe Text)
  , _cabreOutputLocation     :: !(Maybe InstanceAssociationOutputLocation)
  , _cabreTargets            :: !(Maybe [Target])
  , _cabreParameters         :: !(Maybe (Map Text [Text]))
  , _cabreDocumentVersion    :: !(Maybe Text)
  , _cabreAssociationName    :: !(Maybe Text)
  , _cabreName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociationBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabreInstanceId' - The ID of the instance.
--
-- * 'cabreScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'cabreOutputLocation' - An Amazon S3 bucket where you want to store the results of this request.
--
-- * 'cabreTargets' - The instances targeted by the request.
--
-- * 'cabreParameters' - A description of the parameters for a document.
--
-- * 'cabreDocumentVersion' - The document version.
--
-- * 'cabreAssociationName' - Specify a descriptive name for the association.
--
-- * 'cabreName' - The name of the configuration document.
createAssociationBatchRequestEntry
    :: Text -- ^ 'cabreName'
    -> CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry pName_ =
  CreateAssociationBatchRequestEntry'
    { _cabreInstanceId = Nothing
    , _cabreScheduleExpression = Nothing
    , _cabreOutputLocation = Nothing
    , _cabreTargets = Nothing
    , _cabreParameters = Nothing
    , _cabreDocumentVersion = Nothing
    , _cabreAssociationName = Nothing
    , _cabreName = pName_
    }


-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\ s a -> s{_cabreInstanceId = a})

-- | A cron expression that specifies a schedule when the association runs.
cabreScheduleExpression :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreScheduleExpression = lens _cabreScheduleExpression (\ s a -> s{_cabreScheduleExpression = a})

-- | An Amazon S3 bucket where you want to store the results of this request.
cabreOutputLocation :: Lens' CreateAssociationBatchRequestEntry (Maybe InstanceAssociationOutputLocation)
cabreOutputLocation = lens _cabreOutputLocation (\ s a -> s{_cabreOutputLocation = a})

-- | The instances targeted by the request.
cabreTargets :: Lens' CreateAssociationBatchRequestEntry [Target]
cabreTargets = lens _cabreTargets (\ s a -> s{_cabreTargets = a}) . _Default . _Coerce

-- | A description of the parameters for a document.
cabreParameters :: Lens' CreateAssociationBatchRequestEntry (HashMap Text [Text])
cabreParameters = lens _cabreParameters (\ s a -> s{_cabreParameters = a}) . _Default . _Map

-- | The document version.
cabreDocumentVersion :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreDocumentVersion = lens _cabreDocumentVersion (\ s a -> s{_cabreDocumentVersion = a})

-- | Specify a descriptive name for the association.
cabreAssociationName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreAssociationName = lens _cabreAssociationName (\ s a -> s{_cabreAssociationName = a})

-- | The name of the configuration document.
cabreName :: Lens' CreateAssociationBatchRequestEntry Text
cabreName = lens _cabreName (\ s a -> s{_cabreName = a})

instance FromJSON CreateAssociationBatchRequestEntry
         where
        parseJSON
          = withObject "CreateAssociationBatchRequestEntry"
              (\ x ->
                 CreateAssociationBatchRequestEntry' <$>
                   (x .:? "InstanceId") <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "OutputLocation")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AssociationName")
                     <*> (x .: "Name"))

instance Hashable CreateAssociationBatchRequestEntry
         where

instance NFData CreateAssociationBatchRequestEntry
         where

instance ToJSON CreateAssociationBatchRequestEntry
         where
        toJSON CreateAssociationBatchRequestEntry'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _cabreInstanceId,
                  ("ScheduleExpression" .=) <$>
                    _cabreScheduleExpression,
                  ("OutputLocation" .=) <$> _cabreOutputLocation,
                  ("Targets" .=) <$> _cabreTargets,
                  ("Parameters" .=) <$> _cabreParameters,
                  ("DocumentVersion" .=) <$> _cabreDocumentVersion,
                  ("AssociationName" .=) <$> _cabreAssociationName,
                  Just ("Name" .= _cabreName)])

-- | Filter for the DescribeActivation API.
--
--
--
-- /See:/ 'describeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { _dafFilterKey    :: !(Maybe DescribeActivationsFilterKeys)
  , _dafFilterValues :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivationsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dafFilterKey' - The name of the filter.
--
-- * 'dafFilterValues' - The filter values.
describeActivationsFilter
    :: DescribeActivationsFilter
describeActivationsFilter =
  DescribeActivationsFilter'
    {_dafFilterKey = Nothing, _dafFilterValues = Nothing}


-- | The name of the filter.
dafFilterKey :: Lens' DescribeActivationsFilter (Maybe DescribeActivationsFilterKeys)
dafFilterKey = lens _dafFilterKey (\ s a -> s{_dafFilterKey = a})

-- | The filter values.
dafFilterValues :: Lens' DescribeActivationsFilter [Text]
dafFilterValues = lens _dafFilterValues (\ s a -> s{_dafFilterValues = a}) . _Default . _Coerce

instance Hashable DescribeActivationsFilter where

instance NFData DescribeActivationsFilter where

instance ToJSON DescribeActivationsFilter where
        toJSON DescribeActivationsFilter'{..}
          = object
              (catMaybes
                 [("FilterKey" .=) <$> _dafFilterKey,
                  ("FilterValues" .=) <$> _dafFilterValues])

-- | A default version of a document.
--
--
--
-- /See:/ 'documentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
  { _ddvdDefaultVersion :: !(Maybe Text)
  , _ddvdName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentDefaultVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvdDefaultVersion' - The default version of the document.
--
-- * 'ddvdName' - The name of the document.
documentDefaultVersionDescription
    :: DocumentDefaultVersionDescription
documentDefaultVersionDescription =
  DocumentDefaultVersionDescription'
    {_ddvdDefaultVersion = Nothing, _ddvdName = Nothing}


-- | The default version of the document.
ddvdDefaultVersion :: Lens' DocumentDefaultVersionDescription (Maybe Text)
ddvdDefaultVersion = lens _ddvdDefaultVersion (\ s a -> s{_ddvdDefaultVersion = a})

-- | The name of the document.
ddvdName :: Lens' DocumentDefaultVersionDescription (Maybe Text)
ddvdName = lens _ddvdName (\ s a -> s{_ddvdName = a})

instance FromJSON DocumentDefaultVersionDescription
         where
        parseJSON
          = withObject "DocumentDefaultVersionDescription"
              (\ x ->
                 DocumentDefaultVersionDescription' <$>
                   (x .:? "DefaultVersion") <*> (x .:? "Name"))

instance Hashable DocumentDefaultVersionDescription
         where

instance NFData DocumentDefaultVersionDescription
         where

-- | Describes a Systems Manager document.
--
--
--
-- /See:/ 'documentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
  { _dStatus          :: !(Maybe DocumentStatus)
  , _dDocumentType    :: !(Maybe DocumentType)
  , _dHash            :: !(Maybe Text)
  , _dSchemaVersion   :: !(Maybe Text)
  , _dSha1            :: !(Maybe Text)
  , _dDefaultVersion  :: !(Maybe Text)
  , _dTargetType      :: !(Maybe Text)
  , _dOwner           :: !(Maybe Text)
  , _dPlatformTypes   :: !(Maybe [PlatformType])
  , _dCreatedDate     :: !(Maybe POSIX)
  , _dDocumentFormat  :: !(Maybe DocumentFormat)
  , _dName            :: !(Maybe Text)
  , _dHashType        :: !(Maybe DocumentHashType)
  , _dParameters      :: !(Maybe [DocumentParameter])
  , _dDocumentVersion :: !(Maybe Text)
  , _dDescription     :: !(Maybe Text)
  , _dTags            :: !(Maybe [Tag])
  , _dLatestVersion   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus' - The status of the Systems Manager document.
--
-- * 'dDocumentType' - The type of document.
--
-- * 'dHash' - The Sha256 or Sha1 hash created by the system when the document was created.
--
-- * 'dSchemaVersion' - The schema version.
--
-- * 'dSha1' - The SHA1 hash of the document, which you can use for verification.
--
-- * 'dDefaultVersion' - The default version.
--
-- * 'dTargetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ .
--
-- * 'dOwner' - The AWS user account that created the document.
--
-- * 'dPlatformTypes' - The list of OS platforms compatible with this Systems Manager document.
--
-- * 'dCreatedDate' - The date when the document was created.
--
-- * 'dDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'dName' - The name of the Systems Manager document.
--
-- * 'dHashType' - Sha256 or Sha1.
--
-- * 'dParameters' - A description of the parameters for a document.
--
-- * 'dDocumentVersion' - The document version.
--
-- * 'dDescription' - A description of the document.
--
-- * 'dTags' - The tags, or metadata, that have been applied to the document.
--
-- * 'dLatestVersion' - The latest version of the document.
documentDescription
    :: DocumentDescription
documentDescription =
  DocumentDescription'
    { _dStatus = Nothing
    , _dDocumentType = Nothing
    , _dHash = Nothing
    , _dSchemaVersion = Nothing
    , _dSha1 = Nothing
    , _dDefaultVersion = Nothing
    , _dTargetType = Nothing
    , _dOwner = Nothing
    , _dPlatformTypes = Nothing
    , _dCreatedDate = Nothing
    , _dDocumentFormat = Nothing
    , _dName = Nothing
    , _dHashType = Nothing
    , _dParameters = Nothing
    , _dDocumentVersion = Nothing
    , _dDescription = Nothing
    , _dTags = Nothing
    , _dLatestVersion = Nothing
    }


-- | The status of the Systems Manager document.
dStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The type of document.
dDocumentType :: Lens' DocumentDescription (Maybe DocumentType)
dDocumentType = lens _dDocumentType (\ s a -> s{_dDocumentType = a})

-- | The Sha256 or Sha1 hash created by the system when the document was created.
dHash :: Lens' DocumentDescription (Maybe Text)
dHash = lens _dHash (\ s a -> s{_dHash = a})

-- | The schema version.
dSchemaVersion :: Lens' DocumentDescription (Maybe Text)
dSchemaVersion = lens _dSchemaVersion (\ s a -> s{_dSchemaVersion = a})

-- | The SHA1 hash of the document, which you can use for verification.
dSha1 :: Lens' DocumentDescription (Maybe Text)
dSha1 = lens _dSha1 (\ s a -> s{_dSha1 = a})

-- | The default version.
dDefaultVersion :: Lens' DocumentDescription (Maybe Text)
dDefaultVersion = lens _dDefaultVersion (\ s a -> s{_dDefaultVersion = a})

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ .
dTargetType :: Lens' DocumentDescription (Maybe Text)
dTargetType = lens _dTargetType (\ s a -> s{_dTargetType = a})

-- | The AWS user account that created the document.
dOwner :: Lens' DocumentDescription (Maybe Text)
dOwner = lens _dOwner (\ s a -> s{_dOwner = a})

-- | The list of OS platforms compatible with this Systems Manager document.
dPlatformTypes :: Lens' DocumentDescription [PlatformType]
dPlatformTypes = lens _dPlatformTypes (\ s a -> s{_dPlatformTypes = a}) . _Default . _Coerce

-- | The date when the document was created.
dCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time

-- | The document format, either JSON or YAML.
dDocumentFormat :: Lens' DocumentDescription (Maybe DocumentFormat)
dDocumentFormat = lens _dDocumentFormat (\ s a -> s{_dDocumentFormat = a})

-- | The name of the Systems Manager document.
dName :: Lens' DocumentDescription (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | Sha256 or Sha1.
dHashType :: Lens' DocumentDescription (Maybe DocumentHashType)
dHashType = lens _dHashType (\ s a -> s{_dHashType = a})

-- | A description of the parameters for a document.
dParameters :: Lens' DocumentDescription [DocumentParameter]
dParameters = lens _dParameters (\ s a -> s{_dParameters = a}) . _Default . _Coerce

-- | The document version.
dDocumentVersion :: Lens' DocumentDescription (Maybe Text)
dDocumentVersion = lens _dDocumentVersion (\ s a -> s{_dDocumentVersion = a})

-- | A description of the document.
dDescription :: Lens' DocumentDescription (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a})

-- | The tags, or metadata, that have been applied to the document.
dTags :: Lens' DocumentDescription [Tag]
dTags = lens _dTags (\ s a -> s{_dTags = a}) . _Default . _Coerce

-- | The latest version of the document.
dLatestVersion :: Lens' DocumentDescription (Maybe Text)
dLatestVersion = lens _dLatestVersion (\ s a -> s{_dLatestVersion = a})

instance FromJSON DocumentDescription where
        parseJSON
          = withObject "DocumentDescription"
              (\ x ->
                 DocumentDescription' <$>
                   (x .:? "Status") <*> (x .:? "DocumentType") <*>
                     (x .:? "Hash")
                     <*> (x .:? "SchemaVersion")
                     <*> (x .:? "Sha1")
                     <*> (x .:? "DefaultVersion")
                     <*> (x .:? "TargetType")
                     <*> (x .:? "Owner")
                     <*> (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "DocumentFormat")
                     <*> (x .:? "Name")
                     <*> (x .:? "HashType")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "Description")
                     <*> (x .:? "Tags" .!= mempty)
                     <*> (x .:? "LatestVersion"))

instance Hashable DocumentDescription where

instance NFData DocumentDescription where

-- | Describes a filter.
--
--
--
-- /See:/ 'documentFilter' smart constructor.
data DocumentFilter = DocumentFilter'
  { _dfKey   :: !DocumentFilterKey
  , _dfValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfKey' - The name of the filter.
--
-- * 'dfValue' - The value of the filter.
documentFilter
    :: DocumentFilterKey -- ^ 'dfKey'
    -> Text -- ^ 'dfValue'
    -> DocumentFilter
documentFilter pKey_ pValue_ =
  DocumentFilter' {_dfKey = pKey_, _dfValue = pValue_}


-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\ s a -> s{_dfKey = a})

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\ s a -> s{_dfValue = a})

instance Hashable DocumentFilter where

instance NFData DocumentFilter where

instance ToJSON DocumentFilter where
        toJSON DocumentFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _dfKey), Just ("value" .= _dfValue)])

-- | Describes the name of a Systems Manager document.
--
--
--
-- /See:/ 'documentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
  { _diDocumentType    :: !(Maybe DocumentType)
  , _diSchemaVersion   :: !(Maybe Text)
  , _diTargetType      :: !(Maybe Text)
  , _diOwner           :: !(Maybe Text)
  , _diPlatformTypes   :: !(Maybe [PlatformType])
  , _diDocumentFormat  :: !(Maybe DocumentFormat)
  , _diName            :: !(Maybe Text)
  , _diDocumentVersion :: !(Maybe Text)
  , _diTags            :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDocumentType' - The document type.
--
-- * 'diSchemaVersion' - The schema version.
--
-- * 'diTargetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ .
--
-- * 'diOwner' - The AWS user account that created the document.
--
-- * 'diPlatformTypes' - The operating system platform.
--
-- * 'diDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'diName' - The name of the Systems Manager document.
--
-- * 'diDocumentVersion' - The document version.
--
-- * 'diTags' - The tags, or metadata, that have been applied to the document.
documentIdentifier
    :: DocumentIdentifier
documentIdentifier =
  DocumentIdentifier'
    { _diDocumentType = Nothing
    , _diSchemaVersion = Nothing
    , _diTargetType = Nothing
    , _diOwner = Nothing
    , _diPlatformTypes = Nothing
    , _diDocumentFormat = Nothing
    , _diName = Nothing
    , _diDocumentVersion = Nothing
    , _diTags = Nothing
    }


-- | The document type.
diDocumentType :: Lens' DocumentIdentifier (Maybe DocumentType)
diDocumentType = lens _diDocumentType (\ s a -> s{_diDocumentType = a})

-- | The schema version.
diSchemaVersion :: Lens' DocumentIdentifier (Maybe Text)
diSchemaVersion = lens _diSchemaVersion (\ s a -> s{_diSchemaVersion = a})

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ .
diTargetType :: Lens' DocumentIdentifier (Maybe Text)
diTargetType = lens _diTargetType (\ s a -> s{_diTargetType = a})

-- | The AWS user account that created the document.
diOwner :: Lens' DocumentIdentifier (Maybe Text)
diOwner = lens _diOwner (\ s a -> s{_diOwner = a})

-- | The operating system platform.
diPlatformTypes :: Lens' DocumentIdentifier [PlatformType]
diPlatformTypes = lens _diPlatformTypes (\ s a -> s{_diPlatformTypes = a}) . _Default . _Coerce

-- | The document format, either JSON or YAML.
diDocumentFormat :: Lens' DocumentIdentifier (Maybe DocumentFormat)
diDocumentFormat = lens _diDocumentFormat (\ s a -> s{_diDocumentFormat = a})

-- | The name of the Systems Manager document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a})

-- | The document version.
diDocumentVersion :: Lens' DocumentIdentifier (Maybe Text)
diDocumentVersion = lens _diDocumentVersion (\ s a -> s{_diDocumentVersion = a})

-- | The tags, or metadata, that have been applied to the document.
diTags :: Lens' DocumentIdentifier [Tag]
diTags = lens _diTags (\ s a -> s{_diTags = a}) . _Default . _Coerce

instance FromJSON DocumentIdentifier where
        parseJSON
          = withObject "DocumentIdentifier"
              (\ x ->
                 DocumentIdentifier' <$>
                   (x .:? "DocumentType") <*> (x .:? "SchemaVersion")
                     <*> (x .:? "TargetType")
                     <*> (x .:? "Owner")
                     <*> (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "DocumentFormat")
                     <*> (x .:? "Name")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable DocumentIdentifier where

instance NFData DocumentIdentifier where

-- | One or more filters. Use a filter to return a more specific list of documents.
--
--
-- For keys, you can specify one or more tags that have been applied to a document.
--
-- Other valid values include Owner, Name, PlatformTypes, and DocumentType.
--
-- Note that only one Owner can be specified in a request. For example: @Key=Owner,Values=Self@ .
--
-- If you use Name as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with @Te@ , run the following command:
--
-- @aws ssm list-documents --filters Key=Name,Values=Te@
--
-- If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.
--
-- To specify a custom key and value pair, use the format @Key=tag:[tagName],Values=[valueName]@ .
--
-- For example, if you created a Key called region and are using the AWS CLI to call the @list-documents@ command:
--
-- @aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self@
--
--
-- /See:/ 'documentKeyValuesFilter' smart constructor.
data DocumentKeyValuesFilter = DocumentKeyValuesFilter'
  { _dkvfValues :: !(Maybe [Text])
  , _dkvfKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentKeyValuesFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkvfValues' - The value for the filter key.
--
-- * 'dkvfKey' - The name of the filter key.
documentKeyValuesFilter
    :: DocumentKeyValuesFilter
documentKeyValuesFilter =
  DocumentKeyValuesFilter' {_dkvfValues = Nothing, _dkvfKey = Nothing}


-- | The value for the filter key.
dkvfValues :: Lens' DocumentKeyValuesFilter [Text]
dkvfValues = lens _dkvfValues (\ s a -> s{_dkvfValues = a}) . _Default . _Coerce

-- | The name of the filter key.
dkvfKey :: Lens' DocumentKeyValuesFilter (Maybe Text)
dkvfKey = lens _dkvfKey (\ s a -> s{_dkvfKey = a})

instance Hashable DocumentKeyValuesFilter where

instance NFData DocumentKeyValuesFilter where

instance ToJSON DocumentKeyValuesFilter where
        toJSON DocumentKeyValuesFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _dkvfValues,
                  ("Key" .=) <$> _dkvfKey])

-- | Parameters specified in a System Manager document that execute on the server when the command is run.
--
--
--
-- /See:/ 'documentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
  { _dpName         :: !(Maybe Text)
  , _dpDefaultValue :: !(Maybe Text)
  , _dpType         :: !(Maybe DocumentParameterType)
  , _dpDescription  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpName' - The name of the parameter.
--
-- * 'dpDefaultValue' - If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
--
-- * 'dpType' - The type of parameter. The type can be either String or StringList.
--
-- * 'dpDescription' - A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
documentParameter
    :: DocumentParameter
documentParameter =
  DocumentParameter'
    { _dpName = Nothing
    , _dpDefaultValue = Nothing
    , _dpType = Nothing
    , _dpDescription = Nothing
    }


-- | The name of the parameter.
dpName :: Lens' DocumentParameter (Maybe Text)
dpName = lens _dpName (\ s a -> s{_dpName = a})

-- | If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
dpDefaultValue :: Lens' DocumentParameter (Maybe Text)
dpDefaultValue = lens _dpDefaultValue (\ s a -> s{_dpDefaultValue = a})

-- | The type of parameter. The type can be either String or StringList.
dpType :: Lens' DocumentParameter (Maybe DocumentParameterType)
dpType = lens _dpType (\ s a -> s{_dpType = a})

-- | A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
dpDescription :: Lens' DocumentParameter (Maybe Text)
dpDescription = lens _dpDescription (\ s a -> s{_dpDescription = a})

instance FromJSON DocumentParameter where
        parseJSON
          = withObject "DocumentParameter"
              (\ x ->
                 DocumentParameter' <$>
                   (x .:? "Name") <*> (x .:? "DefaultValue") <*>
                     (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable DocumentParameter where

instance NFData DocumentParameter where

-- | Version information about the document.
--
--
--
-- /See:/ 'documentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { _dviCreatedDate      :: !(Maybe POSIX)
  , _dviDocumentFormat   :: !(Maybe DocumentFormat)
  , _dviName             :: !(Maybe Text)
  , _dviDocumentVersion  :: !(Maybe Text)
  , _dviIsDefaultVersion :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentVersionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dviCreatedDate' - The date the document was created.
--
-- * 'dviDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'dviName' - The document name.
--
-- * 'dviDocumentVersion' - The document version.
--
-- * 'dviIsDefaultVersion' - An identifier for the default version of the document.
documentVersionInfo
    :: DocumentVersionInfo
documentVersionInfo =
  DocumentVersionInfo'
    { _dviCreatedDate = Nothing
    , _dviDocumentFormat = Nothing
    , _dviName = Nothing
    , _dviDocumentVersion = Nothing
    , _dviIsDefaultVersion = Nothing
    }


-- | The date the document was created.
dviCreatedDate :: Lens' DocumentVersionInfo (Maybe UTCTime)
dviCreatedDate = lens _dviCreatedDate (\ s a -> s{_dviCreatedDate = a}) . mapping _Time

-- | The document format, either JSON or YAML.
dviDocumentFormat :: Lens' DocumentVersionInfo (Maybe DocumentFormat)
dviDocumentFormat = lens _dviDocumentFormat (\ s a -> s{_dviDocumentFormat = a})

-- | The document name.
dviName :: Lens' DocumentVersionInfo (Maybe Text)
dviName = lens _dviName (\ s a -> s{_dviName = a})

-- | The document version.
dviDocumentVersion :: Lens' DocumentVersionInfo (Maybe Text)
dviDocumentVersion = lens _dviDocumentVersion (\ s a -> s{_dviDocumentVersion = a})

-- | An identifier for the default version of the document.
dviIsDefaultVersion :: Lens' DocumentVersionInfo (Maybe Bool)
dviIsDefaultVersion = lens _dviIsDefaultVersion (\ s a -> s{_dviIsDefaultVersion = a})

instance FromJSON DocumentVersionInfo where
        parseJSON
          = withObject "DocumentVersionInfo"
              (\ x ->
                 DocumentVersionInfo' <$>
                   (x .:? "CreatedDate") <*> (x .:? "DocumentFormat")
                     <*> (x .:? "Name")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "IsDefaultVersion"))

instance Hashable DocumentVersionInfo where

instance NFData DocumentVersionInfo where

-- | The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
--
--
--
-- /See:/ 'effectivePatch' smart constructor.
data EffectivePatch = EffectivePatch'
  { _epPatch       :: !(Maybe Patch)
  , _epPatchStatus :: !(Maybe PatchStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EffectivePatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epPatch' - Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
--
-- * 'epPatchStatus' - The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
effectivePatch
    :: EffectivePatch
effectivePatch = EffectivePatch' {_epPatch = Nothing, _epPatchStatus = Nothing}


-- | Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
epPatch :: Lens' EffectivePatch (Maybe Patch)
epPatch = lens _epPatch (\ s a -> s{_epPatch = a})

-- | The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
epPatchStatus :: Lens' EffectivePatch (Maybe PatchStatus)
epPatchStatus = lens _epPatchStatus (\ s a -> s{_epPatchStatus = a})

instance FromJSON EffectivePatch where
        parseJSON
          = withObject "EffectivePatch"
              (\ x ->
                 EffectivePatch' <$>
                   (x .:? "Patch") <*> (x .:? "PatchStatus"))

instance Hashable EffectivePatch where

instance NFData EffectivePatch where

-- | Describes a failed association.
--
--
--
-- /See:/ 'failedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { _fcaEntry   :: !(Maybe CreateAssociationBatchRequestEntry)
  , _fcaFault   :: !(Maybe Fault)
  , _fcaMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FailedCreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcaEntry' - The association.
--
-- * 'fcaFault' - The source of the failure.
--
-- * 'fcaMessage' - A description of the failure.
failedCreateAssociation
    :: FailedCreateAssociation
failedCreateAssociation =
  FailedCreateAssociation'
    {_fcaEntry = Nothing, _fcaFault = Nothing, _fcaMessage = Nothing}


-- | The association.
fcaEntry :: Lens' FailedCreateAssociation (Maybe CreateAssociationBatchRequestEntry)
fcaEntry = lens _fcaEntry (\ s a -> s{_fcaEntry = a})

-- | The source of the failure.
fcaFault :: Lens' FailedCreateAssociation (Maybe Fault)
fcaFault = lens _fcaFault (\ s a -> s{_fcaFault = a})

-- | A description of the failure.
fcaMessage :: Lens' FailedCreateAssociation (Maybe Text)
fcaMessage = lens _fcaMessage (\ s a -> s{_fcaMessage = a})

instance FromJSON FailedCreateAssociation where
        parseJSON
          = withObject "FailedCreateAssociation"
              (\ x ->
                 FailedCreateAssociation' <$>
                   (x .:? "Entry") <*> (x .:? "Fault") <*>
                     (x .:? "Message"))

instance Hashable FailedCreateAssociation where

instance NFData FailedCreateAssociation where

-- | Information about an Automation failure.
--
--
--
-- /See:/ 'failureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { _fdFailureType  :: !(Maybe Text)
  , _fdFailureStage :: !(Maybe Text)
  , _fdDetails      :: !(Maybe (Map Text [Text]))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdFailureType' - The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
--
-- * 'fdFailureStage' - The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
--
-- * 'fdDetails' - Detailed information about the Automation step failure.
failureDetails
    :: FailureDetails
failureDetails =
  FailureDetails'
    {_fdFailureType = Nothing, _fdFailureStage = Nothing, _fdDetails = Nothing}


-- | The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
fdFailureType :: Lens' FailureDetails (Maybe Text)
fdFailureType = lens _fdFailureType (\ s a -> s{_fdFailureType = a})

-- | The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
fdFailureStage :: Lens' FailureDetails (Maybe Text)
fdFailureStage = lens _fdFailureStage (\ s a -> s{_fdFailureStage = a})

-- | Detailed information about the Automation step failure.
fdDetails :: Lens' FailureDetails (HashMap Text [Text])
fdDetails = lens _fdDetails (\ s a -> s{_fdDetails = a}) . _Default . _Map

instance FromJSON FailureDetails where
        parseJSON
          = withObject "FailureDetails"
              (\ x ->
                 FailureDetails' <$>
                   (x .:? "FailureType") <*> (x .:? "FailureStage") <*>
                     (x .:? "Details" .!= mempty))

instance Hashable FailureDetails where

instance NFData FailureDetails where

-- | Status information about the aggregated associations.
--
--
--
-- /See:/ 'instanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { _iaaoDetailedStatus                           :: !(Maybe Text)
  , _iaaoInstanceAssociationStatusAggregatedCount :: !(Maybe (Map Text Int))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceAggregatedAssociationOverview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaaoDetailedStatus' - Detailed status information about the aggregated associations.
--
-- * 'iaaoInstanceAssociationStatusAggregatedCount' - The number of associations for the instance(s).
instanceAggregatedAssociationOverview
    :: InstanceAggregatedAssociationOverview
instanceAggregatedAssociationOverview =
  InstanceAggregatedAssociationOverview'
    { _iaaoDetailedStatus = Nothing
    , _iaaoInstanceAssociationStatusAggregatedCount = Nothing
    }


-- | Detailed status information about the aggregated associations.
iaaoDetailedStatus :: Lens' InstanceAggregatedAssociationOverview (Maybe Text)
iaaoDetailedStatus = lens _iaaoDetailedStatus (\ s a -> s{_iaaoDetailedStatus = a})

-- | The number of associations for the instance(s).
iaaoInstanceAssociationStatusAggregatedCount :: Lens' InstanceAggregatedAssociationOverview (HashMap Text Int)
iaaoInstanceAssociationStatusAggregatedCount = lens _iaaoInstanceAssociationStatusAggregatedCount (\ s a -> s{_iaaoInstanceAssociationStatusAggregatedCount = a}) . _Default . _Map

instance FromJSON
           InstanceAggregatedAssociationOverview
         where
        parseJSON
          = withObject "InstanceAggregatedAssociationOverview"
              (\ x ->
                 InstanceAggregatedAssociationOverview' <$>
                   (x .:? "DetailedStatus") <*>
                     (x .:? "InstanceAssociationStatusAggregatedCount" .!=
                        mempty))

instance Hashable
           InstanceAggregatedAssociationOverview
         where

instance NFData InstanceAggregatedAssociationOverview
         where

-- | One or more association documents on the instance.
--
--
--
-- /See:/ 'instanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
  { _iaAssociationId      :: !(Maybe Text)
  , _iaInstanceId         :: !(Maybe Text)
  , _iaContent            :: !(Maybe Text)
  , _iaAssociationVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaAssociationId' - The association ID.
--
-- * 'iaInstanceId' - The instance ID.
--
-- * 'iaContent' - The content of the association document for the instance(s).
--
-- * 'iaAssociationVersion' - Version information for the association on the instance.
instanceAssociation
    :: InstanceAssociation
instanceAssociation =
  InstanceAssociation'
    { _iaAssociationId = Nothing
    , _iaInstanceId = Nothing
    , _iaContent = Nothing
    , _iaAssociationVersion = Nothing
    }


-- | The association ID.
iaAssociationId :: Lens' InstanceAssociation (Maybe Text)
iaAssociationId = lens _iaAssociationId (\ s a -> s{_iaAssociationId = a})

-- | The instance ID.
iaInstanceId :: Lens' InstanceAssociation (Maybe Text)
iaInstanceId = lens _iaInstanceId (\ s a -> s{_iaInstanceId = a})

-- | The content of the association document for the instance(s).
iaContent :: Lens' InstanceAssociation (Maybe Text)
iaContent = lens _iaContent (\ s a -> s{_iaContent = a})

-- | Version information for the association on the instance.
iaAssociationVersion :: Lens' InstanceAssociation (Maybe Text)
iaAssociationVersion = lens _iaAssociationVersion (\ s a -> s{_iaAssociationVersion = a})

instance FromJSON InstanceAssociation where
        parseJSON
          = withObject "InstanceAssociation"
              (\ x ->
                 InstanceAssociation' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Content")
                     <*> (x .:? "AssociationVersion"))

instance Hashable InstanceAssociation where

instance NFData InstanceAssociation where

-- | An Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 'instanceAssociationOutputLocation' smart constructor.
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { _iaolS3Location :: Maybe S3OutputLocation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceAssociationOutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaolS3Location' - An Amazon S3 bucket where you want to store the results of this request.
instanceAssociationOutputLocation
    :: InstanceAssociationOutputLocation
instanceAssociationOutputLocation =
  InstanceAssociationOutputLocation' {_iaolS3Location = Nothing}


-- | An Amazon S3 bucket where you want to store the results of this request.
iaolS3Location :: Lens' InstanceAssociationOutputLocation (Maybe S3OutputLocation)
iaolS3Location = lens _iaolS3Location (\ s a -> s{_iaolS3Location = a})

instance FromJSON InstanceAssociationOutputLocation
         where
        parseJSON
          = withObject "InstanceAssociationOutputLocation"
              (\ x ->
                 InstanceAssociationOutputLocation' <$>
                   (x .:? "S3Location"))

instance Hashable InstanceAssociationOutputLocation
         where

instance NFData InstanceAssociationOutputLocation
         where

instance ToJSON InstanceAssociationOutputLocation
         where
        toJSON InstanceAssociationOutputLocation'{..}
          = object
              (catMaybes [("S3Location" .=) <$> _iaolS3Location])

-- | The URL of Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 'instanceAssociationOutputURL' smart constructor.
newtype InstanceAssociationOutputURL = InstanceAssociationOutputURL'
  { _iaouS3OutputURL :: Maybe S3OutputURL
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceAssociationOutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaouS3OutputURL' - The URL of Amazon S3 bucket where you want to store the results of this request.
instanceAssociationOutputURL
    :: InstanceAssociationOutputURL
instanceAssociationOutputURL =
  InstanceAssociationOutputURL' {_iaouS3OutputURL = Nothing}


-- | The URL of Amazon S3 bucket where you want to store the results of this request.
iaouS3OutputURL :: Lens' InstanceAssociationOutputURL (Maybe S3OutputURL)
iaouS3OutputURL = lens _iaouS3OutputURL (\ s a -> s{_iaouS3OutputURL = a})

instance FromJSON InstanceAssociationOutputURL where
        parseJSON
          = withObject "InstanceAssociationOutputURL"
              (\ x ->
                 InstanceAssociationOutputURL' <$>
                   (x .:? "S3OutputUrl"))

instance Hashable InstanceAssociationOutputURL where

instance NFData InstanceAssociationOutputURL where

-- | Status information about the instance association.
--
--
--
-- /See:/ 'instanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { _iasiAssociationId      :: !(Maybe Text)
  , _iasiInstanceId         :: !(Maybe Text)
  , _iasiDetailedStatus     :: !(Maybe Text)
  , _iasiStatus             :: !(Maybe Text)
  , _iasiOutputURL          :: !(Maybe InstanceAssociationOutputURL)
  , _iasiExecutionSummary   :: !(Maybe Text)
  , _iasiName               :: !(Maybe Text)
  , _iasiErrorCode          :: !(Maybe Text)
  , _iasiDocumentVersion    :: !(Maybe Text)
  , _iasiAssociationVersion :: !(Maybe Text)
  , _iasiExecutionDate      :: !(Maybe POSIX)
  , _iasiAssociationName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceAssociationStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iasiAssociationId' - The association ID.
--
-- * 'iasiInstanceId' - The instance ID where the association was created.
--
-- * 'iasiDetailedStatus' - Detailed status information about the instance association.
--
-- * 'iasiStatus' - Status information about the instance association.
--
-- * 'iasiOutputURL' - A URL for an Amazon S3 bucket where you want to store the results of this request.
--
-- * 'iasiExecutionSummary' - Summary information about association execution.
--
-- * 'iasiName' - The name of the association.
--
-- * 'iasiErrorCode' - An error code returned by the request to create the association.
--
-- * 'iasiDocumentVersion' - The association document verions.
--
-- * 'iasiAssociationVersion' - The version of the association applied to the instance.
--
-- * 'iasiExecutionDate' - The date the instance association executed.
--
-- * 'iasiAssociationName' - The name of the association applied to the instance.
instanceAssociationStatusInfo
    :: InstanceAssociationStatusInfo
instanceAssociationStatusInfo =
  InstanceAssociationStatusInfo'
    { _iasiAssociationId = Nothing
    , _iasiInstanceId = Nothing
    , _iasiDetailedStatus = Nothing
    , _iasiStatus = Nothing
    , _iasiOutputURL = Nothing
    , _iasiExecutionSummary = Nothing
    , _iasiName = Nothing
    , _iasiErrorCode = Nothing
    , _iasiDocumentVersion = Nothing
    , _iasiAssociationVersion = Nothing
    , _iasiExecutionDate = Nothing
    , _iasiAssociationName = Nothing
    }


-- | The association ID.
iasiAssociationId :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationId = lens _iasiAssociationId (\ s a -> s{_iasiAssociationId = a})

-- | The instance ID where the association was created.
iasiInstanceId :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiInstanceId = lens _iasiInstanceId (\ s a -> s{_iasiInstanceId = a})

-- | Detailed status information about the instance association.
iasiDetailedStatus :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiDetailedStatus = lens _iasiDetailedStatus (\ s a -> s{_iasiDetailedStatus = a})

-- | Status information about the instance association.
iasiStatus :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiStatus = lens _iasiStatus (\ s a -> s{_iasiStatus = a})

-- | A URL for an Amazon S3 bucket where you want to store the results of this request.
iasiOutputURL :: Lens' InstanceAssociationStatusInfo (Maybe InstanceAssociationOutputURL)
iasiOutputURL = lens _iasiOutputURL (\ s a -> s{_iasiOutputURL = a})

-- | Summary information about association execution.
iasiExecutionSummary :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiExecutionSummary = lens _iasiExecutionSummary (\ s a -> s{_iasiExecutionSummary = a})

-- | The name of the association.
iasiName :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiName = lens _iasiName (\ s a -> s{_iasiName = a})

-- | An error code returned by the request to create the association.
iasiErrorCode :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiErrorCode = lens _iasiErrorCode (\ s a -> s{_iasiErrorCode = a})

-- | The association document verions.
iasiDocumentVersion :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiDocumentVersion = lens _iasiDocumentVersion (\ s a -> s{_iasiDocumentVersion = a})

-- | The version of the association applied to the instance.
iasiAssociationVersion :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationVersion = lens _iasiAssociationVersion (\ s a -> s{_iasiAssociationVersion = a})

-- | The date the instance association executed.
iasiExecutionDate :: Lens' InstanceAssociationStatusInfo (Maybe UTCTime)
iasiExecutionDate = lens _iasiExecutionDate (\ s a -> s{_iasiExecutionDate = a}) . mapping _Time

-- | The name of the association applied to the instance.
iasiAssociationName :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationName = lens _iasiAssociationName (\ s a -> s{_iasiAssociationName = a})

instance FromJSON InstanceAssociationStatusInfo where
        parseJSON
          = withObject "InstanceAssociationStatusInfo"
              (\ x ->
                 InstanceAssociationStatusInfo' <$>
                   (x .:? "AssociationId") <*> (x .:? "InstanceId") <*>
                     (x .:? "DetailedStatus")
                     <*> (x .:? "Status")
                     <*> (x .:? "OutputUrl")
                     <*> (x .:? "ExecutionSummary")
                     <*> (x .:? "Name")
                     <*> (x .:? "ErrorCode")
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "AssociationVersion")
                     <*> (x .:? "ExecutionDate")
                     <*> (x .:? "AssociationName"))

instance Hashable InstanceAssociationStatusInfo where

instance NFData InstanceAssociationStatusInfo where

-- | Describes a filter for a specific list of instances.
--
--
--
-- /See:/ 'instanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
  { _iiInstanceId :: !(Maybe Text)
  , _iiPingStatus :: !(Maybe PingStatus)
  , _iiIPAddress :: !(Maybe Text)
  , _iiResourceType :: !(Maybe ResourceType)
  , _iiRegistrationDate :: !(Maybe POSIX)
  , _iiPlatformVersion :: !(Maybe Text)
  , _iiIsLatestVersion :: !(Maybe Bool)
  , _iiAgentVersion :: !(Maybe Text)
  , _iiLastPingDateTime :: !(Maybe POSIX)
  , _iiLastSuccessfulAssociationExecutionDate :: !(Maybe POSIX)
  , _iiActivationId :: !(Maybe Text)
  , _iiName :: !(Maybe Text)
  , _iiPlatformType :: !(Maybe PlatformType)
  , _iiAssociationOverview :: !(Maybe InstanceAggregatedAssociationOverview)
  , _iiAssociationStatus :: !(Maybe Text)
  , _iiLastAssociationExecutionDate :: !(Maybe POSIX)
  , _iiPlatformName :: !(Maybe Text)
  , _iiComputerName :: !(Maybe Text)
  , _iiIAMRole :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInstanceId' - The instance ID.
--
-- * 'iiPingStatus' - Connection status of the SSM Agent.
--
-- * 'iiIPAddress' - The IP address of the managed instance.
--
-- * 'iiResourceType' - The type of instance. Instances are either EC2 instances or managed instances.
--
-- * 'iiRegistrationDate' - The date the server or VM was registered with AWS as a managed instance.
--
-- * 'iiPlatformVersion' - The version of the OS platform running on your instance.
--
-- * 'iiIsLatestVersion' - Indicates whether latest version of the SSM Agent is running on your instance. Some older versions of Windows Server use the EC2Config service to process SSM requests. For this reason, this field does not indicate whether or not the latest version is installed on Windows managed instances.
--
-- * 'iiAgentVersion' - The version of the SSM Agent running on your Linux instance.
--
-- * 'iiLastPingDateTime' - The date and time when agent last pinged Systems Manager service.
--
-- * 'iiLastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
--
-- * 'iiActivationId' - The activation ID created by Systems Manager when the server or VM was registered.
--
-- * 'iiName' - The name of the managed instance.
--
-- * 'iiPlatformType' - The operating system platform type.
--
-- * 'iiAssociationOverview' - Information about the association.
--
-- * 'iiAssociationStatus' - The status of the association.
--
-- * 'iiLastAssociationExecutionDate' - The date the association was last executed.
--
-- * 'iiPlatformName' - The name of the operating system platform running on your instance.
--
-- * 'iiComputerName' - The fully qualified host name of the managed instance.
--
-- * 'iiIAMRole' - The Amazon Identity and Access Management (IAM) role assigned to EC2 instances or managed instances.
instanceInformation
    :: InstanceInformation
instanceInformation =
  InstanceInformation'
    { _iiInstanceId = Nothing
    , _iiPingStatus = Nothing
    , _iiIPAddress = Nothing
    , _iiResourceType = Nothing
    , _iiRegistrationDate = Nothing
    , _iiPlatformVersion = Nothing
    , _iiIsLatestVersion = Nothing
    , _iiAgentVersion = Nothing
    , _iiLastPingDateTime = Nothing
    , _iiLastSuccessfulAssociationExecutionDate = Nothing
    , _iiActivationId = Nothing
    , _iiName = Nothing
    , _iiPlatformType = Nothing
    , _iiAssociationOverview = Nothing
    , _iiAssociationStatus = Nothing
    , _iiLastAssociationExecutionDate = Nothing
    , _iiPlatformName = Nothing
    , _iiComputerName = Nothing
    , _iiIAMRole = Nothing
    }


-- | The instance ID.
iiInstanceId :: Lens' InstanceInformation (Maybe Text)
iiInstanceId = lens _iiInstanceId (\ s a -> s{_iiInstanceId = a})

-- | Connection status of the SSM Agent.
iiPingStatus :: Lens' InstanceInformation (Maybe PingStatus)
iiPingStatus = lens _iiPingStatus (\ s a -> s{_iiPingStatus = a})

-- | The IP address of the managed instance.
iiIPAddress :: Lens' InstanceInformation (Maybe Text)
iiIPAddress = lens _iiIPAddress (\ s a -> s{_iiIPAddress = a})

-- | The type of instance. Instances are either EC2 instances or managed instances.
iiResourceType :: Lens' InstanceInformation (Maybe ResourceType)
iiResourceType = lens _iiResourceType (\ s a -> s{_iiResourceType = a})

-- | The date the server or VM was registered with AWS as a managed instance.
iiRegistrationDate :: Lens' InstanceInformation (Maybe UTCTime)
iiRegistrationDate = lens _iiRegistrationDate (\ s a -> s{_iiRegistrationDate = a}) . mapping _Time

-- | The version of the OS platform running on your instance.
iiPlatformVersion :: Lens' InstanceInformation (Maybe Text)
iiPlatformVersion = lens _iiPlatformVersion (\ s a -> s{_iiPlatformVersion = a})

-- | Indicates whether latest version of the SSM Agent is running on your instance. Some older versions of Windows Server use the EC2Config service to process SSM requests. For this reason, this field does not indicate whether or not the latest version is installed on Windows managed instances.
iiIsLatestVersion :: Lens' InstanceInformation (Maybe Bool)
iiIsLatestVersion = lens _iiIsLatestVersion (\ s a -> s{_iiIsLatestVersion = a})

-- | The version of the SSM Agent running on your Linux instance.
iiAgentVersion :: Lens' InstanceInformation (Maybe Text)
iiAgentVersion = lens _iiAgentVersion (\ s a -> s{_iiAgentVersion = a})

-- | The date and time when agent last pinged Systems Manager service.
iiLastPingDateTime :: Lens' InstanceInformation (Maybe UTCTime)
iiLastPingDateTime = lens _iiLastPingDateTime (\ s a -> s{_iiLastPingDateTime = a}) . mapping _Time

-- | The last date the association was successfully run.
iiLastSuccessfulAssociationExecutionDate :: Lens' InstanceInformation (Maybe UTCTime)
iiLastSuccessfulAssociationExecutionDate = lens _iiLastSuccessfulAssociationExecutionDate (\ s a -> s{_iiLastSuccessfulAssociationExecutionDate = a}) . mapping _Time

-- | The activation ID created by Systems Manager when the server or VM was registered.
iiActivationId :: Lens' InstanceInformation (Maybe Text)
iiActivationId = lens _iiActivationId (\ s a -> s{_iiActivationId = a})

-- | The name of the managed instance.
iiName :: Lens' InstanceInformation (Maybe Text)
iiName = lens _iiName (\ s a -> s{_iiName = a})

-- | The operating system platform type.
iiPlatformType :: Lens' InstanceInformation (Maybe PlatformType)
iiPlatformType = lens _iiPlatformType (\ s a -> s{_iiPlatformType = a})

-- | Information about the association.
iiAssociationOverview :: Lens' InstanceInformation (Maybe InstanceAggregatedAssociationOverview)
iiAssociationOverview = lens _iiAssociationOverview (\ s a -> s{_iiAssociationOverview = a})

-- | The status of the association.
iiAssociationStatus :: Lens' InstanceInformation (Maybe Text)
iiAssociationStatus = lens _iiAssociationStatus (\ s a -> s{_iiAssociationStatus = a})

-- | The date the association was last executed.
iiLastAssociationExecutionDate :: Lens' InstanceInformation (Maybe UTCTime)
iiLastAssociationExecutionDate = lens _iiLastAssociationExecutionDate (\ s a -> s{_iiLastAssociationExecutionDate = a}) . mapping _Time

-- | The name of the operating system platform running on your instance.
iiPlatformName :: Lens' InstanceInformation (Maybe Text)
iiPlatformName = lens _iiPlatformName (\ s a -> s{_iiPlatformName = a})

-- | The fully qualified host name of the managed instance.
iiComputerName :: Lens' InstanceInformation (Maybe Text)
iiComputerName = lens _iiComputerName (\ s a -> s{_iiComputerName = a})

-- | The Amazon Identity and Access Management (IAM) role assigned to EC2 instances or managed instances.
iiIAMRole :: Lens' InstanceInformation (Maybe Text)
iiIAMRole = lens _iiIAMRole (\ s a -> s{_iiIAMRole = a})

instance FromJSON InstanceInformation where
        parseJSON
          = withObject "InstanceInformation"
              (\ x ->
                 InstanceInformation' <$>
                   (x .:? "InstanceId") <*> (x .:? "PingStatus") <*>
                     (x .:? "IPAddress")
                     <*> (x .:? "ResourceType")
                     <*> (x .:? "RegistrationDate")
                     <*> (x .:? "PlatformVersion")
                     <*> (x .:? "IsLatestVersion")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "LastPingDateTime")
                     <*> (x .:? "LastSuccessfulAssociationExecutionDate")
                     <*> (x .:? "ActivationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "PlatformType")
                     <*> (x .:? "AssociationOverview")
                     <*> (x .:? "AssociationStatus")
                     <*> (x .:? "LastAssociationExecutionDate")
                     <*> (x .:? "PlatformName")
                     <*> (x .:? "ComputerName")
                     <*> (x .:? "IamRole"))

instance Hashable InstanceInformation where

instance NFData InstanceInformation where

-- | Describes a filter for a specific list of instances.
--
--
--
-- /See:/ 'instanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { _iifKey      :: !InstanceInformationFilterKey
  , _iifValueSet :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceInformationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iifKey' - The name of the filter.
--
-- * 'iifValueSet' - The filter values.
instanceInformationFilter
    :: InstanceInformationFilterKey -- ^ 'iifKey'
    -> NonEmpty Text -- ^ 'iifValueSet'
    -> InstanceInformationFilter
instanceInformationFilter pKey_ pValueSet_ =
  InstanceInformationFilter'
    {_iifKey = pKey_, _iifValueSet = _List1 # pValueSet_}


-- | The name of the filter.
iifKey :: Lens' InstanceInformationFilter InstanceInformationFilterKey
iifKey = lens _iifKey (\ s a -> s{_iifKey = a})

-- | The filter values.
iifValueSet :: Lens' InstanceInformationFilter (NonEmpty Text)
iifValueSet = lens _iifValueSet (\ s a -> s{_iifValueSet = a}) . _List1

instance Hashable InstanceInformationFilter where

instance NFData InstanceInformationFilter where

instance ToJSON InstanceInformationFilter where
        toJSON InstanceInformationFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _iifKey),
                  Just ("valueSet" .= _iifValueSet)])

-- | The filters to describe or get information about your managed instances.
--
--
--
-- /See:/ 'instanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
  { _iisfKey    :: !Text
  , _iisfValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceInformationStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iisfKey' - The filter key name to describe your instances. For example: "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
--
-- * 'iisfValues' - The filter values.
instanceInformationStringFilter
    :: Text -- ^ 'iisfKey'
    -> NonEmpty Text -- ^ 'iisfValues'
    -> InstanceInformationStringFilter
instanceInformationStringFilter pKey_ pValues_ =
  InstanceInformationStringFilter'
    {_iisfKey = pKey_, _iisfValues = _List1 # pValues_}


-- | The filter key name to describe your instances. For example: "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
iisfKey :: Lens' InstanceInformationStringFilter Text
iisfKey = lens _iisfKey (\ s a -> s{_iisfKey = a})

-- | The filter values.
iisfValues :: Lens' InstanceInformationStringFilter (NonEmpty Text)
iisfValues = lens _iisfValues (\ s a -> s{_iisfValues = a}) . _List1

instance Hashable InstanceInformationStringFilter
         where

instance NFData InstanceInformationStringFilter where

instance ToJSON InstanceInformationStringFilter where
        toJSON InstanceInformationStringFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _iisfKey),
                  Just ("Values" .= _iisfValues)])

-- | Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.
--
--
--
-- /See:/ 'instancePatchState' smart constructor.
data InstancePatchState = InstancePatchState'
  { _ipsOwnerInformation    :: !(Maybe (Sensitive Text))
  , _ipsFailedCount         :: !(Maybe Int)
  , _ipsInstalledOtherCount :: !(Maybe Int)
  , _ipsMissingCount        :: !(Maybe Int)
  , _ipsNotApplicableCount  :: !(Maybe Int)
  , _ipsInstalledCount      :: !(Maybe Int)
  , _ipsSnapshotId          :: !(Maybe Text)
  , _ipsInstanceId          :: !Text
  , _ipsPatchGroup          :: !Text
  , _ipsBaselineId          :: !Text
  , _ipsOperationStartTime  :: !POSIX
  , _ipsOperationEndTime    :: !POSIX
  , _ipsOperation           :: !PatchOperationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstancePatchState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsOwnerInformation' - Placeholder information. This field will always be empty in the current release of the service.
--
-- * 'ipsFailedCount' - The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
--
-- * 'ipsInstalledOtherCount' - The number of patches not specified in the patch baseline that are installed on the instance.
--
-- * 'ipsMissingCount' - The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
--
-- * 'ipsNotApplicableCount' - The number of patches from the patch baseline that aren't applicable for the instance and hence aren't installed on the instance.
--
-- * 'ipsInstalledCount' - The number of patches from the patch baseline that are installed on the instance.
--
-- * 'ipsSnapshotId' - The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
--
-- * 'ipsInstanceId' - The ID of the managed instance the high-level patch compliance information was collected for.
--
-- * 'ipsPatchGroup' - The name of the patch group the managed instance belongs to.
--
-- * 'ipsBaselineId' - The ID of the patch baseline used to patch the instance.
--
-- * 'ipsOperationStartTime' - The time the most recent patching operation was started on the instance.
--
-- * 'ipsOperationEndTime' - The time the most recent patching operation completed on the instance.
--
-- * 'ipsOperation' - The type of patching operation that was performed: SCAN (assess patch compliance state) or INSTALL (install missing patches).
instancePatchState
    :: Text -- ^ 'ipsInstanceId'
    -> Text -- ^ 'ipsPatchGroup'
    -> Text -- ^ 'ipsBaselineId'
    -> UTCTime -- ^ 'ipsOperationStartTime'
    -> UTCTime -- ^ 'ipsOperationEndTime'
    -> PatchOperationType -- ^ 'ipsOperation'
    -> InstancePatchState
instancePatchState pInstanceId_ pPatchGroup_ pBaselineId_ pOperationStartTime_ pOperationEndTime_ pOperation_ =
  InstancePatchState'
    { _ipsOwnerInformation = Nothing
    , _ipsFailedCount = Nothing
    , _ipsInstalledOtherCount = Nothing
    , _ipsMissingCount = Nothing
    , _ipsNotApplicableCount = Nothing
    , _ipsInstalledCount = Nothing
    , _ipsSnapshotId = Nothing
    , _ipsInstanceId = pInstanceId_
    , _ipsPatchGroup = pPatchGroup_
    , _ipsBaselineId = pBaselineId_
    , _ipsOperationStartTime = _Time # pOperationStartTime_
    , _ipsOperationEndTime = _Time # pOperationEndTime_
    , _ipsOperation = pOperation_
    }


-- | Placeholder information. This field will always be empty in the current release of the service.
ipsOwnerInformation :: Lens' InstancePatchState (Maybe Text)
ipsOwnerInformation = lens _ipsOwnerInformation (\ s a -> s{_ipsOwnerInformation = a}) . mapping _Sensitive

-- | The number of patches from the patch baseline that were attempted to be installed during the last patching operation, but failed to install.
ipsFailedCount :: Lens' InstancePatchState (Maybe Int)
ipsFailedCount = lens _ipsFailedCount (\ s a -> s{_ipsFailedCount = a})

-- | The number of patches not specified in the patch baseline that are installed on the instance.
ipsInstalledOtherCount :: Lens' InstancePatchState (Maybe Int)
ipsInstalledOtherCount = lens _ipsInstalledOtherCount (\ s a -> s{_ipsInstalledOtherCount = a})

-- | The number of patches from the patch baseline that are applicable for the instance but aren't currently installed.
ipsMissingCount :: Lens' InstancePatchState (Maybe Int)
ipsMissingCount = lens _ipsMissingCount (\ s a -> s{_ipsMissingCount = a})

-- | The number of patches from the patch baseline that aren't applicable for the instance and hence aren't installed on the instance.
ipsNotApplicableCount :: Lens' InstancePatchState (Maybe Int)
ipsNotApplicableCount = lens _ipsNotApplicableCount (\ s a -> s{_ipsNotApplicableCount = a})

-- | The number of patches from the patch baseline that are installed on the instance.
ipsInstalledCount :: Lens' InstancePatchState (Maybe Int)
ipsInstalledCount = lens _ipsInstalledCount (\ s a -> s{_ipsInstalledCount = a})

-- | The ID of the patch baseline snapshot used during the patching operation when this compliance data was collected.
ipsSnapshotId :: Lens' InstancePatchState (Maybe Text)
ipsSnapshotId = lens _ipsSnapshotId (\ s a -> s{_ipsSnapshotId = a})

-- | The ID of the managed instance the high-level patch compliance information was collected for.
ipsInstanceId :: Lens' InstancePatchState Text
ipsInstanceId = lens _ipsInstanceId (\ s a -> s{_ipsInstanceId = a})

-- | The name of the patch group the managed instance belongs to.
ipsPatchGroup :: Lens' InstancePatchState Text
ipsPatchGroup = lens _ipsPatchGroup (\ s a -> s{_ipsPatchGroup = a})

-- | The ID of the patch baseline used to patch the instance.
ipsBaselineId :: Lens' InstancePatchState Text
ipsBaselineId = lens _ipsBaselineId (\ s a -> s{_ipsBaselineId = a})

-- | The time the most recent patching operation was started on the instance.
ipsOperationStartTime :: Lens' InstancePatchState UTCTime
ipsOperationStartTime = lens _ipsOperationStartTime (\ s a -> s{_ipsOperationStartTime = a}) . _Time

-- | The time the most recent patching operation completed on the instance.
ipsOperationEndTime :: Lens' InstancePatchState UTCTime
ipsOperationEndTime = lens _ipsOperationEndTime (\ s a -> s{_ipsOperationEndTime = a}) . _Time

-- | The type of patching operation that was performed: SCAN (assess patch compliance state) or INSTALL (install missing patches).
ipsOperation :: Lens' InstancePatchState PatchOperationType
ipsOperation = lens _ipsOperation (\ s a -> s{_ipsOperation = a})

instance FromJSON InstancePatchState where
        parseJSON
          = withObject "InstancePatchState"
              (\ x ->
                 InstancePatchState' <$>
                   (x .:? "OwnerInformation") <*> (x .:? "FailedCount")
                     <*> (x .:? "InstalledOtherCount")
                     <*> (x .:? "MissingCount")
                     <*> (x .:? "NotApplicableCount")
                     <*> (x .:? "InstalledCount")
                     <*> (x .:? "SnapshotId")
                     <*> (x .: "InstanceId")
                     <*> (x .: "PatchGroup")
                     <*> (x .: "BaselineId")
                     <*> (x .: "OperationStartTime")
                     <*> (x .: "OperationEndTime")
                     <*> (x .: "Operation"))

instance Hashable InstancePatchState where

instance NFData InstancePatchState where

-- | Defines a filter used in DescribeInstancePatchStatesForPatchGroup used to scope down the information returned by the API.
--
--
--
-- /See:/ 'instancePatchStateFilter' smart constructor.
data InstancePatchStateFilter = InstancePatchStateFilter'
  { _ipsfKey    :: !Text
  , _ipsfValues :: !(List1 Text)
  , _ipsfType   :: !InstancePatchStateOperatorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstancePatchStateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsfKey' - The key for the filter. Supported values are FailedCount, InstalledCount, InstalledOtherCount, MissingCount and NotApplicableCount.
--
-- * 'ipsfValues' - The value for the filter, must be an integer greater than or equal to 0.
--
-- * 'ipsfType' - The type of comparison that should be performed for the value: Equal, NotEqual, LessThan or GreaterThan.
instancePatchStateFilter
    :: Text -- ^ 'ipsfKey'
    -> NonEmpty Text -- ^ 'ipsfValues'
    -> InstancePatchStateOperatorType -- ^ 'ipsfType'
    -> InstancePatchStateFilter
instancePatchStateFilter pKey_ pValues_ pType_ =
  InstancePatchStateFilter'
    {_ipsfKey = pKey_, _ipsfValues = _List1 # pValues_, _ipsfType = pType_}


-- | The key for the filter. Supported values are FailedCount, InstalledCount, InstalledOtherCount, MissingCount and NotApplicableCount.
ipsfKey :: Lens' InstancePatchStateFilter Text
ipsfKey = lens _ipsfKey (\ s a -> s{_ipsfKey = a})

-- | The value for the filter, must be an integer greater than or equal to 0.
ipsfValues :: Lens' InstancePatchStateFilter (NonEmpty Text)
ipsfValues = lens _ipsfValues (\ s a -> s{_ipsfValues = a}) . _List1

-- | The type of comparison that should be performed for the value: Equal, NotEqual, LessThan or GreaterThan.
ipsfType :: Lens' InstancePatchStateFilter InstancePatchStateOperatorType
ipsfType = lens _ipsfType (\ s a -> s{_ipsfType = a})

instance Hashable InstancePatchStateFilter where

instance NFData InstancePatchStateFilter where

instance ToJSON InstancePatchStateFilter where
        toJSON InstancePatchStateFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _ipsfKey),
                  Just ("Values" .= _ipsfValues),
                  Just ("Type" .= _ipsfType)])

-- | Specifies the inventory type and attribute for the aggregation execution.
--
--
--
-- /See:/ 'inventoryAggregator' smart constructor.
data InventoryAggregator = InventoryAggregator'
  { _iaAggregators :: !(Maybe (List1 InventoryAggregator))
  , _iaExpression  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryAggregator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaAggregators' - Nested aggregators to further refine aggregation for an inventory type.
--
-- * 'iaExpression' - The inventory type and attribute name for aggregation.
inventoryAggregator
    :: InventoryAggregator
inventoryAggregator =
  InventoryAggregator' {_iaAggregators = Nothing, _iaExpression = Nothing}


-- | Nested aggregators to further refine aggregation for an inventory type.
iaAggregators :: Lens' InventoryAggregator (Maybe (NonEmpty InventoryAggregator))
iaAggregators = lens _iaAggregators (\ s a -> s{_iaAggregators = a}) . mapping _List1

-- | The inventory type and attribute name for aggregation.
iaExpression :: Lens' InventoryAggregator (Maybe Text)
iaExpression = lens _iaExpression (\ s a -> s{_iaExpression = a})

instance Hashable InventoryAggregator where

instance NFData InventoryAggregator where

instance ToJSON InventoryAggregator where
        toJSON InventoryAggregator'{..}
          = object
              (catMaybes
                 [("Aggregators" .=) <$> _iaAggregators,
                  ("Expression" .=) <$> _iaExpression])

-- | Status information returned by the @DeleteInventory@ action.
--
--
--
-- /See:/ 'inventoryDeletionStatusItem' smart constructor.
data InventoryDeletionStatusItem = InventoryDeletionStatusItem'
  { _idsiTypeName             :: !(Maybe Text)
  , _idsiLastStatusUpdateTime :: !(Maybe POSIX)
  , _idsiLastStatusMessage    :: !(Maybe Text)
  , _idsiDeletionSummary      :: !(Maybe InventoryDeletionSummary)
  , _idsiLastStatus           :: !(Maybe InventoryDeletionStatus)
  , _idsiDeletionStartTime    :: !(Maybe POSIX)
  , _idsiDeletionId           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryDeletionStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsiTypeName' - The name of the inventory data type.
--
-- * 'idsiLastStatusUpdateTime' - The UTC timestamp of when the last status report.
--
-- * 'idsiLastStatusMessage' - Information about the status.
--
-- * 'idsiDeletionSummary' - Information about the delete operation. For more information about this summary, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-delete.html#sysman-inventory-delete-summary Understanding the Delete Inventory Summary> .
--
-- * 'idsiLastStatus' - The status of the operation. Possible values are InProgress and Complete.
--
-- * 'idsiDeletionStartTime' - The UTC timestamp when the delete operation started.
--
-- * 'idsiDeletionId' - The deletion ID returned by the @DeleteInventory@ action.
inventoryDeletionStatusItem
    :: InventoryDeletionStatusItem
inventoryDeletionStatusItem =
  InventoryDeletionStatusItem'
    { _idsiTypeName = Nothing
    , _idsiLastStatusUpdateTime = Nothing
    , _idsiLastStatusMessage = Nothing
    , _idsiDeletionSummary = Nothing
    , _idsiLastStatus = Nothing
    , _idsiDeletionStartTime = Nothing
    , _idsiDeletionId = Nothing
    }


-- | The name of the inventory data type.
idsiTypeName :: Lens' InventoryDeletionStatusItem (Maybe Text)
idsiTypeName = lens _idsiTypeName (\ s a -> s{_idsiTypeName = a})

-- | The UTC timestamp of when the last status report.
idsiLastStatusUpdateTime :: Lens' InventoryDeletionStatusItem (Maybe UTCTime)
idsiLastStatusUpdateTime = lens _idsiLastStatusUpdateTime (\ s a -> s{_idsiLastStatusUpdateTime = a}) . mapping _Time

-- | Information about the status.
idsiLastStatusMessage :: Lens' InventoryDeletionStatusItem (Maybe Text)
idsiLastStatusMessage = lens _idsiLastStatusMessage (\ s a -> s{_idsiLastStatusMessage = a})

-- | Information about the delete operation. For more information about this summary, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-delete.html#sysman-inventory-delete-summary Understanding the Delete Inventory Summary> .
idsiDeletionSummary :: Lens' InventoryDeletionStatusItem (Maybe InventoryDeletionSummary)
idsiDeletionSummary = lens _idsiDeletionSummary (\ s a -> s{_idsiDeletionSummary = a})

-- | The status of the operation. Possible values are InProgress and Complete.
idsiLastStatus :: Lens' InventoryDeletionStatusItem (Maybe InventoryDeletionStatus)
idsiLastStatus = lens _idsiLastStatus (\ s a -> s{_idsiLastStatus = a})

-- | The UTC timestamp when the delete operation started.
idsiDeletionStartTime :: Lens' InventoryDeletionStatusItem (Maybe UTCTime)
idsiDeletionStartTime = lens _idsiDeletionStartTime (\ s a -> s{_idsiDeletionStartTime = a}) . mapping _Time

-- | The deletion ID returned by the @DeleteInventory@ action.
idsiDeletionId :: Lens' InventoryDeletionStatusItem (Maybe Text)
idsiDeletionId = lens _idsiDeletionId (\ s a -> s{_idsiDeletionId = a})

instance FromJSON InventoryDeletionStatusItem where
        parseJSON
          = withObject "InventoryDeletionStatusItem"
              (\ x ->
                 InventoryDeletionStatusItem' <$>
                   (x .:? "TypeName") <*> (x .:? "LastStatusUpdateTime")
                     <*> (x .:? "LastStatusMessage")
                     <*> (x .:? "DeletionSummary")
                     <*> (x .:? "LastStatus")
                     <*> (x .:? "DeletionStartTime")
                     <*> (x .:? "DeletionId"))

instance Hashable InventoryDeletionStatusItem where

instance NFData InventoryDeletionStatusItem where

-- | Information about the delete operation.
--
--
--
-- /See:/ 'inventoryDeletionSummary' smart constructor.
data InventoryDeletionSummary = InventoryDeletionSummary'
  { _idsRemainingCount :: !(Maybe Int)
  , _idsSummaryItems   :: !(Maybe [InventoryDeletionSummaryItem])
  , _idsTotalCount     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryDeletionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsRemainingCount' - Remaining number of items to delete.
--
-- * 'idsSummaryItems' - A list of counts and versions for deleted items.
--
-- * 'idsTotalCount' - The total number of items to delete. This count does not change during the delete operation.
inventoryDeletionSummary
    :: InventoryDeletionSummary
inventoryDeletionSummary =
  InventoryDeletionSummary'
    { _idsRemainingCount = Nothing
    , _idsSummaryItems = Nothing
    , _idsTotalCount = Nothing
    }


-- | Remaining number of items to delete.
idsRemainingCount :: Lens' InventoryDeletionSummary (Maybe Int)
idsRemainingCount = lens _idsRemainingCount (\ s a -> s{_idsRemainingCount = a})

-- | A list of counts and versions for deleted items.
idsSummaryItems :: Lens' InventoryDeletionSummary [InventoryDeletionSummaryItem]
idsSummaryItems = lens _idsSummaryItems (\ s a -> s{_idsSummaryItems = a}) . _Default . _Coerce

-- | The total number of items to delete. This count does not change during the delete operation.
idsTotalCount :: Lens' InventoryDeletionSummary (Maybe Int)
idsTotalCount = lens _idsTotalCount (\ s a -> s{_idsTotalCount = a})

instance FromJSON InventoryDeletionSummary where
        parseJSON
          = withObject "InventoryDeletionSummary"
              (\ x ->
                 InventoryDeletionSummary' <$>
                   (x .:? "RemainingCount") <*>
                     (x .:? "SummaryItems" .!= mempty)
                     <*> (x .:? "TotalCount"))

instance Hashable InventoryDeletionSummary where

instance NFData InventoryDeletionSummary where

-- | Either a count, remaining count, or a version number in a delete inventory summary.
--
--
--
-- /See:/ 'inventoryDeletionSummaryItem' smart constructor.
data InventoryDeletionSummaryItem = InventoryDeletionSummaryItem'
  { _idsiRemainingCount :: !(Maybe Int)
  , _idsiCount          :: !(Maybe Int)
  , _idsiVersion        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryDeletionSummaryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsiRemainingCount' - The remaining number of items to delete.
--
-- * 'idsiCount' - A count of the number of deleted items.
--
-- * 'idsiVersion' - The inventory type version.
inventoryDeletionSummaryItem
    :: InventoryDeletionSummaryItem
inventoryDeletionSummaryItem =
  InventoryDeletionSummaryItem'
    { _idsiRemainingCount = Nothing
    , _idsiCount = Nothing
    , _idsiVersion = Nothing
    }


-- | The remaining number of items to delete.
idsiRemainingCount :: Lens' InventoryDeletionSummaryItem (Maybe Int)
idsiRemainingCount = lens _idsiRemainingCount (\ s a -> s{_idsiRemainingCount = a})

-- | A count of the number of deleted items.
idsiCount :: Lens' InventoryDeletionSummaryItem (Maybe Int)
idsiCount = lens _idsiCount (\ s a -> s{_idsiCount = a})

-- | The inventory type version.
idsiVersion :: Lens' InventoryDeletionSummaryItem (Maybe Text)
idsiVersion = lens _idsiVersion (\ s a -> s{_idsiVersion = a})

instance FromJSON InventoryDeletionSummaryItem where
        parseJSON
          = withObject "InventoryDeletionSummaryItem"
              (\ x ->
                 InventoryDeletionSummaryItem' <$>
                   (x .:? "RemainingCount") <*> (x .:? "Count") <*>
                     (x .:? "Version"))

instance Hashable InventoryDeletionSummaryItem where

instance NFData InventoryDeletionSummaryItem where

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'inventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { _ifType   :: !(Maybe InventoryQueryOperatorType)
  , _ifKey    :: !Text
  , _ifValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifType' - The type of filter. Valid values include the following: "Equal"|"NotEqual"|"BeginWith"|"LessThan"|"GreaterThan"
--
-- * 'ifKey' - The name of the filter key.
--
-- * 'ifValues' - Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
inventoryFilter
    :: Text -- ^ 'ifKey'
    -> NonEmpty Text -- ^ 'ifValues'
    -> InventoryFilter
inventoryFilter pKey_ pValues_ =
  InventoryFilter'
    {_ifType = Nothing, _ifKey = pKey_, _ifValues = _List1 # pValues_}


-- | The type of filter. Valid values include the following: "Equal"|"NotEqual"|"BeginWith"|"LessThan"|"GreaterThan"
ifType :: Lens' InventoryFilter (Maybe InventoryQueryOperatorType)
ifType = lens _ifType (\ s a -> s{_ifType = a})

-- | The name of the filter key.
ifKey :: Lens' InventoryFilter Text
ifKey = lens _ifKey (\ s a -> s{_ifKey = a})

-- | Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
ifValues :: Lens' InventoryFilter (NonEmpty Text)
ifValues = lens _ifValues (\ s a -> s{_ifValues = a}) . _List1

instance Hashable InventoryFilter where

instance NFData InventoryFilter where

instance ToJSON InventoryFilter where
        toJSON InventoryFilter'{..}
          = object
              (catMaybes
                 [("Type" .=) <$> _ifType, Just ("Key" .= _ifKey),
                  Just ("Values" .= _ifValues)])

-- | Information collected from managed instances based on your inventory policy document
--
--
--
-- /See:/ 'inventoryItem' smart constructor.
data InventoryItem = InventoryItem'
  { _iiContext       :: !(Maybe (Map Text Text))
  , _iiContentHash   :: !(Maybe Text)
  , _iiContent       :: !(Maybe [Map Text Text])
  , _iiTypeName      :: !Text
  , _iiSchemaVersion :: !Text
  , _iiCaptureTime   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiContext' - A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
--
-- * 'iiContentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- * 'iiContent' - The inventory data of the inventory type.
--
-- * 'iiTypeName' - The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- * 'iiSchemaVersion' - The schema version for the inventory item.
--
-- * 'iiCaptureTime' - The time the inventory information was collected.
inventoryItem
    :: Text -- ^ 'iiTypeName'
    -> Text -- ^ 'iiSchemaVersion'
    -> Text -- ^ 'iiCaptureTime'
    -> InventoryItem
inventoryItem pTypeName_ pSchemaVersion_ pCaptureTime_ =
  InventoryItem'
    { _iiContext = Nothing
    , _iiContentHash = Nothing
    , _iiContent = Nothing
    , _iiTypeName = pTypeName_
    , _iiSchemaVersion = pSchemaVersion_
    , _iiCaptureTime = pCaptureTime_
    }


-- | A map of associated properties for a specified inventory type. For example, with this attribute, you can specify the @ExecutionId@ , @ExecutionType@ , @ComplianceType@ properties of the @AWS:ComplianceItem@ type.
iiContext :: Lens' InventoryItem (HashMap Text Text)
iiContext = lens _iiContext (\ s a -> s{_iiContext = a}) . _Default . _Map

-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
iiContentHash :: Lens' InventoryItem (Maybe Text)
iiContentHash = lens _iiContentHash (\ s a -> s{_iiContentHash = a})

-- | The inventory data of the inventory type.
iiContent :: Lens' InventoryItem [HashMap Text Text]
iiContent = lens _iiContent (\ s a -> s{_iiContent = a}) . _Default . _Coerce

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
iiTypeName :: Lens' InventoryItem Text
iiTypeName = lens _iiTypeName (\ s a -> s{_iiTypeName = a})

-- | The schema version for the inventory item.
iiSchemaVersion :: Lens' InventoryItem Text
iiSchemaVersion = lens _iiSchemaVersion (\ s a -> s{_iiSchemaVersion = a})

-- | The time the inventory information was collected.
iiCaptureTime :: Lens' InventoryItem Text
iiCaptureTime = lens _iiCaptureTime (\ s a -> s{_iiCaptureTime = a})

instance Hashable InventoryItem where

instance NFData InventoryItem where

instance ToJSON InventoryItem where
        toJSON InventoryItem'{..}
          = object
              (catMaybes
                 [("Context" .=) <$> _iiContext,
                  ("ContentHash" .=) <$> _iiContentHash,
                  ("Content" .=) <$> _iiContent,
                  Just ("TypeName" .= _iiTypeName),
                  Just ("SchemaVersion" .= _iiSchemaVersion),
                  Just ("CaptureTime" .= _iiCaptureTime)])

-- | Attributes are the entries within the inventory item content. It contains name and value.
--
--
--
-- /See:/ 'inventoryItemAttribute' smart constructor.
data InventoryItemAttribute = InventoryItemAttribute'
  { _iiaName     :: !Text
  , _iiaDataType :: !InventoryAttributeDataType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryItemAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiaName' - Name of the inventory item attribute.
--
-- * 'iiaDataType' - The data type of the inventory item attribute.
inventoryItemAttribute
    :: Text -- ^ 'iiaName'
    -> InventoryAttributeDataType -- ^ 'iiaDataType'
    -> InventoryItemAttribute
inventoryItemAttribute pName_ pDataType_ =
  InventoryItemAttribute' {_iiaName = pName_, _iiaDataType = pDataType_}


-- | Name of the inventory item attribute.
iiaName :: Lens' InventoryItemAttribute Text
iiaName = lens _iiaName (\ s a -> s{_iiaName = a})

-- | The data type of the inventory item attribute.
iiaDataType :: Lens' InventoryItemAttribute InventoryAttributeDataType
iiaDataType = lens _iiaDataType (\ s a -> s{_iiaDataType = a})

instance FromJSON InventoryItemAttribute where
        parseJSON
          = withObject "InventoryItemAttribute"
              (\ x ->
                 InventoryItemAttribute' <$>
                   (x .: "Name") <*> (x .: "DataType"))

instance Hashable InventoryItemAttribute where

instance NFData InventoryItemAttribute where

-- | The inventory item schema definition. Users can use this to compose inventory query filters.
--
--
--
-- /See:/ 'inventoryItemSchema' smart constructor.
data InventoryItemSchema = InventoryItemSchema'
  { _iisVersion     :: !(Maybe Text)
  , _iisDisplayName :: !(Maybe Text)
  , _iisTypeName    :: !Text
  , _iisAttributes  :: !(List1 InventoryItemAttribute)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryItemSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iisVersion' - The schema version for the inventory item.
--
-- * 'iisDisplayName' - The alias name of the inventory type. The alias name is used for display purposes.
--
-- * 'iisTypeName' - The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
--
-- * 'iisAttributes' - The schema attributes for inventory. This contains data type and attribute name.
inventoryItemSchema
    :: Text -- ^ 'iisTypeName'
    -> NonEmpty InventoryItemAttribute -- ^ 'iisAttributes'
    -> InventoryItemSchema
inventoryItemSchema pTypeName_ pAttributes_ =
  InventoryItemSchema'
    { _iisVersion = Nothing
    , _iisDisplayName = Nothing
    , _iisTypeName = pTypeName_
    , _iisAttributes = _List1 # pAttributes_
    }


-- | The schema version for the inventory item.
iisVersion :: Lens' InventoryItemSchema (Maybe Text)
iisVersion = lens _iisVersion (\ s a -> s{_iisVersion = a})

-- | The alias name of the inventory type. The alias name is used for display purposes.
iisDisplayName :: Lens' InventoryItemSchema (Maybe Text)
iisDisplayName = lens _iisDisplayName (\ s a -> s{_iisDisplayName = a})

-- | The name of the inventory type. Default inventory item type names start with AWS. Custom inventory type names will start with Custom. Default inventory item types include the following: AWS:AWSComponent, AWS:Application, AWS:InstanceInformation, AWS:Network, and AWS:WindowsUpdate.
iisTypeName :: Lens' InventoryItemSchema Text
iisTypeName = lens _iisTypeName (\ s a -> s{_iisTypeName = a})

-- | The schema attributes for inventory. This contains data type and attribute name.
iisAttributes :: Lens' InventoryItemSchema (NonEmpty InventoryItemAttribute)
iisAttributes = lens _iisAttributes (\ s a -> s{_iisAttributes = a}) . _List1

instance FromJSON InventoryItemSchema where
        parseJSON
          = withObject "InventoryItemSchema"
              (\ x ->
                 InventoryItemSchema' <$>
                   (x .:? "Version") <*> (x .:? "DisplayName") <*>
                     (x .: "TypeName")
                     <*> (x .: "Attributes"))

instance Hashable InventoryItemSchema where

instance NFData InventoryItemSchema where

-- | Inventory query results.
--
--
--
-- /See:/ 'inventoryResultEntity' smart constructor.
data InventoryResultEntity = InventoryResultEntity'
  { _ireData :: !(Maybe (Map Text InventoryResultItem))
  , _ireId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryResultEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ireData' - The data section in the inventory result entity JSON.
--
-- * 'ireId' - ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
inventoryResultEntity
    :: InventoryResultEntity
inventoryResultEntity =
  InventoryResultEntity' {_ireData = Nothing, _ireId = Nothing}


-- | The data section in the inventory result entity JSON.
ireData :: Lens' InventoryResultEntity (HashMap Text InventoryResultItem)
ireData = lens _ireData (\ s a -> s{_ireData = a}) . _Default . _Map

-- | ID of the inventory result entity. For example, for managed instance inventory the result will be the managed instance ID. For EC2 instance inventory, the result will be the instance ID.
ireId :: Lens' InventoryResultEntity (Maybe Text)
ireId = lens _ireId (\ s a -> s{_ireId = a})

instance FromJSON InventoryResultEntity where
        parseJSON
          = withObject "InventoryResultEntity"
              (\ x ->
                 InventoryResultEntity' <$>
                   (x .:? "Data" .!= mempty) <*> (x .:? "Id"))

instance Hashable InventoryResultEntity where

instance NFData InventoryResultEntity where

-- | The inventory result item.
--
--
--
-- /See:/ 'inventoryResultItem' smart constructor.
data InventoryResultItem = InventoryResultItem'
  { _iriContentHash   :: !(Maybe Text)
  , _iriCaptureTime   :: !(Maybe Text)
  , _iriTypeName      :: !Text
  , _iriSchemaVersion :: !Text
  , _iriContent       :: ![Map Text Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryResultItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iriContentHash' - MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
--
-- * 'iriCaptureTime' - The time inventory item data was captured.
--
-- * 'iriTypeName' - The name of the inventory result item type.
--
-- * 'iriSchemaVersion' - The schema version for the inventory result item/
--
-- * 'iriContent' - Contains all the inventory data of the item type. Results include attribute names and values.
inventoryResultItem
    :: Text -- ^ 'iriTypeName'
    -> Text -- ^ 'iriSchemaVersion'
    -> InventoryResultItem
inventoryResultItem pTypeName_ pSchemaVersion_ =
  InventoryResultItem'
    { _iriContentHash = Nothing
    , _iriCaptureTime = Nothing
    , _iriTypeName = pTypeName_
    , _iriSchemaVersion = pSchemaVersion_
    , _iriContent = mempty
    }


-- | MD5 hash of the inventory item type contents. The content hash is used to determine whether to update inventory information. The PutInventory API does not update the inventory item type contents if the MD5 hash has not changed since last update.
iriContentHash :: Lens' InventoryResultItem (Maybe Text)
iriContentHash = lens _iriContentHash (\ s a -> s{_iriContentHash = a})

-- | The time inventory item data was captured.
iriCaptureTime :: Lens' InventoryResultItem (Maybe Text)
iriCaptureTime = lens _iriCaptureTime (\ s a -> s{_iriCaptureTime = a})

-- | The name of the inventory result item type.
iriTypeName :: Lens' InventoryResultItem Text
iriTypeName = lens _iriTypeName (\ s a -> s{_iriTypeName = a})

-- | The schema version for the inventory result item/
iriSchemaVersion :: Lens' InventoryResultItem Text
iriSchemaVersion = lens _iriSchemaVersion (\ s a -> s{_iriSchemaVersion = a})

-- | Contains all the inventory data of the item type. Results include attribute names and values.
iriContent :: Lens' InventoryResultItem [HashMap Text Text]
iriContent = lens _iriContent (\ s a -> s{_iriContent = a}) . _Coerce

instance FromJSON InventoryResultItem where
        parseJSON
          = withObject "InventoryResultItem"
              (\ x ->
                 InventoryResultItem' <$>
                   (x .:? "ContentHash") <*> (x .:? "CaptureTime") <*>
                     (x .: "TypeName")
                     <*> (x .: "SchemaVersion")
                     <*> (x .:? "Content" .!= mempty))

instance Hashable InventoryResultItem where

instance NFData InventoryResultItem where

-- | Information about an Amazon S3 bucket to write instance-level logs to.
--
--
--
-- /See:/ 'loggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
  { _liS3KeyPrefix  :: !(Maybe Text)
  , _liS3BucketName :: !Text
  , _liS3Region     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liS3KeyPrefix' - (Optional) The Amazon S3 bucket subfolder.
--
-- * 'liS3BucketName' - The name of an Amazon S3 bucket where execution logs are stored .
--
-- * 'liS3Region' - The region where the Amazon S3 bucket is located.
loggingInfo
    :: Text -- ^ 'liS3BucketName'
    -> Text -- ^ 'liS3Region'
    -> LoggingInfo
loggingInfo pS3BucketName_ pS3Region_ =
  LoggingInfo'
    { _liS3KeyPrefix = Nothing
    , _liS3BucketName = pS3BucketName_
    , _liS3Region = pS3Region_
    }


-- | (Optional) The Amazon S3 bucket subfolder.
liS3KeyPrefix :: Lens' LoggingInfo (Maybe Text)
liS3KeyPrefix = lens _liS3KeyPrefix (\ s a -> s{_liS3KeyPrefix = a})

-- | The name of an Amazon S3 bucket where execution logs are stored .
liS3BucketName :: Lens' LoggingInfo Text
liS3BucketName = lens _liS3BucketName (\ s a -> s{_liS3BucketName = a})

-- | The region where the Amazon S3 bucket is located.
liS3Region :: Lens' LoggingInfo Text
liS3Region = lens _liS3Region (\ s a -> s{_liS3Region = a})

instance FromJSON LoggingInfo where
        parseJSON
          = withObject "LoggingInfo"
              (\ x ->
                 LoggingInfo' <$>
                   (x .:? "S3KeyPrefix") <*> (x .: "S3BucketName") <*>
                     (x .: "S3Region"))

instance Hashable LoggingInfo where

instance NFData LoggingInfo where

instance ToJSON LoggingInfo where
        toJSON LoggingInfo'{..}
          = object
              (catMaybes
                 [("S3KeyPrefix" .=) <$> _liS3KeyPrefix,
                  Just ("S3BucketName" .= _liS3BucketName),
                  Just ("S3Region" .= _liS3Region)])

-- | The parameters for an AUTOMATION task type.
--
--
--
-- /See:/ 'maintenanceWindowAutomationParameters' smart constructor.
data MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters'
  { _mwapParameters      :: !(Maybe (Map Text [Text]))
  , _mwapDocumentVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowAutomationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwapParameters' - The parameters for the AUTOMATION task. For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- * 'mwapDocumentVersion' - The version of an Automation document to use during task execution.
maintenanceWindowAutomationParameters
    :: MaintenanceWindowAutomationParameters
maintenanceWindowAutomationParameters =
  MaintenanceWindowAutomationParameters'
    {_mwapParameters = Nothing, _mwapDocumentVersion = Nothing}


-- | The parameters for the AUTOMATION task. For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
mwapParameters :: Lens' MaintenanceWindowAutomationParameters (HashMap Text [Text])
mwapParameters = lens _mwapParameters (\ s a -> s{_mwapParameters = a}) . _Default . _Map

-- | The version of an Automation document to use during task execution.
mwapDocumentVersion :: Lens' MaintenanceWindowAutomationParameters (Maybe Text)
mwapDocumentVersion = lens _mwapDocumentVersion (\ s a -> s{_mwapDocumentVersion = a})

instance FromJSON
           MaintenanceWindowAutomationParameters
         where
        parseJSON
          = withObject "MaintenanceWindowAutomationParameters"
              (\ x ->
                 MaintenanceWindowAutomationParameters' <$>
                   (x .:? "Parameters" .!= mempty) <*>
                     (x .:? "DocumentVersion"))

instance Hashable
           MaintenanceWindowAutomationParameters
         where

instance NFData MaintenanceWindowAutomationParameters
         where

instance ToJSON MaintenanceWindowAutomationParameters
         where
        toJSON MaintenanceWindowAutomationParameters'{..}
          = object
              (catMaybes
                 [("Parameters" .=) <$> _mwapParameters,
                  ("DocumentVersion" .=) <$> _mwapDocumentVersion])

-- | Describes the information about an execution of a Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { _mweStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
  , _mweStartTime         :: !(Maybe POSIX)
  , _mweWindowExecutionId :: !(Maybe Text)
  , _mweStatusDetails     :: !(Maybe Text)
  , _mweEndTime           :: !(Maybe POSIX)
  , _mweWindowId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mweStatus' - The status of the execution.
--
-- * 'mweStartTime' - The time the execution started.
--
-- * 'mweWindowExecutionId' - The ID of the Maintenance Window execution.
--
-- * 'mweStatusDetails' - The details explaining the Status. Only available for certain status values.
--
-- * 'mweEndTime' - The time the execution finished.
--
-- * 'mweWindowId' - The ID of the Maintenance Window.
maintenanceWindowExecution
    :: MaintenanceWindowExecution
maintenanceWindowExecution =
  MaintenanceWindowExecution'
    { _mweStatus = Nothing
    , _mweStartTime = Nothing
    , _mweWindowExecutionId = Nothing
    , _mweStatusDetails = Nothing
    , _mweEndTime = Nothing
    , _mweWindowId = Nothing
    }


-- | The status of the execution.
mweStatus :: Lens' MaintenanceWindowExecution (Maybe MaintenanceWindowExecutionStatus)
mweStatus = lens _mweStatus (\ s a -> s{_mweStatus = a})

-- | The time the execution started.
mweStartTime :: Lens' MaintenanceWindowExecution (Maybe UTCTime)
mweStartTime = lens _mweStartTime (\ s a -> s{_mweStartTime = a}) . mapping _Time

-- | The ID of the Maintenance Window execution.
mweWindowExecutionId :: Lens' MaintenanceWindowExecution (Maybe Text)
mweWindowExecutionId = lens _mweWindowExecutionId (\ s a -> s{_mweWindowExecutionId = a})

-- | The details explaining the Status. Only available for certain status values.
mweStatusDetails :: Lens' MaintenanceWindowExecution (Maybe Text)
mweStatusDetails = lens _mweStatusDetails (\ s a -> s{_mweStatusDetails = a})

-- | The time the execution finished.
mweEndTime :: Lens' MaintenanceWindowExecution (Maybe UTCTime)
mweEndTime = lens _mweEndTime (\ s a -> s{_mweEndTime = a}) . mapping _Time

-- | The ID of the Maintenance Window.
mweWindowId :: Lens' MaintenanceWindowExecution (Maybe Text)
mweWindowId = lens _mweWindowId (\ s a -> s{_mweWindowId = a})

instance FromJSON MaintenanceWindowExecution where
        parseJSON
          = withObject "MaintenanceWindowExecution"
              (\ x ->
                 MaintenanceWindowExecution' <$>
                   (x .:? "Status") <*> (x .:? "StartTime") <*>
                     (x .:? "WindowExecutionId")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowExecution where

instance NFData MaintenanceWindowExecution where

-- | Information about a task execution performed as part of a Maintenance Window execution.
--
--
--
-- /See:/ 'maintenanceWindowExecutionTaskIdentity' smart constructor.
data MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity'
  { _mwetiStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
  , _mwetiTaskExecutionId   :: !(Maybe Text)
  , _mwetiStartTime         :: !(Maybe POSIX)
  , _mwetiTaskType          :: !(Maybe MaintenanceWindowTaskType)
  , _mwetiTaskARN           :: !(Maybe Text)
  , _mwetiWindowExecutionId :: !(Maybe Text)
  , _mwetiStatusDetails     :: !(Maybe Text)
  , _mwetiEndTime           :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowExecutionTaskIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwetiStatus' - The status of the task execution.
--
-- * 'mwetiTaskExecutionId' - The ID of the specific task execution in the Maintenance Window execution.
--
-- * 'mwetiStartTime' - The time the task execution started.
--
-- * 'mwetiTaskType' - The type of executed task.
--
-- * 'mwetiTaskARN' - The ARN of the executed task.
--
-- * 'mwetiWindowExecutionId' - The ID of the Maintenance Window execution that ran the task.
--
-- * 'mwetiStatusDetails' - The details explaining the status of the task execution. Only available for certain status values.
--
-- * 'mwetiEndTime' - The time the task execution finished.
maintenanceWindowExecutionTaskIdentity
    :: MaintenanceWindowExecutionTaskIdentity
maintenanceWindowExecutionTaskIdentity =
  MaintenanceWindowExecutionTaskIdentity'
    { _mwetiStatus = Nothing
    , _mwetiTaskExecutionId = Nothing
    , _mwetiStartTime = Nothing
    , _mwetiTaskType = Nothing
    , _mwetiTaskARN = Nothing
    , _mwetiWindowExecutionId = Nothing
    , _mwetiStatusDetails = Nothing
    , _mwetiEndTime = Nothing
    }


-- | The status of the task execution.
mwetiStatus :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe MaintenanceWindowExecutionStatus)
mwetiStatus = lens _mwetiStatus (\ s a -> s{_mwetiStatus = a})

-- | The ID of the specific task execution in the Maintenance Window execution.
mwetiTaskExecutionId :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiTaskExecutionId = lens _mwetiTaskExecutionId (\ s a -> s{_mwetiTaskExecutionId = a})

-- | The time the task execution started.
mwetiStartTime :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe UTCTime)
mwetiStartTime = lens _mwetiStartTime (\ s a -> s{_mwetiStartTime = a}) . mapping _Time

-- | The type of executed task.
mwetiTaskType :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe MaintenanceWindowTaskType)
mwetiTaskType = lens _mwetiTaskType (\ s a -> s{_mwetiTaskType = a})

-- | The ARN of the executed task.
mwetiTaskARN :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiTaskARN = lens _mwetiTaskARN (\ s a -> s{_mwetiTaskARN = a})

-- | The ID of the Maintenance Window execution that ran the task.
mwetiWindowExecutionId :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiWindowExecutionId = lens _mwetiWindowExecutionId (\ s a -> s{_mwetiWindowExecutionId = a})

-- | The details explaining the status of the task execution. Only available for certain status values.
mwetiStatusDetails :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe Text)
mwetiStatusDetails = lens _mwetiStatusDetails (\ s a -> s{_mwetiStatusDetails = a})

-- | The time the task execution finished.
mwetiEndTime :: Lens' MaintenanceWindowExecutionTaskIdentity (Maybe UTCTime)
mwetiEndTime = lens _mwetiEndTime (\ s a -> s{_mwetiEndTime = a}) . mapping _Time

instance FromJSON
           MaintenanceWindowExecutionTaskIdentity
         where
        parseJSON
          = withObject "MaintenanceWindowExecutionTaskIdentity"
              (\ x ->
                 MaintenanceWindowExecutionTaskIdentity' <$>
                   (x .:? "Status") <*> (x .:? "TaskExecutionId") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "TaskType")
                     <*> (x .:? "TaskArn")
                     <*> (x .:? "WindowExecutionId")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "EndTime"))

instance Hashable
           MaintenanceWindowExecutionTaskIdentity
         where

instance NFData
           MaintenanceWindowExecutionTaskIdentity
         where

-- | Describes the information about a task invocation for a particular target as part of a task execution performed as part of a Maintenance Window execution.
--
--
--
-- /See:/ 'maintenanceWindowExecutionTaskInvocationIdentity' smart constructor.
data MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity'
  { _mwetiiStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
  , _mwetiiExecutionId       :: !(Maybe Text)
  , _mwetiiTaskExecutionId   :: !(Maybe Text)
  , _mwetiiStartTime         :: !(Maybe POSIX)
  , _mwetiiInvocationId      :: !(Maybe Text)
  , _mwetiiOwnerInformation  :: !(Maybe (Sensitive Text))
  , _mwetiiTaskType          :: !(Maybe MaintenanceWindowTaskType)
  , _mwetiiWindowTargetId    :: !(Maybe Text)
  , _mwetiiWindowExecutionId :: !(Maybe Text)
  , _mwetiiStatusDetails     :: !(Maybe Text)
  , _mwetiiEndTime           :: !(Maybe POSIX)
  , _mwetiiParameters        :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowExecutionTaskInvocationIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwetiiStatus' - The status of the task invocation.
--
-- * 'mwetiiExecutionId' - The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
--
-- * 'mwetiiTaskExecutionId' - The ID of the specific task execution in the Maintenance Window execution.
--
-- * 'mwetiiStartTime' - The time the invocation started.
--
-- * 'mwetiiInvocationId' - The ID of the task invocation.
--
-- * 'mwetiiOwnerInformation' - User-provided value that was specified when the target was registered with the Maintenance Window. This was also included in any CloudWatch events raised during the task invocation.
--
-- * 'mwetiiTaskType' - The task type.
--
-- * 'mwetiiWindowTargetId' - The ID of the target definition in this Maintenance Window the invocation was performed for.
--
-- * 'mwetiiWindowExecutionId' - The ID of the Maintenance Window execution that ran the task.
--
-- * 'mwetiiStatusDetails' - The details explaining the status of the task invocation. Only available for certain Status values.
--
-- * 'mwetiiEndTime' - The time the invocation finished.
--
-- * 'mwetiiParameters' - The parameters that were provided for the invocation when it was executed.
maintenanceWindowExecutionTaskInvocationIdentity
    :: MaintenanceWindowExecutionTaskInvocationIdentity
maintenanceWindowExecutionTaskInvocationIdentity =
  MaintenanceWindowExecutionTaskInvocationIdentity'
    { _mwetiiStatus = Nothing
    , _mwetiiExecutionId = Nothing
    , _mwetiiTaskExecutionId = Nothing
    , _mwetiiStartTime = Nothing
    , _mwetiiInvocationId = Nothing
    , _mwetiiOwnerInformation = Nothing
    , _mwetiiTaskType = Nothing
    , _mwetiiWindowTargetId = Nothing
    , _mwetiiWindowExecutionId = Nothing
    , _mwetiiStatusDetails = Nothing
    , _mwetiiEndTime = Nothing
    , _mwetiiParameters = Nothing
    }


-- | The status of the task invocation.
mwetiiStatus :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe MaintenanceWindowExecutionStatus)
mwetiiStatus = lens _mwetiiStatus (\ s a -> s{_mwetiiStatus = a})

-- | The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
mwetiiExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiExecutionId = lens _mwetiiExecutionId (\ s a -> s{_mwetiiExecutionId = a})

-- | The ID of the specific task execution in the Maintenance Window execution.
mwetiiTaskExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiTaskExecutionId = lens _mwetiiTaskExecutionId (\ s a -> s{_mwetiiTaskExecutionId = a})

-- | The time the invocation started.
mwetiiStartTime :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe UTCTime)
mwetiiStartTime = lens _mwetiiStartTime (\ s a -> s{_mwetiiStartTime = a}) . mapping _Time

-- | The ID of the task invocation.
mwetiiInvocationId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiInvocationId = lens _mwetiiInvocationId (\ s a -> s{_mwetiiInvocationId = a})

-- | User-provided value that was specified when the target was registered with the Maintenance Window. This was also included in any CloudWatch events raised during the task invocation.
mwetiiOwnerInformation :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiOwnerInformation = lens _mwetiiOwnerInformation (\ s a -> s{_mwetiiOwnerInformation = a}) . mapping _Sensitive

-- | The task type.
mwetiiTaskType :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe MaintenanceWindowTaskType)
mwetiiTaskType = lens _mwetiiTaskType (\ s a -> s{_mwetiiTaskType = a})

-- | The ID of the target definition in this Maintenance Window the invocation was performed for.
mwetiiWindowTargetId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiWindowTargetId = lens _mwetiiWindowTargetId (\ s a -> s{_mwetiiWindowTargetId = a})

-- | The ID of the Maintenance Window execution that ran the task.
mwetiiWindowExecutionId :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiWindowExecutionId = lens _mwetiiWindowExecutionId (\ s a -> s{_mwetiiWindowExecutionId = a})

-- | The details explaining the status of the task invocation. Only available for certain Status values.
mwetiiStatusDetails :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiStatusDetails = lens _mwetiiStatusDetails (\ s a -> s{_mwetiiStatusDetails = a})

-- | The time the invocation finished.
mwetiiEndTime :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe UTCTime)
mwetiiEndTime = lens _mwetiiEndTime (\ s a -> s{_mwetiiEndTime = a}) . mapping _Time

-- | The parameters that were provided for the invocation when it was executed.
mwetiiParameters :: Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Maybe Text)
mwetiiParameters = lens _mwetiiParameters (\ s a -> s{_mwetiiParameters = a}) . mapping _Sensitive

instance FromJSON
           MaintenanceWindowExecutionTaskInvocationIdentity
         where
        parseJSON
          = withObject
              "MaintenanceWindowExecutionTaskInvocationIdentity"
              (\ x ->
                 MaintenanceWindowExecutionTaskInvocationIdentity' <$>
                   (x .:? "Status") <*> (x .:? "ExecutionId") <*>
                     (x .:? "TaskExecutionId")
                     <*> (x .:? "StartTime")
                     <*> (x .:? "InvocationId")
                     <*> (x .:? "OwnerInformation")
                     <*> (x .:? "TaskType")
                     <*> (x .:? "WindowTargetId")
                     <*> (x .:? "WindowExecutionId")
                     <*> (x .:? "StatusDetails")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "Parameters"))

instance Hashable
           MaintenanceWindowExecutionTaskInvocationIdentity
         where

instance NFData
           MaintenanceWindowExecutionTaskInvocationIdentity
         where

-- | Filter used in the request.
--
--
--
-- /See:/ 'maintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { _mwfValues :: !(Maybe [Text])
  , _mwfKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwfValues' - The filter values.
--
-- * 'mwfKey' - The name of the filter.
maintenanceWindowFilter
    :: MaintenanceWindowFilter
maintenanceWindowFilter =
  MaintenanceWindowFilter' {_mwfValues = Nothing, _mwfKey = Nothing}


-- | The filter values.
mwfValues :: Lens' MaintenanceWindowFilter [Text]
mwfValues = lens _mwfValues (\ s a -> s{_mwfValues = a}) . _Default . _Coerce

-- | The name of the filter.
mwfKey :: Lens' MaintenanceWindowFilter (Maybe Text)
mwfKey = lens _mwfKey (\ s a -> s{_mwfKey = a})

instance Hashable MaintenanceWindowFilter where

instance NFData MaintenanceWindowFilter where

instance ToJSON MaintenanceWindowFilter where
        toJSON MaintenanceWindowFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _mwfValues,
                  ("Key" .=) <$> _mwfKey])

-- | Information about the Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { _mwiEnabled     :: !(Maybe Bool)
  , _mwiName        :: !(Maybe Text)
  , _mwiCutoff      :: !(Maybe Nat)
  , _mwiDescription :: !(Maybe (Sensitive Text))
  , _mwiDuration    :: !(Maybe Nat)
  , _mwiWindowId    :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwiEnabled' - Whether the Maintenance Window is enabled.
--
-- * 'mwiName' - The name of the Maintenance Window.
--
-- * 'mwiCutoff' - The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
--
-- * 'mwiDescription' - A description of the Maintenance Window.
--
-- * 'mwiDuration' - The duration of the Maintenance Window in hours.
--
-- * 'mwiWindowId' - The ID of the Maintenance Window.
maintenanceWindowIdentity
    :: MaintenanceWindowIdentity
maintenanceWindowIdentity =
  MaintenanceWindowIdentity'
    { _mwiEnabled = Nothing
    , _mwiName = Nothing
    , _mwiCutoff = Nothing
    , _mwiDescription = Nothing
    , _mwiDuration = Nothing
    , _mwiWindowId = Nothing
    }


-- | Whether the Maintenance Window is enabled.
mwiEnabled :: Lens' MaintenanceWindowIdentity (Maybe Bool)
mwiEnabled = lens _mwiEnabled (\ s a -> s{_mwiEnabled = a})

-- | The name of the Maintenance Window.
mwiName :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiName = lens _mwiName (\ s a -> s{_mwiName = a})

-- | The number of hours before the end of the Maintenance Window that Systems Manager stops scheduling new tasks for execution.
mwiCutoff :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiCutoff = lens _mwiCutoff (\ s a -> s{_mwiCutoff = a}) . mapping _Nat

-- | A description of the Maintenance Window.
mwiDescription :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiDescription = lens _mwiDescription (\ s a -> s{_mwiDescription = a}) . mapping _Sensitive

-- | The duration of the Maintenance Window in hours.
mwiDuration :: Lens' MaintenanceWindowIdentity (Maybe Natural)
mwiDuration = lens _mwiDuration (\ s a -> s{_mwiDuration = a}) . mapping _Nat

-- | The ID of the Maintenance Window.
mwiWindowId :: Lens' MaintenanceWindowIdentity (Maybe Text)
mwiWindowId = lens _mwiWindowId (\ s a -> s{_mwiWindowId = a})

instance FromJSON MaintenanceWindowIdentity where
        parseJSON
          = withObject "MaintenanceWindowIdentity"
              (\ x ->
                 MaintenanceWindowIdentity' <$>
                   (x .:? "Enabled") <*> (x .:? "Name") <*>
                     (x .:? "Cutoff")
                     <*> (x .:? "Description")
                     <*> (x .:? "Duration")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowIdentity where

instance NFData MaintenanceWindowIdentity where

-- | The parameters for a LAMBDA task type.
--
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
--
-- /See:/ 'maintenanceWindowLambdaParameters' smart constructor.
data MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters'
  { _mwlpPayload       :: !(Maybe (Sensitive Base64))
  , _mwlpQualifier     :: !(Maybe Text)
  , _mwlpClientContext :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowLambdaParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwlpPayload' - JSON to provide to your Lambda function as input.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'mwlpQualifier' - (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
--
-- * 'mwlpClientContext' - Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
maintenanceWindowLambdaParameters
    :: MaintenanceWindowLambdaParameters
maintenanceWindowLambdaParameters =
  MaintenanceWindowLambdaParameters'
    { _mwlpPayload = Nothing
    , _mwlpQualifier = Nothing
    , _mwlpClientContext = Nothing
    }


-- | JSON to provide to your Lambda function as input.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mwlpPayload :: Lens' MaintenanceWindowLambdaParameters (Maybe ByteString)
mwlpPayload = lens _mwlpPayload (\ s a -> s{_mwlpPayload = a}) . mapping (_Sensitive . _Base64)

-- | (Optional) Specify a Lambda function version or alias name. If you specify a function version, the action uses the qualified function ARN to invoke a specific Lambda function. If you specify an alias name, the action uses the alias ARN to invoke the Lambda function version to which the alias points.
mwlpQualifier :: Lens' MaintenanceWindowLambdaParameters (Maybe Text)
mwlpQualifier = lens _mwlpQualifier (\ s a -> s{_mwlpQualifier = a})

-- | Pass client-specific information to the Lambda function that you are invoking. You can then process the client information in your Lambda function as you choose through the context variable.
mwlpClientContext :: Lens' MaintenanceWindowLambdaParameters (Maybe Text)
mwlpClientContext = lens _mwlpClientContext (\ s a -> s{_mwlpClientContext = a})

instance FromJSON MaintenanceWindowLambdaParameters
         where
        parseJSON
          = withObject "MaintenanceWindowLambdaParameters"
              (\ x ->
                 MaintenanceWindowLambdaParameters' <$>
                   (x .:? "Payload") <*> (x .:? "Qualifier") <*>
                     (x .:? "ClientContext"))

instance Hashable MaintenanceWindowLambdaParameters
         where

instance NFData MaintenanceWindowLambdaParameters
         where

instance ToJSON MaintenanceWindowLambdaParameters
         where
        toJSON MaintenanceWindowLambdaParameters'{..}
          = object
              (catMaybes
                 [("Payload" .=) <$> _mwlpPayload,
                  ("Qualifier" .=) <$> _mwlpQualifier,
                  ("ClientContext" .=) <$> _mwlpClientContext])

-- | The parameters for a RUN_COMMAND task type.
--
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
--
-- /See:/ 'maintenanceWindowRunCommandParameters' smart constructor.
data MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters'
  { _mwrcpServiceRoleARN     :: !(Maybe Text)
  , _mwrcpNotificationConfig :: !(Maybe NotificationConfig)
  , _mwrcpDocumentHashType   :: !(Maybe DocumentHashType)
  , _mwrcpOutputS3KeyPrefix  :: !(Maybe Text)
  , _mwrcpParameters         :: !(Maybe (Map Text [Text]))
  , _mwrcpDocumentHash       :: !(Maybe Text)
  , _mwrcpTimeoutSeconds     :: !(Maybe Nat)
  , _mwrcpComment            :: !(Maybe Text)
  , _mwrcpOutputS3BucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowRunCommandParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwrcpServiceRoleARN' - The IAM service role to assume during task execution.
--
-- * 'mwrcpNotificationConfig' - Configurations for sending notifications about command status changes on a per-instance basis.
--
-- * 'mwrcpDocumentHashType' - SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
--
-- * 'mwrcpOutputS3KeyPrefix' - The Amazon S3 bucket subfolder.
--
-- * 'mwrcpParameters' - The parameters for the RUN_COMMAND task execution.
--
-- * 'mwrcpDocumentHash' - The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
--
-- * 'mwrcpTimeoutSeconds' - If this time is reached and the command has not already started executing, it doesn not execute.
--
-- * 'mwrcpComment' - Information about the command(s) to execute.
--
-- * 'mwrcpOutputS3BucketName' - The name of the Amazon S3 bucket.
maintenanceWindowRunCommandParameters
    :: MaintenanceWindowRunCommandParameters
maintenanceWindowRunCommandParameters =
  MaintenanceWindowRunCommandParameters'
    { _mwrcpServiceRoleARN = Nothing
    , _mwrcpNotificationConfig = Nothing
    , _mwrcpDocumentHashType = Nothing
    , _mwrcpOutputS3KeyPrefix = Nothing
    , _mwrcpParameters = Nothing
    , _mwrcpDocumentHash = Nothing
    , _mwrcpTimeoutSeconds = Nothing
    , _mwrcpComment = Nothing
    , _mwrcpOutputS3BucketName = Nothing
    }


-- | The IAM service role to assume during task execution.
mwrcpServiceRoleARN :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpServiceRoleARN = lens _mwrcpServiceRoleARN (\ s a -> s{_mwrcpServiceRoleARN = a})

-- | Configurations for sending notifications about command status changes on a per-instance basis.
mwrcpNotificationConfig :: Lens' MaintenanceWindowRunCommandParameters (Maybe NotificationConfig)
mwrcpNotificationConfig = lens _mwrcpNotificationConfig (\ s a -> s{_mwrcpNotificationConfig = a})

-- | SHA-256 or SHA-1. SHA-1 hashes have been deprecated.
mwrcpDocumentHashType :: Lens' MaintenanceWindowRunCommandParameters (Maybe DocumentHashType)
mwrcpDocumentHashType = lens _mwrcpDocumentHashType (\ s a -> s{_mwrcpDocumentHashType = a})

-- | The Amazon S3 bucket subfolder.
mwrcpOutputS3KeyPrefix :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpOutputS3KeyPrefix = lens _mwrcpOutputS3KeyPrefix (\ s a -> s{_mwrcpOutputS3KeyPrefix = a})

-- | The parameters for the RUN_COMMAND task execution.
mwrcpParameters :: Lens' MaintenanceWindowRunCommandParameters (HashMap Text [Text])
mwrcpParameters = lens _mwrcpParameters (\ s a -> s{_mwrcpParameters = a}) . _Default . _Map

-- | The SHA-256 or SHA-1 hash created by the system when the document was created. SHA-1 hashes have been deprecated.
mwrcpDocumentHash :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpDocumentHash = lens _mwrcpDocumentHash (\ s a -> s{_mwrcpDocumentHash = a})

-- | If this time is reached and the command has not already started executing, it doesn not execute.
mwrcpTimeoutSeconds :: Lens' MaintenanceWindowRunCommandParameters (Maybe Natural)
mwrcpTimeoutSeconds = lens _mwrcpTimeoutSeconds (\ s a -> s{_mwrcpTimeoutSeconds = a}) . mapping _Nat

-- | Information about the command(s) to execute.
mwrcpComment :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpComment = lens _mwrcpComment (\ s a -> s{_mwrcpComment = a})

-- | The name of the Amazon S3 bucket.
mwrcpOutputS3BucketName :: Lens' MaintenanceWindowRunCommandParameters (Maybe Text)
mwrcpOutputS3BucketName = lens _mwrcpOutputS3BucketName (\ s a -> s{_mwrcpOutputS3BucketName = a})

instance FromJSON
           MaintenanceWindowRunCommandParameters
         where
        parseJSON
          = withObject "MaintenanceWindowRunCommandParameters"
              (\ x ->
                 MaintenanceWindowRunCommandParameters' <$>
                   (x .:? "ServiceRoleArn") <*>
                     (x .:? "NotificationConfig")
                     <*> (x .:? "DocumentHashType")
                     <*> (x .:? "OutputS3KeyPrefix")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentHash")
                     <*> (x .:? "TimeoutSeconds")
                     <*> (x .:? "Comment")
                     <*> (x .:? "OutputS3BucketName"))

instance Hashable
           MaintenanceWindowRunCommandParameters
         where

instance NFData MaintenanceWindowRunCommandParameters
         where

instance ToJSON MaintenanceWindowRunCommandParameters
         where
        toJSON MaintenanceWindowRunCommandParameters'{..}
          = object
              (catMaybes
                 [("ServiceRoleArn" .=) <$> _mwrcpServiceRoleARN,
                  ("NotificationConfig" .=) <$>
                    _mwrcpNotificationConfig,
                  ("DocumentHashType" .=) <$> _mwrcpDocumentHashType,
                  ("OutputS3KeyPrefix" .=) <$> _mwrcpOutputS3KeyPrefix,
                  ("Parameters" .=) <$> _mwrcpParameters,
                  ("DocumentHash" .=) <$> _mwrcpDocumentHash,
                  ("TimeoutSeconds" .=) <$> _mwrcpTimeoutSeconds,
                  ("Comment" .=) <$> _mwrcpComment,
                  ("OutputS3BucketName" .=) <$>
                    _mwrcpOutputS3BucketName])

-- | The parameters for a STEP_FUNCTION task.
--
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
--
-- /See:/ 'maintenanceWindowStepFunctionsParameters' smart constructor.
data MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters'
  { _mwsfpInput :: !(Maybe (Sensitive Text))
  , _mwsfpName  :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowStepFunctionsParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwsfpInput' - The inputs for the STEP_FUNCTION task.
--
-- * 'mwsfpName' - The name of the STEP_FUNCTION task.
maintenanceWindowStepFunctionsParameters
    :: MaintenanceWindowStepFunctionsParameters
maintenanceWindowStepFunctionsParameters =
  MaintenanceWindowStepFunctionsParameters'
    {_mwsfpInput = Nothing, _mwsfpName = Nothing}


-- | The inputs for the STEP_FUNCTION task.
mwsfpInput :: Lens' MaintenanceWindowStepFunctionsParameters (Maybe Text)
mwsfpInput = lens _mwsfpInput (\ s a -> s{_mwsfpInput = a}) . mapping _Sensitive

-- | The name of the STEP_FUNCTION task.
mwsfpName :: Lens' MaintenanceWindowStepFunctionsParameters (Maybe Text)
mwsfpName = lens _mwsfpName (\ s a -> s{_mwsfpName = a})

instance FromJSON
           MaintenanceWindowStepFunctionsParameters
         where
        parseJSON
          = withObject
              "MaintenanceWindowStepFunctionsParameters"
              (\ x ->
                 MaintenanceWindowStepFunctionsParameters' <$>
                   (x .:? "Input") <*> (x .:? "Name"))

instance Hashable
           MaintenanceWindowStepFunctionsParameters
         where

instance NFData
           MaintenanceWindowStepFunctionsParameters
         where

instance ToJSON
           MaintenanceWindowStepFunctionsParameters
         where
        toJSON MaintenanceWindowStepFunctionsParameters'{..}
          = object
              (catMaybes
                 [("Input" .=) <$> _mwsfpInput,
                  ("Name" .=) <$> _mwsfpName])

-- | The target registered with the Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { _mResourceType     :: !(Maybe MaintenanceWindowResourceType)
  , _mOwnerInformation :: !(Maybe (Sensitive Text))
  , _mWindowTargetId   :: !(Maybe Text)
  , _mName             :: !(Maybe Text)
  , _mTargets          :: !(Maybe [Target])
  , _mDescription      :: !(Maybe (Sensitive Text))
  , _mWindowId         :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mResourceType' - The type of target.
--
-- * 'mOwnerInformation' - User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
--
-- * 'mWindowTargetId' - The ID of the target.
--
-- * 'mName' - The target name.
--
-- * 'mTargets' - The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
--
-- * 'mDescription' - A description of the target.
--
-- * 'mWindowId' - The Maintenance Window ID where the target is registered.
maintenanceWindowTarget
    :: MaintenanceWindowTarget
maintenanceWindowTarget =
  MaintenanceWindowTarget'
    { _mResourceType = Nothing
    , _mOwnerInformation = Nothing
    , _mWindowTargetId = Nothing
    , _mName = Nothing
    , _mTargets = Nothing
    , _mDescription = Nothing
    , _mWindowId = Nothing
    }


-- | The type of target.
mResourceType :: Lens' MaintenanceWindowTarget (Maybe MaintenanceWindowResourceType)
mResourceType = lens _mResourceType (\ s a -> s{_mResourceType = a})

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this Maintenance Window.
mOwnerInformation :: Lens' MaintenanceWindowTarget (Maybe Text)
mOwnerInformation = lens _mOwnerInformation (\ s a -> s{_mOwnerInformation = a}) . mapping _Sensitive

-- | The ID of the target.
mWindowTargetId :: Lens' MaintenanceWindowTarget (Maybe Text)
mWindowTargetId = lens _mWindowTargetId (\ s a -> s{_mWindowTargetId = a})

-- | The target name.
mName :: Lens' MaintenanceWindowTarget (Maybe Text)
mName = lens _mName (\ s a -> s{_mName = a})

-- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
mTargets :: Lens' MaintenanceWindowTarget [Target]
mTargets = lens _mTargets (\ s a -> s{_mTargets = a}) . _Default . _Coerce

-- | A description of the target.
mDescription :: Lens' MaintenanceWindowTarget (Maybe Text)
mDescription = lens _mDescription (\ s a -> s{_mDescription = a}) . mapping _Sensitive

-- | The Maintenance Window ID where the target is registered.
mWindowId :: Lens' MaintenanceWindowTarget (Maybe Text)
mWindowId = lens _mWindowId (\ s a -> s{_mWindowId = a})

instance FromJSON MaintenanceWindowTarget where
        parseJSON
          = withObject "MaintenanceWindowTarget"
              (\ x ->
                 MaintenanceWindowTarget' <$>
                   (x .:? "ResourceType") <*> (x .:? "OwnerInformation")
                     <*> (x .:? "WindowTargetId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowTarget where

instance NFData MaintenanceWindowTarget where

-- | Information about a task defined for a Maintenance Window.
--
--
--
-- /See:/ 'maintenanceWindowTask' smart constructor.
data MaintenanceWindowTask = MaintenanceWindowTask'
  { _mwtServiceRoleARN :: !(Maybe Text)
  , _mwtWindowTaskId :: !(Maybe Text)
  , _mwtTaskParameters :: !(Maybe (Sensitive (Map Text (Sensitive MaintenanceWindowTaskParameterValueExpression))))
  , _mwtPriority :: !(Maybe Nat)
  , _mwtTaskARN :: !(Maybe Text)
  , _mwtMaxErrors :: !(Maybe Text)
  , _mwtName :: !(Maybe Text)
  , _mwtTargets :: !(Maybe [Target])
  , _mwtLoggingInfo :: !(Maybe LoggingInfo)
  , _mwtType :: !(Maybe MaintenanceWindowTaskType)
  , _mwtDescription :: !(Maybe (Sensitive Text))
  , _mwtMaxConcurrency :: !(Maybe Text)
  , _mwtWindowId :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtServiceRoleARN' - The role that should be assumed when executing the task
--
-- * 'mwtWindowTaskId' - The task ID.
--
-- * 'mwtTaskParameters' - The parameters that should be passed to the task when it is executed.
--
-- * 'mwtPriority' - The priority of the task in the Maintenance Window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- * 'mwtTaskARN' - The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTION tasks, it's the state machine ARN.
--
-- * 'mwtMaxErrors' - The maximum number of errors allowed before this task stops being scheduled.
--
-- * 'mwtName' - The task name.
--
-- * 'mwtTargets' - The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
--
-- * 'mwtLoggingInfo' - Information about an Amazon S3 bucket to write task-level logs to.
--
-- * 'mwtType' - The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTION.
--
-- * 'mwtDescription' - A description of the task.
--
-- * 'mwtMaxConcurrency' - The maximum number of targets this task can be run for in parallel.
--
-- * 'mwtWindowId' - The Maintenance Window ID where the task is registered.
maintenanceWindowTask
    :: MaintenanceWindowTask
maintenanceWindowTask =
  MaintenanceWindowTask'
    { _mwtServiceRoleARN = Nothing
    , _mwtWindowTaskId = Nothing
    , _mwtTaskParameters = Nothing
    , _mwtPriority = Nothing
    , _mwtTaskARN = Nothing
    , _mwtMaxErrors = Nothing
    , _mwtName = Nothing
    , _mwtTargets = Nothing
    , _mwtLoggingInfo = Nothing
    , _mwtType = Nothing
    , _mwtDescription = Nothing
    , _mwtMaxConcurrency = Nothing
    , _mwtWindowId = Nothing
    }


-- | The role that should be assumed when executing the task
mwtServiceRoleARN :: Lens' MaintenanceWindowTask (Maybe Text)
mwtServiceRoleARN = lens _mwtServiceRoleARN (\ s a -> s{_mwtServiceRoleARN = a})

-- | The task ID.
mwtWindowTaskId :: Lens' MaintenanceWindowTask (Maybe Text)
mwtWindowTaskId = lens _mwtWindowTaskId (\ s a -> s{_mwtWindowTaskId = a})

-- | The parameters that should be passed to the task when it is executed.
mwtTaskParameters :: Lens' MaintenanceWindowTask (Maybe (HashMap Text MaintenanceWindowTaskParameterValueExpression))
mwtTaskParameters = lens _mwtTaskParameters (\ s a -> s{_mwtTaskParameters = a}) . mapping (_Sensitive . _Map)

-- | The priority of the task in the Maintenance Window. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
mwtPriority :: Lens' MaintenanceWindowTask (Maybe Natural)
mwtPriority = lens _mwtPriority (\ s a -> s{_mwtPriority = a}) . mapping _Nat

-- | The resource that the task uses during execution. For RUN_COMMAND and AUTOMATION task types, @TaskArn@ is the Systems Manager document name or ARN. For LAMBDA tasks, it's the function name or ARN. For STEP_FUNCTION tasks, it's the state machine ARN.
mwtTaskARN :: Lens' MaintenanceWindowTask (Maybe Text)
mwtTaskARN = lens _mwtTaskARN (\ s a -> s{_mwtTaskARN = a})

-- | The maximum number of errors allowed before this task stops being scheduled.
mwtMaxErrors :: Lens' MaintenanceWindowTask (Maybe Text)
mwtMaxErrors = lens _mwtMaxErrors (\ s a -> s{_mwtMaxErrors = a})

-- | The task name.
mwtName :: Lens' MaintenanceWindowTask (Maybe Text)
mwtName = lens _mwtName (\ s a -> s{_mwtName = a})

-- | The targets (either instances or tags). Instances are specified using Key=instanceids,Values=<instanceid1>,<instanceid2>. Tags are specified using Key=<tag name>,Values=<tag value>.
mwtTargets :: Lens' MaintenanceWindowTask [Target]
mwtTargets = lens _mwtTargets (\ s a -> s{_mwtTargets = a}) . _Default . _Coerce

-- | Information about an Amazon S3 bucket to write task-level logs to.
mwtLoggingInfo :: Lens' MaintenanceWindowTask (Maybe LoggingInfo)
mwtLoggingInfo = lens _mwtLoggingInfo (\ s a -> s{_mwtLoggingInfo = a})

-- | The type of task. The type can be one of the following: RUN_COMMAND, AUTOMATION, LAMBDA, or STEP_FUNCTION.
mwtType :: Lens' MaintenanceWindowTask (Maybe MaintenanceWindowTaskType)
mwtType = lens _mwtType (\ s a -> s{_mwtType = a})

-- | A description of the task.
mwtDescription :: Lens' MaintenanceWindowTask (Maybe Text)
mwtDescription = lens _mwtDescription (\ s a -> s{_mwtDescription = a}) . mapping _Sensitive

-- | The maximum number of targets this task can be run for in parallel.
mwtMaxConcurrency :: Lens' MaintenanceWindowTask (Maybe Text)
mwtMaxConcurrency = lens _mwtMaxConcurrency (\ s a -> s{_mwtMaxConcurrency = a})

-- | The Maintenance Window ID where the task is registered.
mwtWindowId :: Lens' MaintenanceWindowTask (Maybe Text)
mwtWindowId = lens _mwtWindowId (\ s a -> s{_mwtWindowId = a})

instance FromJSON MaintenanceWindowTask where
        parseJSON
          = withObject "MaintenanceWindowTask"
              (\ x ->
                 MaintenanceWindowTask' <$>
                   (x .:? "ServiceRoleArn") <*> (x .:? "WindowTaskId")
                     <*> (x .:? "TaskParameters" .!= mempty)
                     <*> (x .:? "Priority")
                     <*> (x .:? "TaskArn")
                     <*> (x .:? "MaxErrors")
                     <*> (x .:? "Name")
                     <*> (x .:? "Targets" .!= mempty)
                     <*> (x .:? "LoggingInfo")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description")
                     <*> (x .:? "MaxConcurrency")
                     <*> (x .:? "WindowId"))

instance Hashable MaintenanceWindowTask where

instance NFData MaintenanceWindowTask where

-- | The parameters for task execution.
--
--
--
-- /See:/ 'maintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { _mwtipAutomation    :: !(Maybe MaintenanceWindowAutomationParameters)
  , _mwtipStepFunctions :: !(Maybe MaintenanceWindowStepFunctionsParameters)
  , _mwtipRunCommand    :: !(Maybe MaintenanceWindowRunCommandParameters)
  , _mwtipLambda        :: !(Maybe MaintenanceWindowLambdaParameters)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowTaskInvocationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtipAutomation' - The parameters for an AUTOMATION task type.
--
-- * 'mwtipStepFunctions' - The parameters for a STEP_FUNCTION task type.
--
-- * 'mwtipRunCommand' - The parameters for a RUN_COMMAND task type.
--
-- * 'mwtipLambda' - The parameters for a LAMBDA task type.
maintenanceWindowTaskInvocationParameters
    :: MaintenanceWindowTaskInvocationParameters
maintenanceWindowTaskInvocationParameters =
  MaintenanceWindowTaskInvocationParameters'
    { _mwtipAutomation = Nothing
    , _mwtipStepFunctions = Nothing
    , _mwtipRunCommand = Nothing
    , _mwtipLambda = Nothing
    }


-- | The parameters for an AUTOMATION task type.
mwtipAutomation :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowAutomationParameters)
mwtipAutomation = lens _mwtipAutomation (\ s a -> s{_mwtipAutomation = a})

-- | The parameters for a STEP_FUNCTION task type.
mwtipStepFunctions :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowStepFunctionsParameters)
mwtipStepFunctions = lens _mwtipStepFunctions (\ s a -> s{_mwtipStepFunctions = a})

-- | The parameters for a RUN_COMMAND task type.
mwtipRunCommand :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowRunCommandParameters)
mwtipRunCommand = lens _mwtipRunCommand (\ s a -> s{_mwtipRunCommand = a})

-- | The parameters for a LAMBDA task type.
mwtipLambda :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowLambdaParameters)
mwtipLambda = lens _mwtipLambda (\ s a -> s{_mwtipLambda = a})

instance FromJSON
           MaintenanceWindowTaskInvocationParameters
         where
        parseJSON
          = withObject
              "MaintenanceWindowTaskInvocationParameters"
              (\ x ->
                 MaintenanceWindowTaskInvocationParameters' <$>
                   (x .:? "Automation") <*> (x .:? "StepFunctions") <*>
                     (x .:? "RunCommand")
                     <*> (x .:? "Lambda"))

instance Hashable
           MaintenanceWindowTaskInvocationParameters
         where

instance NFData
           MaintenanceWindowTaskInvocationParameters
         where

instance ToJSON
           MaintenanceWindowTaskInvocationParameters
         where
        toJSON MaintenanceWindowTaskInvocationParameters'{..}
          = object
              (catMaybes
                 [("Automation" .=) <$> _mwtipAutomation,
                  ("StepFunctions" .=) <$> _mwtipStepFunctions,
                  ("RunCommand" .=) <$> _mwtipRunCommand,
                  ("Lambda" .=) <$> _mwtipLambda])

-- | Defines the values for a task parameter.
--
--
--
-- /See:/ 'maintenanceWindowTaskParameterValueExpression' smart constructor.
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
  { _mwtpveValues :: Maybe (Sensitive [Sensitive Text])
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaintenanceWindowTaskParameterValueExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtpveValues' - This field contains an array of 0 or more strings, each 1 to 255 characters in length.
maintenanceWindowTaskParameterValueExpression
    :: MaintenanceWindowTaskParameterValueExpression
maintenanceWindowTaskParameterValueExpression =
  MaintenanceWindowTaskParameterValueExpression' {_mwtpveValues = Nothing}


-- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
mwtpveValues :: Lens' MaintenanceWindowTaskParameterValueExpression (Maybe [Text])
mwtpveValues = lens _mwtpveValues (\ s a -> s{_mwtpveValues = a}) . mapping (_Sensitive . _Coerce)

instance FromJSON
           MaintenanceWindowTaskParameterValueExpression
         where
        parseJSON
          = withObject
              "MaintenanceWindowTaskParameterValueExpression"
              (\ x ->
                 MaintenanceWindowTaskParameterValueExpression' <$>
                   (x .:? "Values" .!= mempty))

instance Hashable
           MaintenanceWindowTaskParameterValueExpression
         where

instance NFData
           MaintenanceWindowTaskParameterValueExpression
         where

instance ToJSON
           MaintenanceWindowTaskParameterValueExpression
         where
        toJSON
          MaintenanceWindowTaskParameterValueExpression'{..}
          = object
              (catMaybes [("Values" .=) <$> _mwtpveValues])

-- | A summary of resources that are not compliant. The summary is organized according to resource type.
--
--
--
-- /See:/ 'nonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { _ncsNonCompliantCount :: !(Maybe Int)
  , _ncsSeveritySummary   :: !(Maybe SeveritySummary)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NonCompliantSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncsNonCompliantCount' - The total number of compliance items that are not compliant.
--
-- * 'ncsSeveritySummary' - A summary of the non-compliance severity by compliance type
nonCompliantSummary
    :: NonCompliantSummary
nonCompliantSummary =
  NonCompliantSummary'
    {_ncsNonCompliantCount = Nothing, _ncsSeveritySummary = Nothing}


-- | The total number of compliance items that are not compliant.
ncsNonCompliantCount :: Lens' NonCompliantSummary (Maybe Int)
ncsNonCompliantCount = lens _ncsNonCompliantCount (\ s a -> s{_ncsNonCompliantCount = a})

-- | A summary of the non-compliance severity by compliance type
ncsSeveritySummary :: Lens' NonCompliantSummary (Maybe SeveritySummary)
ncsSeveritySummary = lens _ncsSeveritySummary (\ s a -> s{_ncsSeveritySummary = a})

instance FromJSON NonCompliantSummary where
        parseJSON
          = withObject "NonCompliantSummary"
              (\ x ->
                 NonCompliantSummary' <$>
                   (x .:? "NonCompliantCount") <*>
                     (x .:? "SeveritySummary"))

instance Hashable NonCompliantSummary where

instance NFData NonCompliantSummary where

-- | Configurations for sending notifications.
--
--
--
-- /See:/ 'notificationConfig' smart constructor.
data NotificationConfig = NotificationConfig'
  { _ncNotificationEvents :: !(Maybe [NotificationEvent])
  , _ncNotificationType   :: !(Maybe NotificationType)
  , _ncNotificationARN    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncNotificationEvents' - The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Setting Up Events and Notifications> in the /AWS Systems Manager User Guide/ .
--
-- * 'ncNotificationType' - Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
--
-- * 'ncNotificationARN' - An Amazon Resource Name (ARN) for a Simple Notification Service (SNS) topic. Run Command pushes notifications about command status changes to this topic.
notificationConfig
    :: NotificationConfig
notificationConfig =
  NotificationConfig'
    { _ncNotificationEvents = Nothing
    , _ncNotificationType = Nothing
    , _ncNotificationARN = Nothing
    }


-- | The different events for which you can receive notifications. These events include the following: All (events), InProgress, Success, TimedOut, Cancelled, Failed. To learn more about these events, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/monitor-commands.html Setting Up Events and Notifications> in the /AWS Systems Manager User Guide/ .
ncNotificationEvents :: Lens' NotificationConfig [NotificationEvent]
ncNotificationEvents = lens _ncNotificationEvents (\ s a -> s{_ncNotificationEvents = a}) . _Default . _Coerce

-- | Command: Receive notification when the status of a command changes. Invocation: For commands sent to multiple instances, receive notification on a per-instance basis when the status of a command changes.
ncNotificationType :: Lens' NotificationConfig (Maybe NotificationType)
ncNotificationType = lens _ncNotificationType (\ s a -> s{_ncNotificationType = a})

-- | An Amazon Resource Name (ARN) for a Simple Notification Service (SNS) topic. Run Command pushes notifications about command status changes to this topic.
ncNotificationARN :: Lens' NotificationConfig (Maybe Text)
ncNotificationARN = lens _ncNotificationARN (\ s a -> s{_ncNotificationARN = a})

instance FromJSON NotificationConfig where
        parseJSON
          = withObject "NotificationConfig"
              (\ x ->
                 NotificationConfig' <$>
                   (x .:? "NotificationEvents" .!= mempty) <*>
                     (x .:? "NotificationType")
                     <*> (x .:? "NotificationArn"))

instance Hashable NotificationConfig where

instance NFData NotificationConfig where

instance ToJSON NotificationConfig where
        toJSON NotificationConfig'{..}
          = object
              (catMaybes
                 [("NotificationEvents" .=) <$> _ncNotificationEvents,
                  ("NotificationType" .=) <$> _ncNotificationType,
                  ("NotificationArn" .=) <$> _ncNotificationARN])

-- | An Amazon EC2 Systems Manager parameter in Parameter Store.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pValue   :: !(Maybe Text)
  , _pName    :: !(Maybe Text)
  , _pVersion :: !(Maybe Integer)
  , _pType    :: !(Maybe ParameterType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pValue' - The parameter value.
--
-- * 'pName' - The name of the parameter.
--
-- * 'pVersion' - The parameter version.
--
-- * 'pType' - The type of parameter. Valid values include the following: String, String list, Secure string.
parameter
    :: Parameter
parameter =
  Parameter'
    {_pValue = Nothing, _pName = Nothing, _pVersion = Nothing, _pType = Nothing}


-- | The parameter value.
pValue :: Lens' Parameter (Maybe Text)
pValue = lens _pValue (\ s a -> s{_pValue = a})

-- | The name of the parameter.
pName :: Lens' Parameter (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | The parameter version.
pVersion :: Lens' Parameter (Maybe Integer)
pVersion = lens _pVersion (\ s a -> s{_pVersion = a})

-- | The type of parameter. Valid values include the following: String, String list, Secure string.
pType :: Lens' Parameter (Maybe ParameterType)
pType = lens _pType (\ s a -> s{_pType = a})

instance FromJSON Parameter where
        parseJSON
          = withObject "Parameter"
              (\ x ->
                 Parameter' <$>
                   (x .:? "Value") <*> (x .:? "Name") <*>
                     (x .:? "Version")
                     <*> (x .:? "Type"))

instance Hashable Parameter where

instance NFData Parameter where

-- | Information about parameter usage.
--
--
--
-- /See:/ 'parameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { _phLastModifiedDate :: !(Maybe POSIX)
  , _phKeyId            :: !(Maybe Text)
  , _phValue            :: !(Maybe Text)
  , _phName             :: !(Maybe Text)
  , _phVersion          :: !(Maybe Integer)
  , _phLastModifiedUser :: !(Maybe Text)
  , _phAllowedPattern   :: !(Maybe Text)
  , _phType             :: !(Maybe ParameterType)
  , _phDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phLastModifiedDate' - Date the parameter was last changed or updated.
--
-- * 'phKeyId' - The ID of the query key used for this parameter.
--
-- * 'phValue' - The parameter value.
--
-- * 'phName' - The name of the parameter.
--
-- * 'phVersion' - The parameter version.
--
-- * 'phLastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- * 'phAllowedPattern' - Parameter names can include the following letters and symbols. a-zA-Z0-9_.-
--
-- * 'phType' - The type of parameter used.
--
-- * 'phDescription' - Information about the parameter.
parameterHistory
    :: ParameterHistory
parameterHistory =
  ParameterHistory'
    { _phLastModifiedDate = Nothing
    , _phKeyId = Nothing
    , _phValue = Nothing
    , _phName = Nothing
    , _phVersion = Nothing
    , _phLastModifiedUser = Nothing
    , _phAllowedPattern = Nothing
    , _phType = Nothing
    , _phDescription = Nothing
    }


-- | Date the parameter was last changed or updated.
phLastModifiedDate :: Lens' ParameterHistory (Maybe UTCTime)
phLastModifiedDate = lens _phLastModifiedDate (\ s a -> s{_phLastModifiedDate = a}) . mapping _Time

-- | The ID of the query key used for this parameter.
phKeyId :: Lens' ParameterHistory (Maybe Text)
phKeyId = lens _phKeyId (\ s a -> s{_phKeyId = a})

-- | The parameter value.
phValue :: Lens' ParameterHistory (Maybe Text)
phValue = lens _phValue (\ s a -> s{_phValue = a})

-- | The name of the parameter.
phName :: Lens' ParameterHistory (Maybe Text)
phName = lens _phName (\ s a -> s{_phName = a})

-- | The parameter version.
phVersion :: Lens' ParameterHistory (Maybe Integer)
phVersion = lens _phVersion (\ s a -> s{_phVersion = a})

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
phLastModifiedUser :: Lens' ParameterHistory (Maybe Text)
phLastModifiedUser = lens _phLastModifiedUser (\ s a -> s{_phLastModifiedUser = a})

-- | Parameter names can include the following letters and symbols. a-zA-Z0-9_.-
phAllowedPattern :: Lens' ParameterHistory (Maybe Text)
phAllowedPattern = lens _phAllowedPattern (\ s a -> s{_phAllowedPattern = a})

-- | The type of parameter used.
phType :: Lens' ParameterHistory (Maybe ParameterType)
phType = lens _phType (\ s a -> s{_phType = a})

-- | Information about the parameter.
phDescription :: Lens' ParameterHistory (Maybe Text)
phDescription = lens _phDescription (\ s a -> s{_phDescription = a})

instance FromJSON ParameterHistory where
        parseJSON
          = withObject "ParameterHistory"
              (\ x ->
                 ParameterHistory' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "KeyId") <*>
                     (x .:? "Value")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "LastModifiedUser")
                     <*> (x .:? "AllowedPattern")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ParameterHistory where

instance NFData ParameterHistory where

-- | Metada includes information like the ARN of the last user and the date/time the parameter was last used.
--
--
--
-- /See:/ 'parameterMetadata' smart constructor.
data ParameterMetadata = ParameterMetadata'
  { _pmLastModifiedDate :: !(Maybe POSIX)
  , _pmKeyId            :: !(Maybe Text)
  , _pmName             :: !(Maybe Text)
  , _pmVersion          :: !(Maybe Integer)
  , _pmLastModifiedUser :: !(Maybe Text)
  , _pmAllowedPattern   :: !(Maybe Text)
  , _pmType             :: !(Maybe ParameterType)
  , _pmDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmLastModifiedDate' - Date the parameter was last changed or updated.
--
-- * 'pmKeyId' - The ID of the query key used for this parameter.
--
-- * 'pmName' - The parameter name.
--
-- * 'pmVersion' - The parameter version.
--
-- * 'pmLastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
--
-- * 'pmAllowedPattern' - A parameter name can include only the following letters and symbols. a-zA-Z0-9_.-
--
-- * 'pmType' - The type of parameter. Valid parameter types include the following: String, String list, Secure string.
--
-- * 'pmDescription' - Description of the parameter actions.
parameterMetadata
    :: ParameterMetadata
parameterMetadata =
  ParameterMetadata'
    { _pmLastModifiedDate = Nothing
    , _pmKeyId = Nothing
    , _pmName = Nothing
    , _pmVersion = Nothing
    , _pmLastModifiedUser = Nothing
    , _pmAllowedPattern = Nothing
    , _pmType = Nothing
    , _pmDescription = Nothing
    }


-- | Date the parameter was last changed or updated.
pmLastModifiedDate :: Lens' ParameterMetadata (Maybe UTCTime)
pmLastModifiedDate = lens _pmLastModifiedDate (\ s a -> s{_pmLastModifiedDate = a}) . mapping _Time

-- | The ID of the query key used for this parameter.
pmKeyId :: Lens' ParameterMetadata (Maybe Text)
pmKeyId = lens _pmKeyId (\ s a -> s{_pmKeyId = a})

-- | The parameter name.
pmName :: Lens' ParameterMetadata (Maybe Text)
pmName = lens _pmName (\ s a -> s{_pmName = a})

-- | The parameter version.
pmVersion :: Lens' ParameterMetadata (Maybe Integer)
pmVersion = lens _pmVersion (\ s a -> s{_pmVersion = a})

-- | Amazon Resource Name (ARN) of the AWS user who last changed the parameter.
pmLastModifiedUser :: Lens' ParameterMetadata (Maybe Text)
pmLastModifiedUser = lens _pmLastModifiedUser (\ s a -> s{_pmLastModifiedUser = a})

-- | A parameter name can include only the following letters and symbols. a-zA-Z0-9_.-
pmAllowedPattern :: Lens' ParameterMetadata (Maybe Text)
pmAllowedPattern = lens _pmAllowedPattern (\ s a -> s{_pmAllowedPattern = a})

-- | The type of parameter. Valid parameter types include the following: String, String list, Secure string.
pmType :: Lens' ParameterMetadata (Maybe ParameterType)
pmType = lens _pmType (\ s a -> s{_pmType = a})

-- | Description of the parameter actions.
pmDescription :: Lens' ParameterMetadata (Maybe Text)
pmDescription = lens _pmDescription (\ s a -> s{_pmDescription = a})

instance FromJSON ParameterMetadata where
        parseJSON
          = withObject "ParameterMetadata"
              (\ x ->
                 ParameterMetadata' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "KeyId") <*>
                     (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "LastModifiedUser")
                     <*> (x .:? "AllowedPattern")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ParameterMetadata where

instance NFData ParameterMetadata where

-- | One or more filters. Use a filter to return a more specific list of results.
--
--
--
-- /See:/ 'parameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'
  { _psfValues :: !(Maybe (List1 Text))
  , _psfOption :: !(Maybe Text)
  , _psfKey    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psfValues' - The value you want to search for.
--
-- * 'psfOption' - Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.
--
-- * 'psfKey' - The name of the filter.
parameterStringFilter
    :: Text -- ^ 'psfKey'
    -> ParameterStringFilter
parameterStringFilter pKey_ =
  ParameterStringFilter'
    {_psfValues = Nothing, _psfOption = Nothing, _psfKey = pKey_}


-- | The value you want to search for.
psfValues :: Lens' ParameterStringFilter (Maybe (NonEmpty Text))
psfValues = lens _psfValues (\ s a -> s{_psfValues = a}) . mapping _List1

-- | Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.
psfOption :: Lens' ParameterStringFilter (Maybe Text)
psfOption = lens _psfOption (\ s a -> s{_psfOption = a})

-- | The name of the filter.
psfKey :: Lens' ParameterStringFilter Text
psfKey = lens _psfKey (\ s a -> s{_psfKey = a})

instance Hashable ParameterStringFilter where

instance NFData ParameterStringFilter where

instance ToJSON ParameterStringFilter where
        toJSON ParameterStringFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _psfValues,
                  ("Option" .=) <$> _psfOption,
                  Just ("Key" .= _psfKey)])

-- | This data type is deprecated. Instead, use 'ParameterStringFilter' .
--
--
--
-- /See:/ 'parametersFilter' smart constructor.
data ParametersFilter = ParametersFilter'
  { _pKey    :: !ParametersFilterKey
  , _pValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParametersFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pKey' - The name of the filter.
--
-- * 'pValues' - The filter values.
parametersFilter
    :: ParametersFilterKey -- ^ 'pKey'
    -> NonEmpty Text -- ^ 'pValues'
    -> ParametersFilter
parametersFilter pKey_ pValues_ =
  ParametersFilter' {_pKey = pKey_, _pValues = _List1 # pValues_}


-- | The name of the filter.
pKey :: Lens' ParametersFilter ParametersFilterKey
pKey = lens _pKey (\ s a -> s{_pKey = a})

-- | The filter values.
pValues :: Lens' ParametersFilter (NonEmpty Text)
pValues = lens _pValues (\ s a -> s{_pValues = a}) . _List1

instance Hashable ParametersFilter where

instance NFData ParametersFilter where

instance ToJSON ParametersFilter where
        toJSON ParametersFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _pKey), Just ("Values" .= _pValues)])

-- | Represents metadata about a patch.
--
--
--
-- /See:/ 'patch' smart constructor.
data Patch = Patch'
  { _pVendor         :: !(Maybe Text)
  , _pMsrcSeverity   :: !(Maybe Text)
  , _pProductFamily  :: !(Maybe Text)
  , _pClassification :: !(Maybe Text)
  , _pMsrcNumber     :: !(Maybe Text)
  , _pLanguage       :: !(Maybe Text)
  , _pKbNumber       :: !(Maybe Text)
  , _pContentURL     :: !(Maybe Text)
  , _pId             :: !(Maybe Text)
  , _pReleaseDate    :: !(Maybe POSIX)
  , _pTitle          :: !(Maybe Text)
  , _pProduct        :: !(Maybe Text)
  , _pDescription    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Patch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pVendor' - The name of the vendor providing the patch.
--
-- * 'pMsrcSeverity' - The severity of the patch (for example Critical, Important, Moderate).
--
-- * 'pProductFamily' - The product family the patch is applicable for (for example, Windows).
--
-- * 'pClassification' - The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- * 'pMsrcNumber' - The ID of the MSRC bulletin the patch is related to.
--
-- * 'pLanguage' - The language of the patch if it's language-specific.
--
-- * 'pKbNumber' - The Microsoft Knowledge Base ID of the patch.
--
-- * 'pContentURL' - The URL where more information can be obtained about the patch.
--
-- * 'pId' - The ID of the patch (this is different than the Microsoft Knowledge Base ID).
--
-- * 'pReleaseDate' - The date the patch was released.
--
-- * 'pTitle' - The title of the patch.
--
-- * 'pProduct' - The specific product the patch is applicable for (for example, WindowsServer2016).
--
-- * 'pDescription' - The description of the patch.
patch
    :: Patch
patch =
  Patch'
    { _pVendor = Nothing
    , _pMsrcSeverity = Nothing
    , _pProductFamily = Nothing
    , _pClassification = Nothing
    , _pMsrcNumber = Nothing
    , _pLanguage = Nothing
    , _pKbNumber = Nothing
    , _pContentURL = Nothing
    , _pId = Nothing
    , _pReleaseDate = Nothing
    , _pTitle = Nothing
    , _pProduct = Nothing
    , _pDescription = Nothing
    }


-- | The name of the vendor providing the patch.
pVendor :: Lens' Patch (Maybe Text)
pVendor = lens _pVendor (\ s a -> s{_pVendor = a})

-- | The severity of the patch (for example Critical, Important, Moderate).
pMsrcSeverity :: Lens' Patch (Maybe Text)
pMsrcSeverity = lens _pMsrcSeverity (\ s a -> s{_pMsrcSeverity = a})

-- | The product family the patch is applicable for (for example, Windows).
pProductFamily :: Lens' Patch (Maybe Text)
pProductFamily = lens _pProductFamily (\ s a -> s{_pProductFamily = a})

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
pClassification :: Lens' Patch (Maybe Text)
pClassification = lens _pClassification (\ s a -> s{_pClassification = a})

-- | The ID of the MSRC bulletin the patch is related to.
pMsrcNumber :: Lens' Patch (Maybe Text)
pMsrcNumber = lens _pMsrcNumber (\ s a -> s{_pMsrcNumber = a})

-- | The language of the patch if it's language-specific.
pLanguage :: Lens' Patch (Maybe Text)
pLanguage = lens _pLanguage (\ s a -> s{_pLanguage = a})

-- | The Microsoft Knowledge Base ID of the patch.
pKbNumber :: Lens' Patch (Maybe Text)
pKbNumber = lens _pKbNumber (\ s a -> s{_pKbNumber = a})

-- | The URL where more information can be obtained about the patch.
pContentURL :: Lens' Patch (Maybe Text)
pContentURL = lens _pContentURL (\ s a -> s{_pContentURL = a})

-- | The ID of the patch (this is different than the Microsoft Knowledge Base ID).
pId :: Lens' Patch (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

-- | The date the patch was released.
pReleaseDate :: Lens' Patch (Maybe UTCTime)
pReleaseDate = lens _pReleaseDate (\ s a -> s{_pReleaseDate = a}) . mapping _Time

-- | The title of the patch.
pTitle :: Lens' Patch (Maybe Text)
pTitle = lens _pTitle (\ s a -> s{_pTitle = a})

-- | The specific product the patch is applicable for (for example, WindowsServer2016).
pProduct :: Lens' Patch (Maybe Text)
pProduct = lens _pProduct (\ s a -> s{_pProduct = a})

-- | The description of the patch.
pDescription :: Lens' Patch (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

instance FromJSON Patch where
        parseJSON
          = withObject "Patch"
              (\ x ->
                 Patch' <$>
                   (x .:? "Vendor") <*> (x .:? "MsrcSeverity") <*>
                     (x .:? "ProductFamily")
                     <*> (x .:? "Classification")
                     <*> (x .:? "MsrcNumber")
                     <*> (x .:? "Language")
                     <*> (x .:? "KbNumber")
                     <*> (x .:? "ContentUrl")
                     <*> (x .:? "Id")
                     <*> (x .:? "ReleaseDate")
                     <*> (x .:? "Title")
                     <*> (x .:? "Product")
                     <*> (x .:? "Description"))

instance Hashable Patch where

instance NFData Patch where

-- | Defines the basic information about a patch baseline.
--
--
--
-- /See:/ 'patchBaselineIdentity' smart constructor.
data PatchBaselineIdentity = PatchBaselineIdentity'
  { _pbiBaselineName        :: !(Maybe Text)
  , _pbiBaselineDescription :: !(Maybe Text)
  , _pbiOperatingSystem     :: !(Maybe OperatingSystem)
  , _pbiDefaultBaseline     :: !(Maybe Bool)
  , _pbiBaselineId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchBaselineIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbiBaselineName' - The name of the patch baseline.
--
-- * 'pbiBaselineDescription' - The description of the patch baseline.
--
-- * 'pbiOperatingSystem' - Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
--
-- * 'pbiDefaultBaseline' - Whether this is the default baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
--
-- * 'pbiBaselineId' - The ID of the patch baseline.
patchBaselineIdentity
    :: PatchBaselineIdentity
patchBaselineIdentity =
  PatchBaselineIdentity'
    { _pbiBaselineName = Nothing
    , _pbiBaselineDescription = Nothing
    , _pbiOperatingSystem = Nothing
    , _pbiDefaultBaseline = Nothing
    , _pbiBaselineId = Nothing
    }


-- | The name of the patch baseline.
pbiBaselineName :: Lens' PatchBaselineIdentity (Maybe Text)
pbiBaselineName = lens _pbiBaselineName (\ s a -> s{_pbiBaselineName = a})

-- | The description of the patch baseline.
pbiBaselineDescription :: Lens' PatchBaselineIdentity (Maybe Text)
pbiBaselineDescription = lens _pbiBaselineDescription (\ s a -> s{_pbiBaselineDescription = a})

-- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
pbiOperatingSystem :: Lens' PatchBaselineIdentity (Maybe OperatingSystem)
pbiOperatingSystem = lens _pbiOperatingSystem (\ s a -> s{_pbiOperatingSystem = a})

-- | Whether this is the default baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
pbiDefaultBaseline :: Lens' PatchBaselineIdentity (Maybe Bool)
pbiDefaultBaseline = lens _pbiDefaultBaseline (\ s a -> s{_pbiDefaultBaseline = a})

-- | The ID of the patch baseline.
pbiBaselineId :: Lens' PatchBaselineIdentity (Maybe Text)
pbiBaselineId = lens _pbiBaselineId (\ s a -> s{_pbiBaselineId = a})

instance FromJSON PatchBaselineIdentity where
        parseJSON
          = withObject "PatchBaselineIdentity"
              (\ x ->
                 PatchBaselineIdentity' <$>
                   (x .:? "BaselineName") <*>
                     (x .:? "BaselineDescription")
                     <*> (x .:? "OperatingSystem")
                     <*> (x .:? "DefaultBaseline")
                     <*> (x .:? "BaselineId"))

instance Hashable PatchBaselineIdentity where

instance NFData PatchBaselineIdentity where

-- | Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.
--
--
--
-- /See:/ 'patchComplianceData' smart constructor.
data PatchComplianceData = PatchComplianceData'
  { _pcdTitle          :: !Text
  , _pcdKBId           :: !Text
  , _pcdClassification :: !Text
  , _pcdSeverity       :: !Text
  , _pcdState          :: !PatchComplianceDataState
  , _pcdInstalledTime  :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchComplianceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcdTitle' - The title of the patch.
--
-- * 'pcdKBId' - The operating system-specific ID of the patch.
--
-- * 'pcdClassification' - The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- * 'pcdSeverity' - The severity of the patch (for example, Critical, Important, Moderate).
--
-- * 'pcdState' - The state of the patch on the instance (INSTALLED, INSTALLED_OTHER, MISSING, NOT_APPLICABLE or FAILED).
--
-- * 'pcdInstalledTime' - The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
patchComplianceData
    :: Text -- ^ 'pcdTitle'
    -> Text -- ^ 'pcdKBId'
    -> Text -- ^ 'pcdClassification'
    -> Text -- ^ 'pcdSeverity'
    -> PatchComplianceDataState -- ^ 'pcdState'
    -> UTCTime -- ^ 'pcdInstalledTime'
    -> PatchComplianceData
patchComplianceData pTitle_ pKBId_ pClassification_ pSeverity_ pState_ pInstalledTime_ =
  PatchComplianceData'
    { _pcdTitle = pTitle_
    , _pcdKBId = pKBId_
    , _pcdClassification = pClassification_
    , _pcdSeverity = pSeverity_
    , _pcdState = pState_
    , _pcdInstalledTime = _Time # pInstalledTime_
    }


-- | The title of the patch.
pcdTitle :: Lens' PatchComplianceData Text
pcdTitle = lens _pcdTitle (\ s a -> s{_pcdTitle = a})

-- | The operating system-specific ID of the patch.
pcdKBId :: Lens' PatchComplianceData Text
pcdKBId = lens _pcdKBId (\ s a -> s{_pcdKBId = a})

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
pcdClassification :: Lens' PatchComplianceData Text
pcdClassification = lens _pcdClassification (\ s a -> s{_pcdClassification = a})

-- | The severity of the patch (for example, Critical, Important, Moderate).
pcdSeverity :: Lens' PatchComplianceData Text
pcdSeverity = lens _pcdSeverity (\ s a -> s{_pcdSeverity = a})

-- | The state of the patch on the instance (INSTALLED, INSTALLED_OTHER, MISSING, NOT_APPLICABLE or FAILED).
pcdState :: Lens' PatchComplianceData PatchComplianceDataState
pcdState = lens _pcdState (\ s a -> s{_pcdState = a})

-- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
pcdInstalledTime :: Lens' PatchComplianceData UTCTime
pcdInstalledTime = lens _pcdInstalledTime (\ s a -> s{_pcdInstalledTime = a}) . _Time

instance FromJSON PatchComplianceData where
        parseJSON
          = withObject "PatchComplianceData"
              (\ x ->
                 PatchComplianceData' <$>
                   (x .: "Title") <*> (x .: "KBId") <*>
                     (x .: "Classification")
                     <*> (x .: "Severity")
                     <*> (x .: "State")
                     <*> (x .: "InstalledTime"))

instance Hashable PatchComplianceData where

instance NFData PatchComplianceData where

-- | Defines a patch filter.
--
--
-- A patch filter consists of key/value pairs, but not all keys are valid for all operating system types. For example, the key @PRODUCT@ is valid for all supported operating system types. The key @MSRC_SEVERITY@ , however, is valid only for Windows operating systems, and the key @SECTION@ is valid only for Ubuntu operating systems.
--
-- Refer to the following sections for information about which keys may be used with each major operating system, and which values are valid for each key.
--
-- __Windows Operating Systems__
--
-- The supported keys for Windows operating systems are @PRODUCT@ , @CLASSIFICATION@ , and @MSRC_SEVERITY@ . See the following lists for valid values for each of these keys.
--
-- /Supported key:/ @PRODUCT@
--
-- /Supported values:/
--
--     * @Windows7@
--
--     * @Windows8@
--
--     * @Windows8.1@
--
--     * @Windows8Embedded@
--
--     * @Windows10@
--
--     * @Windows10LTSB@
--
--     * @WindowsServer2008@
--
--     * @WindowsServer2008R2@
--
--     * @WindowsServer2012@
--
--     * @WindowsServer2012R2@
--
--     * @WindowsServer2016@
--
--
--
-- /Supported key:/ @CLASSIFICATION@
--
-- /Supported values:/
--
--     * @CriticalUpdates@
--
--     * @DefinitionUpdates@
--
--     * @Drivers@
--
--     * @FeaturePacks@
--
--     * @SecurityUpdates@
--
--     * @ServicePacks@
--
--     * @Tools@
--
--     * @UpdateRollups@
--
--     * @Updates@
--
--     * @Upgrades@
--
--
--
-- /Supported key:/ @MSRC_SEVERITY@
--
-- /Supported values:/
--
--     * @Critical@
--
--     * @Important@
--
--     * @Moderate@
--
--     * @Low@
--
--     * @Unspecified@
--
--
--
-- __Ubuntu Operating Systems__
--
-- The supported keys for Ubuntu operating systems are @PRODUCT@ , @PRIORITY@ , and @SECTION@ . See the following lists for valid values for each of these keys.
--
-- /Supported key:/ @PRODUCT@
--
-- /Supported values:/
--
--     * @Ubuntu14.04@
--
--     * @Ubuntu16.04@
--
--
--
-- /Supported key:/ @PRIORITY@
--
-- /Supported values:/
--
--     * @Required@
--
--     * @Important@
--
--     * @Standard@
--
--     * @Optional@
--
--     * @Extra@
--
--
--
-- /Supported key:/ @SECTION@
--
-- Only the length of the key value is validated. Minimum length is 1. Maximum length is 64.
--
-- __Amazon Linux Operating Systems__
--
-- The supported keys for Amazon Linux operating systems are @PRODUCT@ , @CLASSIFICATION@ , and @SEVERITY@ . See the following lists for valid values for each of these keys.
--
-- /Supported key:/ @PRODUCT@
--
-- /Supported values:/
--
--     * @AmazonLinux2012.03@
--
--     * @AmazonLinux2012.09@
--
--     * @AmazonLinux2013.03@
--
--     * @AmazonLinux2013.09@
--
--     * @AmazonLinux2014.03@
--
--     * @AmazonLinux2014.09@
--
--     * @AmazonLinux2015.03@
--
--     * @AmazonLinux2015.09@
--
--     * @AmazonLinux2016.03@
--
--     * @AmazonLinux2016.09@
--
--     * @AmazonLinux2017.03@
--
--     * @AmazonLinux2017.09@
--
--
--
-- /Supported key:/ @CLASSIFICATION@
--
-- /Supported values:/
--
--     * @Security@
--
--     * @Bugfix@
--
--     * @Enhancement@
--
--     * @Recommended@
--
--     * @Newpackage@
--
--
--
-- /Supported key:/ @SEVERITY@
--
-- /Supported values:/
--
--     * @Critical@
--
--     * @Important@
--
--     * @Medium@
--
--     * @Low@
--
--
--
-- __RedHat Enterprise Linux (RHEL) Operating Systems__
--
-- The supported keys for RedHat Enterprise Linux operating systems are @PRODUCT@ , @CLASSIFICATION@ , and @SEVERITY@ . See the following lists for valid values for each of these keys.
--
-- /Supported key:/ @PRODUCT@
--
-- /Supported values:/
--
--     * @RedhatEnterpriseLinux6.5@
--
--     * @RedhatEnterpriseLinux6.6@
--
--     * @RedhatEnterpriseLinux6.7@
--
--     * @RedhatEnterpriseLinux6.8@
--
--     * @RedhatEnterpriseLinux6.9@
--
--     * @RedhatEnterpriseLinux7.0@
--
--     * @RedhatEnterpriseLinux7.1@
--
--     * @RedhatEnterpriseLinux7.2@
--
--     * @RedhatEnterpriseLinux7.3@
--
--     * @RedhatEnterpriseLinux7.4@
--
--
--
-- /Supported key:/ @CLASSIFICATION@
--
-- /Supported values:/
--
--     * @Security@
--
--     * @Bugfix@
--
--     * @Enhancement@
--
--     * @Recommended@
--
--     * @Newpackage@
--
--
--
-- /Supported key:/ @SEVERITY@
--
-- /Supported values:/
--
--     * @Critical@
--
--     * @Important@
--
--     * @Medium@
--
--     * @Low@
--
--
--
-- __SUSE Linux Enterprise Server (SLES) Operating Systems__
--
-- The supported keys for SLES operating systems are @PRODUCT@ , @CLASSIFICATION@ , and @SEVERITY@ . See the following lists for valid values for each of these keys.
--
-- /Supported key:/ @PRODUCT@
--
-- /Supported values:/
--
--     * @Suse12.0@
--
--     * @Suse12.1@
--
--     * @Suse12.2@
--
--     * @Suse12.3@
--
--     * @Suse12.4@
--
--     * @Suse12.5@
--
--     * @Suse12.6@
--
--     * @Suse12.7@
--
--     * @Suse12.8@
--
--     * @Suse12.9@
--
--
--
-- /Supported key:/ @CLASSIFICATION@
--
-- /Supported values:/
--
--     * @Security@
--
--     * @Recommended@
--
--     * @Optional@
--
--     * @Feature@
--
--     * @Document@
--
--     * @Yast@
--
--
--
-- /Supported key:/ @SEVERITY@
--
-- /Supported values:/
--
--     * @Critical@
--
--     * @Important@
--
--     * @Moderate@
--
--     * @Low@
--
--
--
-- __CentOS Operating Systems__
--
-- The supported keys for CentOS operating systems are @PRODUCT@ , @CLASSIFICATION@ , and @SEVERITY@ . See the following lists for valid values for each of these keys.
--
-- /Supported key:/ @PRODUCT@
--
-- /Supported values:/
--
--     * @CentOS6.5@
--
--     * @CentOS6.6@
--
--     * @CentOS6.7@
--
--     * @CentOS6.8@
--
--     * @CentOS6.9@
--
--     * @CentOS7.0@
--
--     * @CentOS7.1@
--
--     * @CentOS7.2@
--
--     * @CentOS7.3@
--
--     * @CentOS7.4@
--
--
--
-- /Supported key:/ @CLASSIFICATION@
--
-- /Supported values:/
--
--     * @Security@
--
--     * @Bugfix@
--
--     * @Enhancement@
--
--     * @Recommended@
--
--     * @Newpackage@
--
--
--
-- /Supported key:/ @SEVERITY@
--
-- /Supported values:/
--
--     * @Critical@
--
--     * @Important@
--
--     * @Medium@
--
--     * @Low@
--
--
--
--
-- /See:/ 'patchFilter' smart constructor.
data PatchFilter = PatchFilter'
  { _pfKey    :: !PatchFilterKey
  , _pfValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfKey' - The key for the filter. See 'PatchFilter' for lists of valid keys for each operating system type.
--
-- * 'pfValues' - The value for the filter key. See 'PatchFilter' for lists of valid values for each key based on operating system type.
patchFilter
    :: PatchFilterKey -- ^ 'pfKey'
    -> NonEmpty Text -- ^ 'pfValues'
    -> PatchFilter
patchFilter pKey_ pValues_ =
  PatchFilter' {_pfKey = pKey_, _pfValues = _List1 # pValues_}


-- | The key for the filter. See 'PatchFilter' for lists of valid keys for each operating system type.
pfKey :: Lens' PatchFilter PatchFilterKey
pfKey = lens _pfKey (\ s a -> s{_pfKey = a})

-- | The value for the filter key. See 'PatchFilter' for lists of valid values for each key based on operating system type.
pfValues :: Lens' PatchFilter (NonEmpty Text)
pfValues = lens _pfValues (\ s a -> s{_pfValues = a}) . _List1

instance FromJSON PatchFilter where
        parseJSON
          = withObject "PatchFilter"
              (\ x ->
                 PatchFilter' <$> (x .: "Key") <*> (x .: "Values"))

instance Hashable PatchFilter where

instance NFData PatchFilter where

instance ToJSON PatchFilter where
        toJSON PatchFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _pfKey),
                  Just ("Values" .= _pfValues)])

-- | A set of patch filters, typically used for approval rules.
--
--
--
-- /See:/ 'patchFilterGroup' smart constructor.
newtype PatchFilterGroup = PatchFilterGroup'
  { _pfgPatchFilters :: [PatchFilter]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchFilterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfgPatchFilters' - The set of patch filters that make up the group.
patchFilterGroup
    :: PatchFilterGroup
patchFilterGroup = PatchFilterGroup' {_pfgPatchFilters = mempty}


-- | The set of patch filters that make up the group.
pfgPatchFilters :: Lens' PatchFilterGroup [PatchFilter]
pfgPatchFilters = lens _pfgPatchFilters (\ s a -> s{_pfgPatchFilters = a}) . _Coerce

instance FromJSON PatchFilterGroup where
        parseJSON
          = withObject "PatchFilterGroup"
              (\ x ->
                 PatchFilterGroup' <$>
                   (x .:? "PatchFilters" .!= mempty))

instance Hashable PatchFilterGroup where

instance NFData PatchFilterGroup where

instance ToJSON PatchFilterGroup where
        toJSON PatchFilterGroup'{..}
          = object
              (catMaybes
                 [Just ("PatchFilters" .= _pfgPatchFilters)])

-- | The mapping between a patch group and the patch baseline the patch group is registered with.
--
--
--
-- /See:/ 'patchGroupPatchBaselineMapping' smart constructor.
data PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping'
  { _pgpbmBaselineIdentity :: !(Maybe PatchBaselineIdentity)
  , _pgpbmPatchGroup       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchGroupPatchBaselineMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgpbmBaselineIdentity' - The patch baseline the patch group is registered with.
--
-- * 'pgpbmPatchGroup' - The name of the patch group registered with the patch baseline.
patchGroupPatchBaselineMapping
    :: PatchGroupPatchBaselineMapping
patchGroupPatchBaselineMapping =
  PatchGroupPatchBaselineMapping'
    {_pgpbmBaselineIdentity = Nothing, _pgpbmPatchGroup = Nothing}


-- | The patch baseline the patch group is registered with.
pgpbmBaselineIdentity :: Lens' PatchGroupPatchBaselineMapping (Maybe PatchBaselineIdentity)
pgpbmBaselineIdentity = lens _pgpbmBaselineIdentity (\ s a -> s{_pgpbmBaselineIdentity = a})

-- | The name of the patch group registered with the patch baseline.
pgpbmPatchGroup :: Lens' PatchGroupPatchBaselineMapping (Maybe Text)
pgpbmPatchGroup = lens _pgpbmPatchGroup (\ s a -> s{_pgpbmPatchGroup = a})

instance FromJSON PatchGroupPatchBaselineMapping
         where
        parseJSON
          = withObject "PatchGroupPatchBaselineMapping"
              (\ x ->
                 PatchGroupPatchBaselineMapping' <$>
                   (x .:? "BaselineIdentity") <*> (x .:? "PatchGroup"))

instance Hashable PatchGroupPatchBaselineMapping
         where

instance NFData PatchGroupPatchBaselineMapping where

-- | Defines a filter used in Patch Manager APIs.
--
--
--
-- /See:/ 'patchOrchestratorFilter' smart constructor.
data PatchOrchestratorFilter = PatchOrchestratorFilter'
  { _pofValues :: !(Maybe [Text])
  , _pofKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchOrchestratorFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pofValues' - The value for the filter.
--
-- * 'pofKey' - The key for the filter.
patchOrchestratorFilter
    :: PatchOrchestratorFilter
patchOrchestratorFilter =
  PatchOrchestratorFilter' {_pofValues = Nothing, _pofKey = Nothing}


-- | The value for the filter.
pofValues :: Lens' PatchOrchestratorFilter [Text]
pofValues = lens _pofValues (\ s a -> s{_pofValues = a}) . _Default . _Coerce

-- | The key for the filter.
pofKey :: Lens' PatchOrchestratorFilter (Maybe Text)
pofKey = lens _pofKey (\ s a -> s{_pofKey = a})

instance Hashable PatchOrchestratorFilter where

instance NFData PatchOrchestratorFilter where

instance ToJSON PatchOrchestratorFilter where
        toJSON PatchOrchestratorFilter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _pofValues,
                  ("Key" .=) <$> _pofKey])

-- | Defines an approval rule for a patch baseline.
--
--
--
-- /See:/ 'patchRule' smart constructor.
data PatchRule = PatchRule'
  { _prEnableNonSecurity :: !(Maybe Bool)
  , _prComplianceLevel   :: !(Maybe PatchComplianceLevel)
  , _prPatchFilterGroup  :: !PatchFilterGroup
  , _prApproveAfterDays  :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prEnableNonSecurity' - For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
--
-- * 'prComplianceLevel' - A compliance severity level for all approved patches in a patch baseline. Valid compliance severity levels include the following: Unspecified, Critical, High, Medium, Low, and Informational.
--
-- * 'prPatchFilterGroup' - The patch filter group that defines the criteria for the rule.
--
-- * 'prApproveAfterDays' - The number of days after the release date of each patch matched by the rule the patch is marked as approved in the patch baseline.
patchRule
    :: PatchFilterGroup -- ^ 'prPatchFilterGroup'
    -> Natural -- ^ 'prApproveAfterDays'
    -> PatchRule
patchRule pPatchFilterGroup_ pApproveAfterDays_ =
  PatchRule'
    { _prEnableNonSecurity = Nothing
    , _prComplianceLevel = Nothing
    , _prPatchFilterGroup = pPatchFilterGroup_
    , _prApproveAfterDays = _Nat # pApproveAfterDays_
    }


-- | For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
prEnableNonSecurity :: Lens' PatchRule (Maybe Bool)
prEnableNonSecurity = lens _prEnableNonSecurity (\ s a -> s{_prEnableNonSecurity = a})

-- | A compliance severity level for all approved patches in a patch baseline. Valid compliance severity levels include the following: Unspecified, Critical, High, Medium, Low, and Informational.
prComplianceLevel :: Lens' PatchRule (Maybe PatchComplianceLevel)
prComplianceLevel = lens _prComplianceLevel (\ s a -> s{_prComplianceLevel = a})

-- | The patch filter group that defines the criteria for the rule.
prPatchFilterGroup :: Lens' PatchRule PatchFilterGroup
prPatchFilterGroup = lens _prPatchFilterGroup (\ s a -> s{_prPatchFilterGroup = a})

-- | The number of days after the release date of each patch matched by the rule the patch is marked as approved in the patch baseline.
prApproveAfterDays :: Lens' PatchRule Natural
prApproveAfterDays = lens _prApproveAfterDays (\ s a -> s{_prApproveAfterDays = a}) . _Nat

instance FromJSON PatchRule where
        parseJSON
          = withObject "PatchRule"
              (\ x ->
                 PatchRule' <$>
                   (x .:? "EnableNonSecurity") <*>
                     (x .:? "ComplianceLevel")
                     <*> (x .: "PatchFilterGroup")
                     <*> (x .: "ApproveAfterDays"))

instance Hashable PatchRule where

instance NFData PatchRule where

instance ToJSON PatchRule where
        toJSON PatchRule'{..}
          = object
              (catMaybes
                 [("EnableNonSecurity" .=) <$> _prEnableNonSecurity,
                  ("ComplianceLevel" .=) <$> _prComplianceLevel,
                  Just ("PatchFilterGroup" .= _prPatchFilterGroup),
                  Just ("ApproveAfterDays" .= _prApproveAfterDays)])

-- | A set of rules defining the approval rules for a patch baseline.
--
--
--
-- /See:/ 'patchRuleGroup' smart constructor.
newtype PatchRuleGroup = PatchRuleGroup'
  { _prgPatchRules :: [PatchRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prgPatchRules' - The rules that make up the rule group.
patchRuleGroup
    :: PatchRuleGroup
patchRuleGroup = PatchRuleGroup' {_prgPatchRules = mempty}


-- | The rules that make up the rule group.
prgPatchRules :: Lens' PatchRuleGroup [PatchRule]
prgPatchRules = lens _prgPatchRules (\ s a -> s{_prgPatchRules = a}) . _Coerce

instance FromJSON PatchRuleGroup where
        parseJSON
          = withObject "PatchRuleGroup"
              (\ x ->
                 PatchRuleGroup' <$> (x .:? "PatchRules" .!= mempty))

instance Hashable PatchRuleGroup where

instance NFData PatchRuleGroup where

instance ToJSON PatchRuleGroup where
        toJSON PatchRuleGroup'{..}
          = object
              (catMaybes [Just ("PatchRules" .= _prgPatchRules)])

-- | Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.
--
--
--
-- /See:/ 'patchSource' smart constructor.
data PatchSource = PatchSource'
  { _psName          :: !Text
  , _psProducts      :: !(List1 Text)
  , _psConfiguration :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psName' - The name specified to identify the patch source.
--
-- * 'psProducts' - The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
--
-- * 'psConfiguration' - The value of the yum repo configuration. For example: @cachedir=/var/cache/yum/$basesearch@  @> releasever@  @keepcache=0@  @debualevel=2@
patchSource
    :: Text -- ^ 'psName'
    -> NonEmpty Text -- ^ 'psProducts'
    -> Text -- ^ 'psConfiguration'
    -> PatchSource
patchSource pName_ pProducts_ pConfiguration_ =
  PatchSource'
    { _psName = pName_
    , _psProducts = _List1 # pProducts_
    , _psConfiguration = _Sensitive # pConfiguration_
    }


-- | The name specified to identify the patch source.
psName :: Lens' PatchSource Text
psName = lens _psName (\ s a -> s{_psName = a})

-- | The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
psProducts :: Lens' PatchSource (NonEmpty Text)
psProducts = lens _psProducts (\ s a -> s{_psProducts = a}) . _List1

-- | The value of the yum repo configuration. For example: @cachedir=/var/cache/yum/$basesearch@  @> releasever@  @keepcache=0@  @debualevel=2@
psConfiguration :: Lens' PatchSource Text
psConfiguration = lens _psConfiguration (\ s a -> s{_psConfiguration = a}) . _Sensitive

instance FromJSON PatchSource where
        parseJSON
          = withObject "PatchSource"
              (\ x ->
                 PatchSource' <$>
                   (x .: "Name") <*> (x .: "Products") <*>
                     (x .: "Configuration"))

instance Hashable PatchSource where

instance NFData PatchSource where

instance ToJSON PatchSource where
        toJSON PatchSource'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _psName),
                  Just ("Products" .= _psProducts),
                  Just ("Configuration" .= _psConfiguration)])

-- | Information about the approval status of a patch.
--
--
--
-- /See:/ 'patchStatus' smart constructor.
data PatchStatus = PatchStatus'
  { _psApprovalDate     :: !(Maybe POSIX)
  , _psDeploymentStatus :: !(Maybe PatchDeploymentStatus)
  , _psComplianceLevel  :: !(Maybe PatchComplianceLevel)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PatchStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psApprovalDate' - The date the patch was approved (or will be approved if the status is PENDING_APPROVAL).
--
-- * 'psDeploymentStatus' - The approval status of a patch (APPROVED, PENDING_APPROVAL, EXPLICIT_APPROVED, EXPLICIT_REJECTED).
--
-- * 'psComplianceLevel' - The compliance severity level for a patch.
patchStatus
    :: PatchStatus
patchStatus =
  PatchStatus'
    { _psApprovalDate = Nothing
    , _psDeploymentStatus = Nothing
    , _psComplianceLevel = Nothing
    }


-- | The date the patch was approved (or will be approved if the status is PENDING_APPROVAL).
psApprovalDate :: Lens' PatchStatus (Maybe UTCTime)
psApprovalDate = lens _psApprovalDate (\ s a -> s{_psApprovalDate = a}) . mapping _Time

-- | The approval status of a patch (APPROVED, PENDING_APPROVAL, EXPLICIT_APPROVED, EXPLICIT_REJECTED).
psDeploymentStatus :: Lens' PatchStatus (Maybe PatchDeploymentStatus)
psDeploymentStatus = lens _psDeploymentStatus (\ s a -> s{_psDeploymentStatus = a})

-- | The compliance severity level for a patch.
psComplianceLevel :: Lens' PatchStatus (Maybe PatchComplianceLevel)
psComplianceLevel = lens _psComplianceLevel (\ s a -> s{_psComplianceLevel = a})

instance FromJSON PatchStatus where
        parseJSON
          = withObject "PatchStatus"
              (\ x ->
                 PatchStatus' <$>
                   (x .:? "ApprovalDate") <*> (x .:? "DeploymentStatus")
                     <*> (x .:? "ComplianceLevel"))

instance Hashable PatchStatus where

instance NFData PatchStatus where

-- | Information about targets that resolved during the Automation execution.
--
--
--
-- /See:/ 'resolvedTargets' smart constructor.
data ResolvedTargets = ResolvedTargets'
  { _rtTruncated       :: !(Maybe Bool)
  , _rtParameterValues :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolvedTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtTruncated' - A boolean value indicating whether the resolved target list is truncated.
--
-- * 'rtParameterValues' - A list of parameter values sent to targets that resolved during the Automation execution.
resolvedTargets
    :: ResolvedTargets
resolvedTargets =
  ResolvedTargets' {_rtTruncated = Nothing, _rtParameterValues = Nothing}


-- | A boolean value indicating whether the resolved target list is truncated.
rtTruncated :: Lens' ResolvedTargets (Maybe Bool)
rtTruncated = lens _rtTruncated (\ s a -> s{_rtTruncated = a})

-- | A list of parameter values sent to targets that resolved during the Automation execution.
rtParameterValues :: Lens' ResolvedTargets [Text]
rtParameterValues = lens _rtParameterValues (\ s a -> s{_rtParameterValues = a}) . _Default . _Coerce

instance FromJSON ResolvedTargets where
        parseJSON
          = withObject "ResolvedTargets"
              (\ x ->
                 ResolvedTargets' <$>
                   (x .:? "Truncated") <*>
                     (x .:? "ParameterValues" .!= mempty))

instance Hashable ResolvedTargets where

instance NFData ResolvedTargets where

-- | Compliance summary information for a specific resource.
--
--
--
-- /See:/ 'resourceComplianceSummaryItem' smart constructor.
data ResourceComplianceSummaryItem = ResourceComplianceSummaryItem'
  { _rcsiNonCompliantSummary :: !(Maybe NonCompliantSummary)
  , _rcsiStatus              :: !(Maybe ComplianceStatus)
  , _rcsiResourceId          :: !(Maybe Text)
  , _rcsiResourceType        :: !(Maybe Text)
  , _rcsiCompliantSummary    :: !(Maybe CompliantSummary)
  , _rcsiExecutionSummary    :: !(Maybe ComplianceExecutionSummary)
  , _rcsiOverallSeverity     :: !(Maybe ComplianceSeverity)
  , _rcsiComplianceType      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceComplianceSummaryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcsiNonCompliantSummary' - A list of items that aren't compliant for the resource.
--
-- * 'rcsiStatus' - The compliance status for the resource.
--
-- * 'rcsiResourceId' - The resource ID.
--
-- * 'rcsiResourceType' - The resource type.
--
-- * 'rcsiCompliantSummary' - A list of items that are compliant for the resource.
--
-- * 'rcsiExecutionSummary' - Information about the execution.
--
-- * 'rcsiOverallSeverity' - The highest severity item found for the resource. The resource is compliant for this item.
--
-- * 'rcsiComplianceType' - The compliance type.
resourceComplianceSummaryItem
    :: ResourceComplianceSummaryItem
resourceComplianceSummaryItem =
  ResourceComplianceSummaryItem'
    { _rcsiNonCompliantSummary = Nothing
    , _rcsiStatus = Nothing
    , _rcsiResourceId = Nothing
    , _rcsiResourceType = Nothing
    , _rcsiCompliantSummary = Nothing
    , _rcsiExecutionSummary = Nothing
    , _rcsiOverallSeverity = Nothing
    , _rcsiComplianceType = Nothing
    }


-- | A list of items that aren't compliant for the resource.
rcsiNonCompliantSummary :: Lens' ResourceComplianceSummaryItem (Maybe NonCompliantSummary)
rcsiNonCompliantSummary = lens _rcsiNonCompliantSummary (\ s a -> s{_rcsiNonCompliantSummary = a})

-- | The compliance status for the resource.
rcsiStatus :: Lens' ResourceComplianceSummaryItem (Maybe ComplianceStatus)
rcsiStatus = lens _rcsiStatus (\ s a -> s{_rcsiStatus = a})

-- | The resource ID.
rcsiResourceId :: Lens' ResourceComplianceSummaryItem (Maybe Text)
rcsiResourceId = lens _rcsiResourceId (\ s a -> s{_rcsiResourceId = a})

-- | The resource type.
rcsiResourceType :: Lens' ResourceComplianceSummaryItem (Maybe Text)
rcsiResourceType = lens _rcsiResourceType (\ s a -> s{_rcsiResourceType = a})

-- | A list of items that are compliant for the resource.
rcsiCompliantSummary :: Lens' ResourceComplianceSummaryItem (Maybe CompliantSummary)
rcsiCompliantSummary = lens _rcsiCompliantSummary (\ s a -> s{_rcsiCompliantSummary = a})

-- | Information about the execution.
rcsiExecutionSummary :: Lens' ResourceComplianceSummaryItem (Maybe ComplianceExecutionSummary)
rcsiExecutionSummary = lens _rcsiExecutionSummary (\ s a -> s{_rcsiExecutionSummary = a})

-- | The highest severity item found for the resource. The resource is compliant for this item.
rcsiOverallSeverity :: Lens' ResourceComplianceSummaryItem (Maybe ComplianceSeverity)
rcsiOverallSeverity = lens _rcsiOverallSeverity (\ s a -> s{_rcsiOverallSeverity = a})

-- | The compliance type.
rcsiComplianceType :: Lens' ResourceComplianceSummaryItem (Maybe Text)
rcsiComplianceType = lens _rcsiComplianceType (\ s a -> s{_rcsiComplianceType = a})

instance FromJSON ResourceComplianceSummaryItem where
        parseJSON
          = withObject "ResourceComplianceSummaryItem"
              (\ x ->
                 ResourceComplianceSummaryItem' <$>
                   (x .:? "NonCompliantSummary") <*> (x .:? "Status")
                     <*> (x .:? "ResourceId")
                     <*> (x .:? "ResourceType")
                     <*> (x .:? "CompliantSummary")
                     <*> (x .:? "ExecutionSummary")
                     <*> (x .:? "OverallSeverity")
                     <*> (x .:? "ComplianceType"))

instance Hashable ResourceComplianceSummaryItem where

instance NFData ResourceComplianceSummaryItem where

-- | Information about a Resource Data Sync configuration, including its current status and last successful sync.
--
--
--
-- /See:/ 'resourceDataSyncItem' smart constructor.
data ResourceDataSyncItem = ResourceDataSyncItem'
  { _rdsiLastSyncStatusMessage  :: !(Maybe Text)
  , _rdsiSyncCreatedTime        :: !(Maybe POSIX)
  , _rdsiLastSyncTime           :: !(Maybe POSIX)
  , _rdsiSyncName               :: !(Maybe Text)
  , _rdsiLastStatus             :: !(Maybe LastResourceDataSyncStatus)
  , _rdsiS3Destination          :: !(Maybe ResourceDataSyncS3Destination)
  , _rdsiLastSuccessfulSyncTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDataSyncItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsiLastSyncStatusMessage' - The status message details reported by the last sync.
--
-- * 'rdsiSyncCreatedTime' - The date and time the configuration was created (UTC).
--
-- * 'rdsiLastSyncTime' - The last time the configuration attempted to sync (UTC).
--
-- * 'rdsiSyncName' - The name of the Resource Data Sync.
--
-- * 'rdsiLastStatus' - The status reported by the last sync.
--
-- * 'rdsiS3Destination' - Configuration information for the target Amazon S3 bucket.
--
-- * 'rdsiLastSuccessfulSyncTime' - The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
resourceDataSyncItem
    :: ResourceDataSyncItem
resourceDataSyncItem =
  ResourceDataSyncItem'
    { _rdsiLastSyncStatusMessage = Nothing
    , _rdsiSyncCreatedTime = Nothing
    , _rdsiLastSyncTime = Nothing
    , _rdsiSyncName = Nothing
    , _rdsiLastStatus = Nothing
    , _rdsiS3Destination = Nothing
    , _rdsiLastSuccessfulSyncTime = Nothing
    }


-- | The status message details reported by the last sync.
rdsiLastSyncStatusMessage :: Lens' ResourceDataSyncItem (Maybe Text)
rdsiLastSyncStatusMessage = lens _rdsiLastSyncStatusMessage (\ s a -> s{_rdsiLastSyncStatusMessage = a})

-- | The date and time the configuration was created (UTC).
rdsiSyncCreatedTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiSyncCreatedTime = lens _rdsiSyncCreatedTime (\ s a -> s{_rdsiSyncCreatedTime = a}) . mapping _Time

-- | The last time the configuration attempted to sync (UTC).
rdsiLastSyncTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiLastSyncTime = lens _rdsiLastSyncTime (\ s a -> s{_rdsiLastSyncTime = a}) . mapping _Time

-- | The name of the Resource Data Sync.
rdsiSyncName :: Lens' ResourceDataSyncItem (Maybe Text)
rdsiSyncName = lens _rdsiSyncName (\ s a -> s{_rdsiSyncName = a})

-- | The status reported by the last sync.
rdsiLastStatus :: Lens' ResourceDataSyncItem (Maybe LastResourceDataSyncStatus)
rdsiLastStatus = lens _rdsiLastStatus (\ s a -> s{_rdsiLastStatus = a})

-- | Configuration information for the target Amazon S3 bucket.
rdsiS3Destination :: Lens' ResourceDataSyncItem (Maybe ResourceDataSyncS3Destination)
rdsiS3Destination = lens _rdsiS3Destination (\ s a -> s{_rdsiS3Destination = a})

-- | The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
rdsiLastSuccessfulSyncTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiLastSuccessfulSyncTime = lens _rdsiLastSuccessfulSyncTime (\ s a -> s{_rdsiLastSuccessfulSyncTime = a}) . mapping _Time

instance FromJSON ResourceDataSyncItem where
        parseJSON
          = withObject "ResourceDataSyncItem"
              (\ x ->
                 ResourceDataSyncItem' <$>
                   (x .:? "LastSyncStatusMessage") <*>
                     (x .:? "SyncCreatedTime")
                     <*> (x .:? "LastSyncTime")
                     <*> (x .:? "SyncName")
                     <*> (x .:? "LastStatus")
                     <*> (x .:? "S3Destination")
                     <*> (x .:? "LastSuccessfulSyncTime"))

instance Hashable ResourceDataSyncItem where

instance NFData ResourceDataSyncItem where

-- | Information about the target Amazon S3 bucket for the Resource Data Sync.
--
--
--
-- /See:/ 'resourceDataSyncS3Destination' smart constructor.
data ResourceDataSyncS3Destination = ResourceDataSyncS3Destination'
  { _rdssdPrefix       :: !(Maybe Text)
  , _rdssdAWSKMSKeyARN :: !(Maybe Text)
  , _rdssdBucketName   :: !Text
  , _rdssdSyncFormat   :: !ResourceDataSyncS3Format
  , _rdssdRegion       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDataSyncS3Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdssdPrefix' - An Amazon S3 prefix for the bucket.
--
-- * 'rdssdAWSKMSKeyARN' - The ARN of an encryption key for a destination in Amazon S3. Must belong to the same region as the destination Amazon S3 bucket.
--
-- * 'rdssdBucketName' - The name of the Amazon S3 bucket where the aggregated data is stored.
--
-- * 'rdssdSyncFormat' - A supported sync format. The following format is currently supported: JsonSerDe
--
-- * 'rdssdRegion' - The AWS Region with the Amazon S3 bucket targeted by the Resource Data Sync.
resourceDataSyncS3Destination
    :: Text -- ^ 'rdssdBucketName'
    -> ResourceDataSyncS3Format -- ^ 'rdssdSyncFormat'
    -> Text -- ^ 'rdssdRegion'
    -> ResourceDataSyncS3Destination
resourceDataSyncS3Destination pBucketName_ pSyncFormat_ pRegion_ =
  ResourceDataSyncS3Destination'
    { _rdssdPrefix = Nothing
    , _rdssdAWSKMSKeyARN = Nothing
    , _rdssdBucketName = pBucketName_
    , _rdssdSyncFormat = pSyncFormat_
    , _rdssdRegion = pRegion_
    }


-- | An Amazon S3 prefix for the bucket.
rdssdPrefix :: Lens' ResourceDataSyncS3Destination (Maybe Text)
rdssdPrefix = lens _rdssdPrefix (\ s a -> s{_rdssdPrefix = a})

-- | The ARN of an encryption key for a destination in Amazon S3. Must belong to the same region as the destination Amazon S3 bucket.
rdssdAWSKMSKeyARN :: Lens' ResourceDataSyncS3Destination (Maybe Text)
rdssdAWSKMSKeyARN = lens _rdssdAWSKMSKeyARN (\ s a -> s{_rdssdAWSKMSKeyARN = a})

-- | The name of the Amazon S3 bucket where the aggregated data is stored.
rdssdBucketName :: Lens' ResourceDataSyncS3Destination Text
rdssdBucketName = lens _rdssdBucketName (\ s a -> s{_rdssdBucketName = a})

-- | A supported sync format. The following format is currently supported: JsonSerDe
rdssdSyncFormat :: Lens' ResourceDataSyncS3Destination ResourceDataSyncS3Format
rdssdSyncFormat = lens _rdssdSyncFormat (\ s a -> s{_rdssdSyncFormat = a})

-- | The AWS Region with the Amazon S3 bucket targeted by the Resource Data Sync.
rdssdRegion :: Lens' ResourceDataSyncS3Destination Text
rdssdRegion = lens _rdssdRegion (\ s a -> s{_rdssdRegion = a})

instance FromJSON ResourceDataSyncS3Destination where
        parseJSON
          = withObject "ResourceDataSyncS3Destination"
              (\ x ->
                 ResourceDataSyncS3Destination' <$>
                   (x .:? "Prefix") <*> (x .:? "AWSKMSKeyARN") <*>
                     (x .: "BucketName")
                     <*> (x .: "SyncFormat")
                     <*> (x .: "Region"))

instance Hashable ResourceDataSyncS3Destination where

instance NFData ResourceDataSyncS3Destination where

instance ToJSON ResourceDataSyncS3Destination where
        toJSON ResourceDataSyncS3Destination'{..}
          = object
              (catMaybes
                 [("Prefix" .=) <$> _rdssdPrefix,
                  ("AWSKMSKeyARN" .=) <$> _rdssdAWSKMSKeyARN,
                  Just ("BucketName" .= _rdssdBucketName),
                  Just ("SyncFormat" .= _rdssdSyncFormat),
                  Just ("Region" .= _rdssdRegion)])

-- | The inventory item result attribute.
--
--
--
-- /See:/ 'resultAttribute' smart constructor.
newtype ResultAttribute = ResultAttribute'
  { _raTypeName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raTypeName' - Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
resultAttribute
    :: Text -- ^ 'raTypeName'
    -> ResultAttribute
resultAttribute pTypeName_ = ResultAttribute' {_raTypeName = pTypeName_}


-- | Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
raTypeName :: Lens' ResultAttribute Text
raTypeName = lens _raTypeName (\ s a -> s{_raTypeName = a})

instance Hashable ResultAttribute where

instance NFData ResultAttribute where

instance ToJSON ResultAttribute where
        toJSON ResultAttribute'{..}
          = object
              (catMaybes [Just ("TypeName" .= _raTypeName)])

-- | An Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 's3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { _solOutputS3KeyPrefix  :: !(Maybe Text)
  , _solOutputS3Region     :: !(Maybe Text)
  , _solOutputS3BucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'solOutputS3KeyPrefix' - The Amazon S3 bucket subfolder.
--
-- * 'solOutputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
--
-- * 'solOutputS3BucketName' - The name of the Amazon S3 bucket.
s3OutputLocation
    :: S3OutputLocation
s3OutputLocation =
  S3OutputLocation'
    { _solOutputS3KeyPrefix = Nothing
    , _solOutputS3Region = Nothing
    , _solOutputS3BucketName = Nothing
    }


-- | The Amazon S3 bucket subfolder.
solOutputS3KeyPrefix :: Lens' S3OutputLocation (Maybe Text)
solOutputS3KeyPrefix = lens _solOutputS3KeyPrefix (\ s a -> s{_solOutputS3KeyPrefix = a})

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
solOutputS3Region :: Lens' S3OutputLocation (Maybe Text)
solOutputS3Region = lens _solOutputS3Region (\ s a -> s{_solOutputS3Region = a})

-- | The name of the Amazon S3 bucket.
solOutputS3BucketName :: Lens' S3OutputLocation (Maybe Text)
solOutputS3BucketName = lens _solOutputS3BucketName (\ s a -> s{_solOutputS3BucketName = a})

instance FromJSON S3OutputLocation where
        parseJSON
          = withObject "S3OutputLocation"
              (\ x ->
                 S3OutputLocation' <$>
                   (x .:? "OutputS3KeyPrefix") <*>
                     (x .:? "OutputS3Region")
                     <*> (x .:? "OutputS3BucketName"))

instance Hashable S3OutputLocation where

instance NFData S3OutputLocation where

instance ToJSON S3OutputLocation where
        toJSON S3OutputLocation'{..}
          = object
              (catMaybes
                 [("OutputS3KeyPrefix" .=) <$> _solOutputS3KeyPrefix,
                  ("OutputS3Region" .=) <$> _solOutputS3Region,
                  ("OutputS3BucketName" .=) <$>
                    _solOutputS3BucketName])

-- | A URL for the Amazon S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 's3OutputURL' smart constructor.
newtype S3OutputURL = S3OutputURL'
  { _souOutputURL :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3OutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'souOutputURL' - A URL for an Amazon S3 bucket where you want to store the results of this request.
s3OutputURL
    :: S3OutputURL
s3OutputURL = S3OutputURL' {_souOutputURL = Nothing}


-- | A URL for an Amazon S3 bucket where you want to store the results of this request.
souOutputURL :: Lens' S3OutputURL (Maybe Text)
souOutputURL = lens _souOutputURL (\ s a -> s{_souOutputURL = a})

instance FromJSON S3OutputURL where
        parseJSON
          = withObject "S3OutputURL"
              (\ x -> S3OutputURL' <$> (x .:? "OutputUrl"))

instance Hashable S3OutputURL where

instance NFData S3OutputURL where

-- | The number of managed instances found for each patch severity level defined in the request filter.
--
--
--
-- /See:/ 'severitySummary' smart constructor.
data SeveritySummary = SeveritySummary'
  { _ssLowCount           :: !(Maybe Int)
  , _ssUnspecifiedCount   :: !(Maybe Int)
  , _ssHighCount          :: !(Maybe Int)
  , _ssMediumCount        :: !(Maybe Int)
  , _ssInformationalCount :: !(Maybe Int)
  , _ssCriticalCount      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SeveritySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssLowCount' - The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
--
-- * 'ssUnspecifiedCount' - The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
--
-- * 'ssHighCount' - The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
--
-- * 'ssMediumCount' - The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
--
-- * 'ssInformationalCount' - The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
--
-- * 'ssCriticalCount' - The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
severitySummary
    :: SeveritySummary
severitySummary =
  SeveritySummary'
    { _ssLowCount = Nothing
    , _ssUnspecifiedCount = Nothing
    , _ssHighCount = Nothing
    , _ssMediumCount = Nothing
    , _ssInformationalCount = Nothing
    , _ssCriticalCount = Nothing
    }


-- | The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
ssLowCount :: Lens' SeveritySummary (Maybe Int)
ssLowCount = lens _ssLowCount (\ s a -> s{_ssLowCount = a})

-- | The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
ssUnspecifiedCount :: Lens' SeveritySummary (Maybe Int)
ssUnspecifiedCount = lens _ssUnspecifiedCount (\ s a -> s{_ssUnspecifiedCount = a})

-- | The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
ssHighCount :: Lens' SeveritySummary (Maybe Int)
ssHighCount = lens _ssHighCount (\ s a -> s{_ssHighCount = a})

-- | The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
ssMediumCount :: Lens' SeveritySummary (Maybe Int)
ssMediumCount = lens _ssMediumCount (\ s a -> s{_ssMediumCount = a})

-- | The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
ssInformationalCount :: Lens' SeveritySummary (Maybe Int)
ssInformationalCount = lens _ssInformationalCount (\ s a -> s{_ssInformationalCount = a})

-- | The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
ssCriticalCount :: Lens' SeveritySummary (Maybe Int)
ssCriticalCount = lens _ssCriticalCount (\ s a -> s{_ssCriticalCount = a})

instance FromJSON SeveritySummary where
        parseJSON
          = withObject "SeveritySummary"
              (\ x ->
                 SeveritySummary' <$>
                   (x .:? "LowCount") <*> (x .:? "UnspecifiedCount") <*>
                     (x .:? "HighCount")
                     <*> (x .:? "MediumCount")
                     <*> (x .:? "InformationalCount")
                     <*> (x .:? "CriticalCount"))

instance Hashable SeveritySummary where

instance NFData SeveritySummary where

-- | Detailed information about an the execution state of an Automation step.
--
--
--
-- /See:/ 'stepExecution' smart constructor.
data StepExecution = StepExecution'
  { _seFailureDetails       :: !(Maybe FailureDetails)
  , _seInputs               :: !(Maybe (Map Text Text))
  , _seStepName             :: !(Maybe Text)
  , _seExecutionEndTime     :: !(Maybe POSIX)
  , _seFailureMessage       :: !(Maybe Text)
  , _seResponse             :: !(Maybe Text)
  , _seAction               :: !(Maybe Text)
  , _seResponseCode         :: !(Maybe Text)
  , _seStepStatus           :: !(Maybe AutomationExecutionStatus)
  , _seOverriddenParameters :: !(Maybe (Map Text [Text]))
  , _seOutputs              :: !(Maybe (Map Text [Text]))
  , _seExecutionStartTime   :: !(Maybe POSIX)
  , _seMaxAttempts          :: !(Maybe Int)
  , _seStepExecutionId      :: !(Maybe Text)
  , _seTimeoutSeconds       :: !(Maybe Integer)
  , _seOnFailure            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StepExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seFailureDetails' - Information about the Automation failure.
--
-- * 'seInputs' - Fully-resolved values passed into the step before execution.
--
-- * 'seStepName' - The name of this execution step.
--
-- * 'seExecutionEndTime' - If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
--
-- * 'seFailureMessage' - If a step failed, this message explains why the execution failed.
--
-- * 'seResponse' - A message associated with the response code for an execution.
--
-- * 'seAction' - The action this step performs. The action determines the behavior of the step.
--
-- * 'seResponseCode' - The response code returned by the execution of the step.
--
-- * 'seStepStatus' - The execution status for this step. Valid values include: Pending, InProgress, Success, Cancelled, Failed, and TimedOut.
--
-- * 'seOverriddenParameters' - A user-specified list of parameters to override when executing a step.
--
-- * 'seOutputs' - Returned values from the execution of the step.
--
-- * 'seExecutionStartTime' - If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
--
-- * 'seMaxAttempts' - The maximum number of tries to run the action of the step. The default value is 1.
--
-- * 'seStepExecutionId' - The unique ID of a step execution.
--
-- * 'seTimeoutSeconds' - The timeout seconds of the step.
--
-- * 'seOnFailure' - The action to take if the step fails. The default value is Abort.
stepExecution
    :: StepExecution
stepExecution =
  StepExecution'
    { _seFailureDetails = Nothing
    , _seInputs = Nothing
    , _seStepName = Nothing
    , _seExecutionEndTime = Nothing
    , _seFailureMessage = Nothing
    , _seResponse = Nothing
    , _seAction = Nothing
    , _seResponseCode = Nothing
    , _seStepStatus = Nothing
    , _seOverriddenParameters = Nothing
    , _seOutputs = Nothing
    , _seExecutionStartTime = Nothing
    , _seMaxAttempts = Nothing
    , _seStepExecutionId = Nothing
    , _seTimeoutSeconds = Nothing
    , _seOnFailure = Nothing
    }


-- | Information about the Automation failure.
seFailureDetails :: Lens' StepExecution (Maybe FailureDetails)
seFailureDetails = lens _seFailureDetails (\ s a -> s{_seFailureDetails = a})

-- | Fully-resolved values passed into the step before execution.
seInputs :: Lens' StepExecution (HashMap Text Text)
seInputs = lens _seInputs (\ s a -> s{_seInputs = a}) . _Default . _Map

-- | The name of this execution step.
seStepName :: Lens' StepExecution (Maybe Text)
seStepName = lens _seStepName (\ s a -> s{_seStepName = a})

-- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
seExecutionEndTime :: Lens' StepExecution (Maybe UTCTime)
seExecutionEndTime = lens _seExecutionEndTime (\ s a -> s{_seExecutionEndTime = a}) . mapping _Time

-- | If a step failed, this message explains why the execution failed.
seFailureMessage :: Lens' StepExecution (Maybe Text)
seFailureMessage = lens _seFailureMessage (\ s a -> s{_seFailureMessage = a})

-- | A message associated with the response code for an execution.
seResponse :: Lens' StepExecution (Maybe Text)
seResponse = lens _seResponse (\ s a -> s{_seResponse = a})

-- | The action this step performs. The action determines the behavior of the step.
seAction :: Lens' StepExecution (Maybe Text)
seAction = lens _seAction (\ s a -> s{_seAction = a})

-- | The response code returned by the execution of the step.
seResponseCode :: Lens' StepExecution (Maybe Text)
seResponseCode = lens _seResponseCode (\ s a -> s{_seResponseCode = a})

-- | The execution status for this step. Valid values include: Pending, InProgress, Success, Cancelled, Failed, and TimedOut.
seStepStatus :: Lens' StepExecution (Maybe AutomationExecutionStatus)
seStepStatus = lens _seStepStatus (\ s a -> s{_seStepStatus = a})

-- | A user-specified list of parameters to override when executing a step.
seOverriddenParameters :: Lens' StepExecution (HashMap Text [Text])
seOverriddenParameters = lens _seOverriddenParameters (\ s a -> s{_seOverriddenParameters = a}) . _Default . _Map

-- | Returned values from the execution of the step.
seOutputs :: Lens' StepExecution (HashMap Text [Text])
seOutputs = lens _seOutputs (\ s a -> s{_seOutputs = a}) . _Default . _Map

-- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
seExecutionStartTime :: Lens' StepExecution (Maybe UTCTime)
seExecutionStartTime = lens _seExecutionStartTime (\ s a -> s{_seExecutionStartTime = a}) . mapping _Time

-- | The maximum number of tries to run the action of the step. The default value is 1.
seMaxAttempts :: Lens' StepExecution (Maybe Int)
seMaxAttempts = lens _seMaxAttempts (\ s a -> s{_seMaxAttempts = a})

-- | The unique ID of a step execution.
seStepExecutionId :: Lens' StepExecution (Maybe Text)
seStepExecutionId = lens _seStepExecutionId (\ s a -> s{_seStepExecutionId = a})

-- | The timeout seconds of the step.
seTimeoutSeconds :: Lens' StepExecution (Maybe Integer)
seTimeoutSeconds = lens _seTimeoutSeconds (\ s a -> s{_seTimeoutSeconds = a})

-- | The action to take if the step fails. The default value is Abort.
seOnFailure :: Lens' StepExecution (Maybe Text)
seOnFailure = lens _seOnFailure (\ s a -> s{_seOnFailure = a})

instance FromJSON StepExecution where
        parseJSON
          = withObject "StepExecution"
              (\ x ->
                 StepExecution' <$>
                   (x .:? "FailureDetails") <*>
                     (x .:? "Inputs" .!= mempty)
                     <*> (x .:? "StepName")
                     <*> (x .:? "ExecutionEndTime")
                     <*> (x .:? "FailureMessage")
                     <*> (x .:? "Response")
                     <*> (x .:? "Action")
                     <*> (x .:? "ResponseCode")
                     <*> (x .:? "StepStatus")
                     <*> (x .:? "OverriddenParameters" .!= mempty)
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "ExecutionStartTime")
                     <*> (x .:? "MaxAttempts")
                     <*> (x .:? "StepExecutionId")
                     <*> (x .:? "TimeoutSeconds")
                     <*> (x .:? "OnFailure"))

instance Hashable StepExecution where

instance NFData StepExecution where

-- | A filter to limit the amount of step execution information returned by the call.
--
--
--
-- /See:/ 'stepExecutionFilter' smart constructor.
data StepExecutionFilter = StepExecutionFilter'
  { _sefKey    :: !StepExecutionFilterKey
  , _sefValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StepExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sefKey' - One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
--
-- * 'sefValues' - The values of the filter key.
stepExecutionFilter
    :: StepExecutionFilterKey -- ^ 'sefKey'
    -> NonEmpty Text -- ^ 'sefValues'
    -> StepExecutionFilter
stepExecutionFilter pKey_ pValues_ =
  StepExecutionFilter' {_sefKey = pKey_, _sefValues = _List1 # pValues_}


-- | One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
sefKey :: Lens' StepExecutionFilter StepExecutionFilterKey
sefKey = lens _sefKey (\ s a -> s{_sefKey = a})

-- | The values of the filter key.
sefValues :: Lens' StepExecutionFilter (NonEmpty Text)
sefValues = lens _sefValues (\ s a -> s{_sefValues = a}) . _List1

instance Hashable StepExecutionFilter where

instance NFData StepExecutionFilter where

instance ToJSON StepExecutionFilter where
        toJSON StepExecutionFilter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _sefKey),
                  Just ("Values" .= _sefValues)])

-- | Metadata that you assign to your AWS resources. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. In Systems Manager, you can apply tags to documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The name of the tag.
--
-- * 'tagValue' - The value of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The name of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | An array of search criteria that targets instances using a Key,Value combination that you specify. @Targets@ is required if you don't provide one or more instance IDs in the call.
--
--
--
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
  { _tValues :: !(Maybe [Text])
  , _tKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tValues' - User-defined criteria that maps to Key. For example, if you specified tag:ServerRole, you could specify value:WebServer to execute a command on instances that include Amazon EC2 tags of ServerRole,WebServer. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
--
-- * 'tKey' - User-defined criteria for sending commands that target instances that meet the criteria. Key can be tag:<Amazon EC2 tag> or InstanceIds. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
target
    :: Target
target = Target' {_tValues = Nothing, _tKey = Nothing}


-- | User-defined criteria that maps to Key. For example, if you specified tag:ServerRole, you could specify value:WebServer to execute a command on instances that include Amazon EC2 tags of ServerRole,WebServer. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
tValues :: Lens' Target [Text]
tValues = lens _tValues (\ s a -> s{_tValues = a}) . _Default . _Coerce

-- | User-defined criteria for sending commands that target instances that meet the criteria. Key can be tag:<Amazon EC2 tag> or InstanceIds. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
tKey :: Lens' Target (Maybe Text)
tKey = lens _tKey (\ s a -> s{_tKey = a})

instance FromJSON Target where
        parseJSON
          = withObject "Target"
              (\ x ->
                 Target' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable Target where

instance NFData Target where

instance ToJSON Target where
        toJSON Target'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _tValues, ("Key" .=) <$> _tKey])
