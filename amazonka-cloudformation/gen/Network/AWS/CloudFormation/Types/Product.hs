{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Product where

import           Network.AWS.CloudFormation.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | The AccountLimit data type.
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit = AccountLimit'
    { _alValue :: !(Maybe Int)
    , _alName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alValue'
--
-- * 'alName'
accountLimit
    :: AccountLimit
accountLimit =
    AccountLimit'
    { _alValue = Nothing
    , _alName = Nothing
    }

-- | The value that is associated with the account limit name.
alValue :: Lens' AccountLimit (Maybe Int)
alValue = lens _alValue (\ s a -> s{_alValue = a});

-- | The name of the account limit. Currently, the only account limit is
-- 'StackLimit'.
alName :: Lens' AccountLimit (Maybe Text)
alName = lens _alName (\ s a -> s{_alName = a});

instance FromXML AccountLimit where
        parseXML x
          = AccountLimit' <$>
              (x .@? "Value") <*> (x .@? "Name")

instance Hashable AccountLimit

-- | The Output data type.
--
-- /See:/ 'output' smart constructor.
data Output = Output'
    { _oOutputValue :: !(Maybe Text)
    , _oOutputKey   :: !(Maybe Text)
    , _oDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOutputValue'
--
-- * 'oOutputKey'
--
-- * 'oDescription'
output
    :: Output
output =
    Output'
    { _oOutputValue = Nothing
    , _oOutputKey = Nothing
    , _oDescription = Nothing
    }

-- | The value associated with the output.
oOutputValue :: Lens' Output (Maybe Text)
oOutputValue = lens _oOutputValue (\ s a -> s{_oOutputValue = a});

-- | The key associated with the output.
oOutputKey :: Lens' Output (Maybe Text)
oOutputKey = lens _oOutputKey (\ s a -> s{_oOutputKey = a});

-- | User defined description associated with the output.
oDescription :: Lens' Output (Maybe Text)
oDescription = lens _oDescription (\ s a -> s{_oDescription = a});

instance FromXML Output where
        parseXML x
          = Output' <$>
              (x .@? "OutputValue") <*> (x .@? "OutputKey") <*>
                (x .@? "Description")

instance Hashable Output

-- | The Parameter data type.
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
    { _pParameterValue   :: !(Maybe Text)
    , _pParameterKey     :: !(Maybe Text)
    , _pUsePreviousValue :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue'
--
-- * 'pParameterKey'
--
-- * 'pUsePreviousValue'
parameter
    :: Parameter
parameter =
    Parameter'
    { _pParameterValue = Nothing
    , _pParameterKey = Nothing
    , _pUsePreviousValue = Nothing
    }

-- | The value associated with the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a});

-- | The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
pParameterKey :: Lens' Parameter (Maybe Text)
pParameterKey = lens _pParameterKey (\ s a -> s{_pParameterKey = a});

-- | During a stack update, use the existing parameter value that the stack
-- is using for a given parameter key. If you specify 'true', do not
-- specify a parameter value.
pUsePreviousValue :: Lens' Parameter (Maybe Bool)
pUsePreviousValue = lens _pUsePreviousValue (\ s a -> s{_pUsePreviousValue = a});

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ParameterValue") <*> (x .@? "ParameterKey")
                <*> (x .@? "UsePreviousValue")

instance Hashable Parameter

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ParameterValue" =: _pParameterValue,
               "ParameterKey" =: _pParameterKey,
               "UsePreviousValue" =: _pUsePreviousValue]

-- | A set of criteria that AWS CloudFormation uses to validate parameter
-- values. Although other constraints might be defined in the stack
-- template, AWS CloudFormation returns only the 'AllowedValues' property.
--
-- /See:/ 'parameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
    { _pcAllowedValues :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ParameterConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcAllowedValues'
parameterConstraints
    :: ParameterConstraints
parameterConstraints =
    ParameterConstraints'
    { _pcAllowedValues = Nothing
    }

-- | A list of values that are permitted for a parameter.
pcAllowedValues :: Lens' ParameterConstraints [Text]
pcAllowedValues = lens _pcAllowedValues (\ s a -> s{_pcAllowedValues = a}) . _Default . _Coerce;

instance FromXML ParameterConstraints where
        parseXML x
          = ParameterConstraints' <$>
              (x .@? "AllowedValues" .!@ mempty >>=
                 may (parseXMLList "member"))

instance Hashable ParameterConstraints

-- | The ParameterDeclaration data type.
--
-- /See:/ 'parameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
    { _pdParameterKey         :: !(Maybe Text)
    , _pdParameterType        :: !(Maybe Text)
    , _pdParameterConstraints :: !(Maybe ParameterConstraints)
    , _pdDefaultValue         :: !(Maybe Text)
    , _pdNoEcho               :: !(Maybe Bool)
    , _pdDescription          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ParameterDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdParameterKey'
--
-- * 'pdParameterType'
--
-- * 'pdParameterConstraints'
--
-- * 'pdDefaultValue'
--
-- * 'pdNoEcho'
--
-- * 'pdDescription'
parameterDeclaration
    :: ParameterDeclaration
parameterDeclaration =
    ParameterDeclaration'
    { _pdParameterKey = Nothing
    , _pdParameterType = Nothing
    , _pdParameterConstraints = Nothing
    , _pdDefaultValue = Nothing
    , _pdNoEcho = Nothing
    , _pdDescription = Nothing
    }

-- | The name that is associated with the parameter.
pdParameterKey :: Lens' ParameterDeclaration (Maybe Text)
pdParameterKey = lens _pdParameterKey (\ s a -> s{_pdParameterKey = a});

-- | The type of parameter.
pdParameterType :: Lens' ParameterDeclaration (Maybe Text)
pdParameterType = lens _pdParameterType (\ s a -> s{_pdParameterType = a});

-- | The criteria that AWS CloudFormation uses to validate parameter values.
pdParameterConstraints :: Lens' ParameterDeclaration (Maybe ParameterConstraints)
pdParameterConstraints = lens _pdParameterConstraints (\ s a -> s{_pdParameterConstraints = a});

-- | The default value of the parameter.
pdDefaultValue :: Lens' ParameterDeclaration (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\ s a -> s{_pdDefaultValue = a});

-- | Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the AWS Management Console.
pdNoEcho :: Lens' ParameterDeclaration (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\ s a -> s{_pdNoEcho = a});

-- | The description that is associate with the parameter.
pdDescription :: Lens' ParameterDeclaration (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a});

instance FromXML ParameterDeclaration where
        parseXML x
          = ParameterDeclaration' <$>
              (x .@? "ParameterKey") <*> (x .@? "ParameterType")
                <*> (x .@? "ParameterConstraints")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "NoEcho")
                <*> (x .@? "Description")

instance Hashable ParameterDeclaration

-- | The Stack data type.
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
    { _sDisableRollback   :: !(Maybe Bool)
    , _sLastUpdatedTime   :: !(Maybe ISO8601)
    , _sNotificationARNs  :: !(Maybe [Text])
    , _sStackStatusReason :: !(Maybe Text)
    , _sOutputs           :: !(Maybe [Output])
    , _sParameters        :: !(Maybe [Parameter])
    , _sStackId           :: !(Maybe Text)
    , _sDescription       :: !(Maybe Text)
    , _sCapabilities      :: !(Maybe [Capability])
    , _sTags              :: !(Maybe [Tag])
    , _sTimeoutInMinutes  :: !(Maybe Nat)
    , _sStackName         :: !Text
    , _sCreationTime      :: !ISO8601
    , _sStackStatus       :: !StackStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDisableRollback'
--
-- * 'sLastUpdatedTime'
--
-- * 'sNotificationARNs'
--
-- * 'sStackStatusReason'
--
-- * 'sOutputs'
--
-- * 'sParameters'
--
-- * 'sStackId'
--
-- * 'sDescription'
--
-- * 'sCapabilities'
--
-- * 'sTags'
--
-- * 'sTimeoutInMinutes'
--
-- * 'sStackName'
--
-- * 'sCreationTime'
--
-- * 'sStackStatus'
stack
    :: Text -- ^ 'sStackName'
    -> UTCTime -- ^ 'sCreationTime'
    -> StackStatus -- ^ 'sStackStatus'
    -> Stack
stack pStackName_ pCreationTime_ pStackStatus_ =
    Stack'
    { _sDisableRollback = Nothing
    , _sLastUpdatedTime = Nothing
    , _sNotificationARNs = Nothing
    , _sStackStatusReason = Nothing
    , _sOutputs = Nothing
    , _sParameters = Nothing
    , _sStackId = Nothing
    , _sDescription = Nothing
    , _sCapabilities = Nothing
    , _sTags = Nothing
    , _sTimeoutInMinutes = Nothing
    , _sStackName = pStackName_
    , _sCreationTime = _Time # pCreationTime_
    , _sStackStatus = pStackStatus_
    }

-- | Boolean to enable or disable rollback on stack creation failures:
--
-- -   'true': disable rollback
-- -   'false': enable rollback
sDisableRollback :: Lens' Stack (Maybe Bool)
sDisableRollback = lens _sDisableRollback (\ s a -> s{_sDisableRollback = a});

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
sLastUpdatedTime :: Lens' Stack (Maybe UTCTime)
sLastUpdatedTime = lens _sLastUpdatedTime (\ s a -> s{_sLastUpdatedTime = a}) . mapping _Time;

-- | SNS topic ARNs to which stack related events are published.
sNotificationARNs :: Lens' Stack [Text]
sNotificationARNs = lens _sNotificationARNs (\ s a -> s{_sNotificationARNs = a}) . _Default . _Coerce;

-- | Success\/failure message associated with the stack status.
sStackStatusReason :: Lens' Stack (Maybe Text)
sStackStatusReason = lens _sStackStatusReason (\ s a -> s{_sStackStatusReason = a});

-- | A list of output structures.
sOutputs :: Lens' Stack [Output]
sOutputs = lens _sOutputs (\ s a -> s{_sOutputs = a}) . _Default . _Coerce;

-- | A list of 'Parameter' structures.
sParameters :: Lens' Stack [Parameter]
sParameters = lens _sParameters (\ s a -> s{_sParameters = a}) . _Default . _Coerce;

-- | Unique identifier of the stack.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\ s a -> s{_sStackId = a});

-- | A user-defined description associated with the stack.
sDescription :: Lens' Stack (Maybe Text)
sDescription = lens _sDescription (\ s a -> s{_sDescription = a});

-- | The capabilities allowed in the stack.
sCapabilities :: Lens' Stack [Capability]
sCapabilities = lens _sCapabilities (\ s a -> s{_sCapabilities = a}) . _Default . _Coerce;

-- | A list of 'Tag's that specify cost allocation information for the stack.
sTags :: Lens' Stack [Tag]
sTags = lens _sTags (\ s a -> s{_sTags = a}) . _Default . _Coerce;

-- | The amount of time within which stack creation should complete.
sTimeoutInMinutes :: Lens' Stack (Maybe Natural)
sTimeoutInMinutes = lens _sTimeoutInMinutes (\ s a -> s{_sTimeoutInMinutes = a}) . mapping _Nat;

-- | The name associated with the stack.
sStackName :: Lens' Stack Text
sStackName = lens _sStackName (\ s a -> s{_sStackName = a});

-- | The time at which the stack was created.
sCreationTime :: Lens' Stack UTCTime
sCreationTime = lens _sCreationTime (\ s a -> s{_sCreationTime = a}) . _Time;

-- | Current status of the stack.
sStackStatus :: Lens' Stack StackStatus
sStackStatus = lens _sStackStatus (\ s a -> s{_sStackStatus = a});

instance FromXML Stack where
        parseXML x
          = Stack' <$>
              (x .@? "DisableRollback") <*>
                (x .@? "LastUpdatedTime")
                <*>
                (x .@? "NotificationARNs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackStatusReason")
                <*>
                (x .@? "Outputs" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*>
                (x .@? "Capabilities" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TimeoutInMinutes")
                <*> (x .@ "StackName")
                <*> (x .@ "CreationTime")
                <*> (x .@ "StackStatus")

instance Hashable Stack

-- | The StackEvent data type.
--
-- /See:/ 'stackEvent' smart constructor.
data StackEvent = StackEvent'
    { _seLogicalResourceId    :: !(Maybe Text)
    , _sePhysicalResourceId   :: !(Maybe Text)
    , _seResourceType         :: !(Maybe Text)
    , _seResourceStatusReason :: !(Maybe Text)
    , _seResourceProperties   :: !(Maybe Text)
    , _seResourceStatus       :: !(Maybe ResourceStatus)
    , _seStackId              :: !Text
    , _seEventId              :: !Text
    , _seStackName            :: !Text
    , _seTimestamp            :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StackEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seLogicalResourceId'
--
-- * 'sePhysicalResourceId'
--
-- * 'seResourceType'
--
-- * 'seResourceStatusReason'
--
-- * 'seResourceProperties'
--
-- * 'seResourceStatus'
--
-- * 'seStackId'
--
-- * 'seEventId'
--
-- * 'seStackName'
--
-- * 'seTimestamp'
stackEvent
    :: Text -- ^ 'seStackId'
    -> Text -- ^ 'seEventId'
    -> Text -- ^ 'seStackName'
    -> UTCTime -- ^ 'seTimestamp'
    -> StackEvent
stackEvent pStackId_ pEventId_ pStackName_ pTimestamp_ =
    StackEvent'
    { _seLogicalResourceId = Nothing
    , _sePhysicalResourceId = Nothing
    , _seResourceType = Nothing
    , _seResourceStatusReason = Nothing
    , _seResourceProperties = Nothing
    , _seResourceStatus = Nothing
    , _seStackId = pStackId_
    , _seEventId = pEventId_
    , _seStackName = pStackName_
    , _seTimestamp = _Time # pTimestamp_
    }

-- | The logical name of the resource specified in the template.
seLogicalResourceId :: Lens' StackEvent (Maybe Text)
seLogicalResourceId = lens _seLogicalResourceId (\ s a -> s{_seLogicalResourceId = a});

-- | The name or unique identifier associated with the physical instance of
-- the resource.
sePhysicalResourceId :: Lens' StackEvent (Maybe Text)
sePhysicalResourceId = lens _sePhysicalResourceId (\ s a -> s{_sePhysicalResourceId = a});

-- | Type of resource. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
seResourceType :: Lens' StackEvent (Maybe Text)
seResourceType = lens _seResourceType (\ s a -> s{_seResourceType = a});

-- | Success\/failure message associated with the resource.
seResourceStatusReason :: Lens' StackEvent (Maybe Text)
seResourceStatusReason = lens _seResourceStatusReason (\ s a -> s{_seResourceStatusReason = a});

-- | BLOB of the properties used to create the resource.
seResourceProperties :: Lens' StackEvent (Maybe Text)
seResourceProperties = lens _seResourceProperties (\ s a -> s{_seResourceProperties = a});

-- | Current status of the resource.
seResourceStatus :: Lens' StackEvent (Maybe ResourceStatus)
seResourceStatus = lens _seResourceStatus (\ s a -> s{_seResourceStatus = a});

-- | The unique ID name of the instance of the stack.
seStackId :: Lens' StackEvent Text
seStackId = lens _seStackId (\ s a -> s{_seStackId = a});

-- | The unique ID of this event.
seEventId :: Lens' StackEvent Text
seEventId = lens _seEventId (\ s a -> s{_seEventId = a});

-- | The name associated with a stack.
seStackName :: Lens' StackEvent Text
seStackName = lens _seStackName (\ s a -> s{_seStackName = a});

-- | Time the status was updated.
seTimestamp :: Lens' StackEvent UTCTime
seTimestamp = lens _seTimestamp (\ s a -> s{_seTimestamp = a}) . _Time;

instance FromXML StackEvent where
        parseXML x
          = StackEvent' <$>
              (x .@? "LogicalResourceId") <*>
                (x .@? "PhysicalResourceId")
                <*> (x .@? "ResourceType")
                <*> (x .@? "ResourceStatusReason")
                <*> (x .@? "ResourceProperties")
                <*> (x .@? "ResourceStatus")
                <*> (x .@ "StackId")
                <*> (x .@ "EventId")
                <*> (x .@ "StackName")
                <*> (x .@ "Timestamp")

instance Hashable StackEvent

-- | The StackResource data type.
--
-- /See:/ 'stackResource' smart constructor.
data StackResource = StackResource'
    { _srPhysicalResourceId   :: !(Maybe Text)
    , _srResourceStatusReason :: !(Maybe Text)
    , _srStackId              :: !(Maybe Text)
    , _srDescription          :: !(Maybe Text)
    , _srStackName            :: !(Maybe Text)
    , _srLogicalResourceId    :: !Text
    , _srResourceType         :: !Text
    , _srTimestamp            :: !ISO8601
    , _srResourceStatus       :: !ResourceStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StackResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srPhysicalResourceId'
--
-- * 'srResourceStatusReason'
--
-- * 'srStackId'
--
-- * 'srDescription'
--
-- * 'srStackName'
--
-- * 'srLogicalResourceId'
--
-- * 'srResourceType'
--
-- * 'srTimestamp'
--
-- * 'srResourceStatus'
stackResource
    :: Text -- ^ 'srLogicalResourceId'
    -> Text -- ^ 'srResourceType'
    -> UTCTime -- ^ 'srTimestamp'
    -> ResourceStatus -- ^ 'srResourceStatus'
    -> StackResource
stackResource pLogicalResourceId_ pResourceType_ pTimestamp_ pResourceStatus_ =
    StackResource'
    { _srPhysicalResourceId = Nothing
    , _srResourceStatusReason = Nothing
    , _srStackId = Nothing
    , _srDescription = Nothing
    , _srStackName = Nothing
    , _srLogicalResourceId = pLogicalResourceId_
    , _srResourceType = pResourceType_
    , _srTimestamp = _Time # pTimestamp_
    , _srResourceStatus = pResourceStatus_
    }

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
srPhysicalResourceId :: Lens' StackResource (Maybe Text)
srPhysicalResourceId = lens _srPhysicalResourceId (\ s a -> s{_srPhysicalResourceId = a});

-- | Success\/failure message associated with the resource.
srResourceStatusReason :: Lens' StackResource (Maybe Text)
srResourceStatusReason = lens _srResourceStatusReason (\ s a -> s{_srResourceStatusReason = a});

-- | Unique identifier of the stack.
srStackId :: Lens' StackResource (Maybe Text)
srStackId = lens _srStackId (\ s a -> s{_srStackId = a});

-- | User defined description associated with the resource.
srDescription :: Lens' StackResource (Maybe Text)
srDescription = lens _srDescription (\ s a -> s{_srDescription = a});

-- | The name associated with the stack.
srStackName :: Lens' StackResource (Maybe Text)
srStackName = lens _srStackName (\ s a -> s{_srStackName = a});

-- | The logical name of the resource specified in the template.
srLogicalResourceId :: Lens' StackResource Text
srLogicalResourceId = lens _srLogicalResourceId (\ s a -> s{_srLogicalResourceId = a});

-- | Type of resource. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srResourceType :: Lens' StackResource Text
srResourceType = lens _srResourceType (\ s a -> s{_srResourceType = a});

-- | Time the status was updated.
srTimestamp :: Lens' StackResource UTCTime
srTimestamp = lens _srTimestamp (\ s a -> s{_srTimestamp = a}) . _Time;

-- | Current status of the resource.
srResourceStatus :: Lens' StackResource ResourceStatus
srResourceStatus = lens _srResourceStatus (\ s a -> s{_srResourceStatus = a});

instance FromXML StackResource where
        parseXML x
          = StackResource' <$>
              (x .@? "PhysicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "Timestamp")
                <*> (x .@ "ResourceStatus")

instance Hashable StackResource

-- | Contains detailed information about the specified stack resource.
--
-- /See:/ 'stackResourceDetail' smart constructor.
data StackResourceDetail = StackResourceDetail'
    { _srdPhysicalResourceId   :: !(Maybe Text)
    , _srdResourceStatusReason :: !(Maybe Text)
    , _srdMetadata             :: !(Maybe Text)
    , _srdStackId              :: !(Maybe Text)
    , _srdDescription          :: !(Maybe Text)
    , _srdStackName            :: !(Maybe Text)
    , _srdLogicalResourceId    :: !Text
    , _srdResourceType         :: !Text
    , _srdLastUpdatedTimestamp :: !ISO8601
    , _srdResourceStatus       :: !ResourceStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StackResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdPhysicalResourceId'
--
-- * 'srdResourceStatusReason'
--
-- * 'srdMetadata'
--
-- * 'srdStackId'
--
-- * 'srdDescription'
--
-- * 'srdStackName'
--
-- * 'srdLogicalResourceId'
--
-- * 'srdResourceType'
--
-- * 'srdLastUpdatedTimestamp'
--
-- * 'srdResourceStatus'
stackResourceDetail
    :: Text -- ^ 'srdLogicalResourceId'
    -> Text -- ^ 'srdResourceType'
    -> UTCTime -- ^ 'srdLastUpdatedTimestamp'
    -> ResourceStatus -- ^ 'srdResourceStatus'
    -> StackResourceDetail
stackResourceDetail pLogicalResourceId_ pResourceType_ pLastUpdatedTimestamp_ pResourceStatus_ =
    StackResourceDetail'
    { _srdPhysicalResourceId = Nothing
    , _srdResourceStatusReason = Nothing
    , _srdMetadata = Nothing
    , _srdStackId = Nothing
    , _srdDescription = Nothing
    , _srdStackName = Nothing
    , _srdLogicalResourceId = pLogicalResourceId_
    , _srdResourceType = pResourceType_
    , _srdLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp_
    , _srdResourceStatus = pResourceStatus_
    }

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
srdPhysicalResourceId :: Lens' StackResourceDetail (Maybe Text)
srdPhysicalResourceId = lens _srdPhysicalResourceId (\ s a -> s{_srdPhysicalResourceId = a});

-- | Success\/failure message associated with the resource.
srdResourceStatusReason :: Lens' StackResourceDetail (Maybe Text)
srdResourceStatusReason = lens _srdResourceStatusReason (\ s a -> s{_srdResourceStatusReason = a});

-- | The JSON format content of the 'Metadata' attribute declared for the
-- resource. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
-- in the AWS CloudFormation User Guide.
srdMetadata :: Lens' StackResourceDetail (Maybe Text)
srdMetadata = lens _srdMetadata (\ s a -> s{_srdMetadata = a});

-- | Unique identifier of the stack.
srdStackId :: Lens' StackResourceDetail (Maybe Text)
srdStackId = lens _srdStackId (\ s a -> s{_srdStackId = a});

-- | User defined description associated with the resource.
srdDescription :: Lens' StackResourceDetail (Maybe Text)
srdDescription = lens _srdDescription (\ s a -> s{_srdDescription = a});

-- | The name associated with the stack.
srdStackName :: Lens' StackResourceDetail (Maybe Text)
srdStackName = lens _srdStackName (\ s a -> s{_srdStackName = a});

-- | The logical name of the resource specified in the template.
srdLogicalResourceId :: Lens' StackResourceDetail Text
srdLogicalResourceId = lens _srdLogicalResourceId (\ s a -> s{_srdLogicalResourceId = a});

-- | Type of resource. ((For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srdResourceType :: Lens' StackResourceDetail Text
srdResourceType = lens _srdResourceType (\ s a -> s{_srdResourceType = a});

-- | Time the status was updated.
srdLastUpdatedTimestamp :: Lens' StackResourceDetail UTCTime
srdLastUpdatedTimestamp = lens _srdLastUpdatedTimestamp (\ s a -> s{_srdLastUpdatedTimestamp = a}) . _Time;

-- | Current status of the resource.
srdResourceStatus :: Lens' StackResourceDetail ResourceStatus
srdResourceStatus = lens _srdResourceStatus (\ s a -> s{_srdResourceStatus = a});

instance FromXML StackResourceDetail where
        parseXML x
          = StackResourceDetail' <$>
              (x .@? "PhysicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@? "Metadata")
                <*> (x .@? "StackId")
                <*> (x .@? "Description")
                <*> (x .@? "StackName")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "LastUpdatedTimestamp")
                <*> (x .@ "ResourceStatus")

instance Hashable StackResourceDetail

-- | Contains high-level information about the specified stack resource.
--
-- /See:/ 'stackResourceSummary' smart constructor.
data StackResourceSummary = StackResourceSummary'
    { _srsPhysicalResourceId   :: !(Maybe Text)
    , _srsResourceStatusReason :: !(Maybe Text)
    , _srsLogicalResourceId    :: !Text
    , _srsResourceType         :: !Text
    , _srsLastUpdatedTimestamp :: !ISO8601
    , _srsResourceStatus       :: !ResourceStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StackResourceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsPhysicalResourceId'
--
-- * 'srsResourceStatusReason'
--
-- * 'srsLogicalResourceId'
--
-- * 'srsResourceType'
--
-- * 'srsLastUpdatedTimestamp'
--
-- * 'srsResourceStatus'
stackResourceSummary
    :: Text -- ^ 'srsLogicalResourceId'
    -> Text -- ^ 'srsResourceType'
    -> UTCTime -- ^ 'srsLastUpdatedTimestamp'
    -> ResourceStatus -- ^ 'srsResourceStatus'
    -> StackResourceSummary
stackResourceSummary pLogicalResourceId_ pResourceType_ pLastUpdatedTimestamp_ pResourceStatus_ =
    StackResourceSummary'
    { _srsPhysicalResourceId = Nothing
    , _srsResourceStatusReason = Nothing
    , _srsLogicalResourceId = pLogicalResourceId_
    , _srsResourceType = pResourceType_
    , _srsLastUpdatedTimestamp = _Time # pLastUpdatedTimestamp_
    , _srsResourceStatus = pResourceStatus_
    }

-- | The name or unique identifier that corresponds to a physical instance ID
-- of the resource.
srsPhysicalResourceId :: Lens' StackResourceSummary (Maybe Text)
srsPhysicalResourceId = lens _srsPhysicalResourceId (\ s a -> s{_srsPhysicalResourceId = a});

-- | Success\/failure message associated with the resource.
srsResourceStatusReason :: Lens' StackResourceSummary (Maybe Text)
srsResourceStatusReason = lens _srsResourceStatusReason (\ s a -> s{_srsResourceStatusReason = a});

-- | The logical name of the resource specified in the template.
srsLogicalResourceId :: Lens' StackResourceSummary Text
srsLogicalResourceId = lens _srsLogicalResourceId (\ s a -> s{_srsLogicalResourceId = a});

-- | Type of resource. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>
-- in the AWS CloudFormation User Guide.)
srsResourceType :: Lens' StackResourceSummary Text
srsResourceType = lens _srsResourceType (\ s a -> s{_srsResourceType = a});

-- | Time the status was updated.
srsLastUpdatedTimestamp :: Lens' StackResourceSummary UTCTime
srsLastUpdatedTimestamp = lens _srsLastUpdatedTimestamp (\ s a -> s{_srsLastUpdatedTimestamp = a}) . _Time;

-- | Current status of the resource.
srsResourceStatus :: Lens' StackResourceSummary ResourceStatus
srsResourceStatus = lens _srsResourceStatus (\ s a -> s{_srsResourceStatus = a});

instance FromXML StackResourceSummary where
        parseXML x
          = StackResourceSummary' <$>
              (x .@? "PhysicalResourceId") <*>
                (x .@? "ResourceStatusReason")
                <*> (x .@ "LogicalResourceId")
                <*> (x .@ "ResourceType")
                <*> (x .@ "LastUpdatedTimestamp")
                <*> (x .@ "ResourceStatus")

instance Hashable StackResourceSummary

-- | The StackSummary Data Type
--
-- /See:/ 'stackSummary' smart constructor.
data StackSummary = StackSummary'
    { _ssLastUpdatedTime     :: !(Maybe ISO8601)
    , _ssStackStatusReason   :: !(Maybe Text)
    , _ssTemplateDescription :: !(Maybe Text)
    , _ssDeletionTime        :: !(Maybe ISO8601)
    , _ssStackId             :: !(Maybe Text)
    , _ssStackName           :: !Text
    , _ssCreationTime        :: !ISO8601
    , _ssStackStatus         :: !StackStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssLastUpdatedTime'
--
-- * 'ssStackStatusReason'
--
-- * 'ssTemplateDescription'
--
-- * 'ssDeletionTime'
--
-- * 'ssStackId'
--
-- * 'ssStackName'
--
-- * 'ssCreationTime'
--
-- * 'ssStackStatus'
stackSummary
    :: Text -- ^ 'ssStackName'
    -> UTCTime -- ^ 'ssCreationTime'
    -> StackStatus -- ^ 'ssStackStatus'
    -> StackSummary
stackSummary pStackName_ pCreationTime_ pStackStatus_ =
    StackSummary'
    { _ssLastUpdatedTime = Nothing
    , _ssStackStatusReason = Nothing
    , _ssTemplateDescription = Nothing
    , _ssDeletionTime = Nothing
    , _ssStackId = Nothing
    , _ssStackName = pStackName_
    , _ssCreationTime = _Time # pCreationTime_
    , _ssStackStatus = pStackStatus_
    }

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
ssLastUpdatedTime :: Lens' StackSummary (Maybe UTCTime)
ssLastUpdatedTime = lens _ssLastUpdatedTime (\ s a -> s{_ssLastUpdatedTime = a}) . mapping _Time;

-- | Success\/Failure message associated with the stack status.
ssStackStatusReason :: Lens' StackSummary (Maybe Text)
ssStackStatusReason = lens _ssStackStatusReason (\ s a -> s{_ssStackStatusReason = a});

-- | The template description of the template used to create the stack.
ssTemplateDescription :: Lens' StackSummary (Maybe Text)
ssTemplateDescription = lens _ssTemplateDescription (\ s a -> s{_ssTemplateDescription = a});

-- | The time the stack was deleted.
ssDeletionTime :: Lens' StackSummary (Maybe UTCTime)
ssDeletionTime = lens _ssDeletionTime (\ s a -> s{_ssDeletionTime = a}) . mapping _Time;

-- | Unique stack identifier.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\ s a -> s{_ssStackId = a});

-- | The name associated with the stack.
ssStackName :: Lens' StackSummary Text
ssStackName = lens _ssStackName (\ s a -> s{_ssStackName = a});

-- | The time the stack was created.
ssCreationTime :: Lens' StackSummary UTCTime
ssCreationTime = lens _ssCreationTime (\ s a -> s{_ssCreationTime = a}) . _Time;

-- | The current status of the stack.
ssStackStatus :: Lens' StackSummary StackStatus
ssStackStatus = lens _ssStackStatus (\ s a -> s{_ssStackStatus = a});

instance FromXML StackSummary where
        parseXML x
          = StackSummary' <$>
              (x .@? "LastUpdatedTime") <*>
                (x .@? "StackStatusReason")
                <*> (x .@? "TemplateDescription")
                <*> (x .@? "DeletionTime")
                <*> (x .@? "StackId")
                <*> (x .@ "StackName")
                <*> (x .@ "CreationTime")
                <*> (x .@ "StackStatus")

instance Hashable StackSummary

-- | The Tag type is used by 'CreateStack' in the 'Tags' parameter. It allows
-- you to specify a key-value pair that can be used to store information
-- related to cost allocation for an AWS CloudFormation stack.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | /Required/. A string containing the value for this tag. You can specify
-- a maximum of 256 characters for a tag value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | /Required/. A string used to identify this tag. You can specify a
-- maximum of 128 characters for a tag key. Tags owned by Amazon Web
-- Services (AWS) have the reserved prefix: 'aws:'.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The TemplateParameter data type.
--
-- /See:/ 'templateParameter' smart constructor.
data TemplateParameter = TemplateParameter'
    { _tpParameterKey :: !(Maybe Text)
    , _tpDefaultValue :: !(Maybe Text)
    , _tpNoEcho       :: !(Maybe Bool)
    , _tpDescription  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TemplateParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpParameterKey'
--
-- * 'tpDefaultValue'
--
-- * 'tpNoEcho'
--
-- * 'tpDescription'
templateParameter
    :: TemplateParameter
templateParameter =
    TemplateParameter'
    { _tpParameterKey = Nothing
    , _tpDefaultValue = Nothing
    , _tpNoEcho = Nothing
    , _tpDescription = Nothing
    }

-- | The name associated with the parameter.
tpParameterKey :: Lens' TemplateParameter (Maybe Text)
tpParameterKey = lens _tpParameterKey (\ s a -> s{_tpParameterKey = a});

-- | The default value associated with the parameter.
tpDefaultValue :: Lens' TemplateParameter (Maybe Text)
tpDefaultValue = lens _tpDefaultValue (\ s a -> s{_tpDefaultValue = a});

-- | Flag indicating whether the parameter should be displayed as plain text
-- in logs and UIs.
tpNoEcho :: Lens' TemplateParameter (Maybe Bool)
tpNoEcho = lens _tpNoEcho (\ s a -> s{_tpNoEcho = a});

-- | User defined description associated with the parameter.
tpDescription :: Lens' TemplateParameter (Maybe Text)
tpDescription = lens _tpDescription (\ s a -> s{_tpDescription = a});

instance FromXML TemplateParameter where
        parseXML x
          = TemplateParameter' <$>
              (x .@? "ParameterKey") <*> (x .@? "DefaultValue") <*>
                (x .@? "NoEcho")
                <*> (x .@? "Description")

instance Hashable TemplateParameter
