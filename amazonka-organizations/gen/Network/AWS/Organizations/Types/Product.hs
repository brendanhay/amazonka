{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.Product where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.Sum
import Network.AWS.Prelude

-- | Contains information about an AWS account that is a member of an organization.
--
--
--
-- /See:/ 'account' smart constructor.
data Account = Account'
  { _aStatus          :: !(Maybe AccountStatus)
  , _aJoinedMethod    :: !(Maybe AccountJoinedMethod)
  , _aEmail           :: !(Maybe (Sensitive Text))
  , _aARN             :: !(Maybe Text)
  , _aJoinedTimestamp :: !(Maybe POSIX)
  , _aName            :: !(Maybe (Sensitive Text))
  , _aId              :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Account' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aStatus' - The status of the account in the organization.
--
-- * 'aJoinedMethod' - The method by which the account joined the organization.
--
-- * 'aEmail' - The email address associated with the AWS account. The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters that represents a standard Internet email address.
--
-- * 'aARN' - The Amazon Resource Name (ARN) of the account. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'aJoinedTimestamp' - The date the account became a part of the organization.
--
-- * 'aName' - The friendly name of the account. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'aId' - The unique identifier (ID) of the account. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
account
    :: Account
account =
  Account'
    { _aStatus = Nothing
    , _aJoinedMethod = Nothing
    , _aEmail = Nothing
    , _aARN = Nothing
    , _aJoinedTimestamp = Nothing
    , _aName = Nothing
    , _aId = Nothing
    }


-- | The status of the account in the organization.
aStatus :: Lens' Account (Maybe AccountStatus)
aStatus = lens _aStatus (\ s a -> s{_aStatus = a})

-- | The method by which the account joined the organization.
aJoinedMethod :: Lens' Account (Maybe AccountJoinedMethod)
aJoinedMethod = lens _aJoinedMethod (\ s a -> s{_aJoinedMethod = a})

-- | The email address associated with the AWS account. The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters that represents a standard Internet email address.
aEmail :: Lens' Account (Maybe Text)
aEmail = lens _aEmail (\ s a -> s{_aEmail = a}) . mapping _Sensitive

-- | The Amazon Resource Name (ARN) of the account. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
aARN :: Lens' Account (Maybe Text)
aARN = lens _aARN (\ s a -> s{_aARN = a})

-- | The date the account became a part of the organization.
aJoinedTimestamp :: Lens' Account (Maybe UTCTime)
aJoinedTimestamp = lens _aJoinedTimestamp (\ s a -> s{_aJoinedTimestamp = a}) . mapping _Time

-- | The friendly name of the account. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
aName :: Lens' Account (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a}) . mapping _Sensitive

-- | The unique identifier (ID) of the account. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
aId :: Lens' Account (Maybe Text)
aId = lens _aId (\ s a -> s{_aId = a})

instance FromJSON Account where
        parseJSON
          = withObject "Account"
              (\ x ->
                 Account' <$>
                   (x .:? "Status") <*> (x .:? "JoinedMethod") <*>
                     (x .:? "Email")
                     <*> (x .:? "Arn")
                     <*> (x .:? "JoinedTimestamp")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable Account where

instance NFData Account where

-- | Contains a list of child entities, either OUs or accounts.
--
--
--
-- /See:/ 'child' smart constructor.
data Child = Child'
  { _cId   :: !(Maybe Text)
  , _cType :: !(Maybe ChildType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Child' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cId' - The unique identifier (ID) of this child entity. The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'cType' - The type of this child entity.
child
    :: Child
child = Child' {_cId = Nothing, _cType = Nothing}


-- | The unique identifier (ID) of this child entity. The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
cId :: Lens' Child (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | The type of this child entity.
cType :: Lens' Child (Maybe ChildType)
cType = lens _cType (\ s a -> s{_cType = a})

instance FromJSON Child where
        parseJSON
          = withObject "Child"
              (\ x -> Child' <$> (x .:? "Id") <*> (x .:? "Type"))

instance Hashable Child where

instance NFData Child where

-- | Contains the status about a 'CreateAccount' request to create an AWS account in an organization.
--
--
--
-- /See:/ 'createAccountStatus' smart constructor.
data CreateAccountStatus = CreateAccountStatus'
  { _casFailureReason      :: !(Maybe CreateAccountFailureReason)
  , _casState              :: !(Maybe CreateAccountState)
  , _casCompletedTimestamp :: !(Maybe POSIX)
  , _casAccountName        :: !(Maybe (Sensitive Text))
  , _casAccountId          :: !(Maybe Text)
  , _casId                 :: !(Maybe Text)
  , _casRequestedTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccountStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casFailureReason' - If the request failed, a description of the reason for the failure.     * ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you have reached the limit on the number of accounts in your organization.     * EMAIL_ALREADY_EXISTS: The account could not be created because another AWS account with that email address already exists.     * INVALID_ADDRESS: The account could not be created because the address you provided is not valid.     * INVALID_EMAIL: The account could not be created because the email address you provided is not valid.     * INTERNAL_FAILURE: The account could not be created because of an internal failure. Try again later. If the problem persists, contact Customer Support.
--
-- * 'casState' - The status of the request.
--
-- * 'casCompletedTimestamp' - The date and time that the account was created and the request completed.
--
-- * 'casAccountName' - The account name given to the account when it was created.
--
-- * 'casAccountId' - If the account was created successfully, the unique identifier (ID) of the new account. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- * 'casId' - The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account. The <http://wikipedia.org/wiki/regex regex pattern> for an create account request ID string requires "car-" followed by from 8 to 32 lower-case letters or digits.
--
-- * 'casRequestedTimestamp' - The date and time that the request was made for the account creation.
createAccountStatus
    :: CreateAccountStatus
createAccountStatus =
  CreateAccountStatus'
    { _casFailureReason = Nothing
    , _casState = Nothing
    , _casCompletedTimestamp = Nothing
    , _casAccountName = Nothing
    , _casAccountId = Nothing
    , _casId = Nothing
    , _casRequestedTimestamp = Nothing
    }


-- | If the request failed, a description of the reason for the failure.     * ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you have reached the limit on the number of accounts in your organization.     * EMAIL_ALREADY_EXISTS: The account could not be created because another AWS account with that email address already exists.     * INVALID_ADDRESS: The account could not be created because the address you provided is not valid.     * INVALID_EMAIL: The account could not be created because the email address you provided is not valid.     * INTERNAL_FAILURE: The account could not be created because of an internal failure. Try again later. If the problem persists, contact Customer Support.
casFailureReason :: Lens' CreateAccountStatus (Maybe CreateAccountFailureReason)
casFailureReason = lens _casFailureReason (\ s a -> s{_casFailureReason = a})

-- | The status of the request.
casState :: Lens' CreateAccountStatus (Maybe CreateAccountState)
casState = lens _casState (\ s a -> s{_casState = a})

-- | The date and time that the account was created and the request completed.
casCompletedTimestamp :: Lens' CreateAccountStatus (Maybe UTCTime)
casCompletedTimestamp = lens _casCompletedTimestamp (\ s a -> s{_casCompletedTimestamp = a}) . mapping _Time

-- | The account name given to the account when it was created.
casAccountName :: Lens' CreateAccountStatus (Maybe Text)
casAccountName = lens _casAccountName (\ s a -> s{_casAccountName = a}) . mapping _Sensitive

-- | If the account was created successfully, the unique identifier (ID) of the new account. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
casAccountId :: Lens' CreateAccountStatus (Maybe Text)
casAccountId = lens _casAccountId (\ s a -> s{_casAccountId = a})

-- | The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account. The <http://wikipedia.org/wiki/regex regex pattern> for an create account request ID string requires "car-" followed by from 8 to 32 lower-case letters or digits.
casId :: Lens' CreateAccountStatus (Maybe Text)
casId = lens _casId (\ s a -> s{_casId = a})

-- | The date and time that the request was made for the account creation.
casRequestedTimestamp :: Lens' CreateAccountStatus (Maybe UTCTime)
casRequestedTimestamp = lens _casRequestedTimestamp (\ s a -> s{_casRequestedTimestamp = a}) . mapping _Time

instance FromJSON CreateAccountStatus where
        parseJSON
          = withObject "CreateAccountStatus"
              (\ x ->
                 CreateAccountStatus' <$>
                   (x .:? "FailureReason") <*> (x .:? "State") <*>
                     (x .:? "CompletedTimestamp")
                     <*> (x .:? "AccountName")
                     <*> (x .:? "AccountId")
                     <*> (x .:? "Id")
                     <*> (x .:? "RequestedTimestamp"))

instance Hashable CreateAccountStatus where

instance NFData CreateAccountStatus where

-- | A structure that contains details of a service principal that is enabled to integrate with AWS Organizations.
--
--
--
-- /See:/ 'enabledServicePrincipal' smart constructor.
data EnabledServicePrincipal = EnabledServicePrincipal'
  { _espServicePrincipal :: !(Maybe Text)
  , _espDateEnabled      :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnabledServicePrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'espServicePrincipal' - The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- * 'espDateEnabled' - The date that the service principal was enabled for integration with AWS Organizations.
enabledServicePrincipal
    :: EnabledServicePrincipal
enabledServicePrincipal =
  EnabledServicePrincipal'
    {_espServicePrincipal = Nothing, _espDateEnabled = Nothing}


-- | The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
espServicePrincipal :: Lens' EnabledServicePrincipal (Maybe Text)
espServicePrincipal = lens _espServicePrincipal (\ s a -> s{_espServicePrincipal = a})

-- | The date that the service principal was enabled for integration with AWS Organizations.
espDateEnabled :: Lens' EnabledServicePrincipal (Maybe UTCTime)
espDateEnabled = lens _espDateEnabled (\ s a -> s{_espDateEnabled = a}) . mapping _Time

instance FromJSON EnabledServicePrincipal where
        parseJSON
          = withObject "EnabledServicePrincipal"
              (\ x ->
                 EnabledServicePrincipal' <$>
                   (x .:? "ServicePrincipal") <*> (x .:? "DateEnabled"))

instance Hashable EnabledServicePrincipal where

instance NFData EnabledServicePrincipal where

-- | Contains information that must be exchanged to securely establish a relationship between two accounts (an /originator/ and a /recipient/ ). For example, when a master account (the originator) invites another account (the recipient) to join its organization, the two accounts exchange information as a series of handshake requests and responses.
--
--
-- __Note:__ Handshakes that are CANCELED, ACCEPTED, or DECLINED show up in lists for only 30 days after entering that state After that they are deleted.
--
--
-- /See:/ 'handshake' smart constructor.
data Handshake = Handshake'
  { _hState               :: !(Maybe HandshakeState)
  , _hARN                 :: !(Maybe Text)
  , _hAction              :: !(Maybe ActionType)
  , _hResources           :: !(Maybe [HandshakeResource])
  , _hId                  :: !(Maybe Text)
  , _hExpirationTimestamp :: !(Maybe POSIX)
  , _hParties             :: !(Maybe [HandshakeParty])
  , _hRequestedTimestamp  :: !(Maybe POSIX)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Handshake' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hState' - The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
--
-- * 'hARN' - The Amazon Resource Name (ARN) of a handshake. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'hAction' - The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the master account to only non-member accounts.     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the master account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's master account and approval is inferred.     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the master account and signals the master that it can finalize the process to enable all features.
--
-- * 'hResources' - Additional information that is needed to process the handshake.
--
-- * 'hId' - The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
--
-- * 'hExpirationTimestamp' - The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
--
-- * 'hParties' - Information about the two accounts that are participating in the handshake.
--
-- * 'hRequestedTimestamp' - The date and time that the handshake request was made.
handshake
    :: Handshake
handshake =
  Handshake'
    { _hState = Nothing
    , _hARN = Nothing
    , _hAction = Nothing
    , _hResources = Nothing
    , _hId = Nothing
    , _hExpirationTimestamp = Nothing
    , _hParties = Nothing
    , _hRequestedTimestamp = Nothing
    }


-- | The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
hState :: Lens' Handshake (Maybe HandshakeState)
hState = lens _hState (\ s a -> s{_hState = a})

-- | The Amazon Resource Name (ARN) of a handshake. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
hARN :: Lens' Handshake (Maybe Text)
hARN = lens _hARN (\ s a -> s{_hARN = a})

-- | The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the master account to only non-member accounts.     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the master account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's master account and approval is inferred.     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the master account and signals the master that it can finalize the process to enable all features.
hAction :: Lens' Handshake (Maybe ActionType)
hAction = lens _hAction (\ s a -> s{_hAction = a})

-- | Additional information that is needed to process the handshake.
hResources :: Lens' Handshake [HandshakeResource]
hResources = lens _hResources (\ s a -> s{_hResources = a}) . _Default . _Coerce

-- | The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
hId :: Lens' Handshake (Maybe Text)
hId = lens _hId (\ s a -> s{_hId = a})

-- | The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
hExpirationTimestamp :: Lens' Handshake (Maybe UTCTime)
hExpirationTimestamp = lens _hExpirationTimestamp (\ s a -> s{_hExpirationTimestamp = a}) . mapping _Time

-- | Information about the two accounts that are participating in the handshake.
hParties :: Lens' Handshake [HandshakeParty]
hParties = lens _hParties (\ s a -> s{_hParties = a}) . _Default . _Coerce

-- | The date and time that the handshake request was made.
hRequestedTimestamp :: Lens' Handshake (Maybe UTCTime)
hRequestedTimestamp = lens _hRequestedTimestamp (\ s a -> s{_hRequestedTimestamp = a}) . mapping _Time

instance FromJSON Handshake where
        parseJSON
          = withObject "Handshake"
              (\ x ->
                 Handshake' <$>
                   (x .:? "State") <*> (x .:? "Arn") <*>
                     (x .:? "Action")
                     <*> (x .:? "Resources" .!= mempty)
                     <*> (x .:? "Id")
                     <*> (x .:? "ExpirationTimestamp")
                     <*> (x .:? "Parties" .!= mempty)
                     <*> (x .:? "RequestedTimestamp"))

instance Hashable Handshake where

instance NFData Handshake where

-- | Specifies the criteria that are used to select the handshakes for the operation.
--
--
--
-- /See:/ 'handshakeFilter' smart constructor.
data HandshakeFilter = HandshakeFilter'
  { _hfParentHandshakeId :: !(Maybe Text)
  , _hfActionType        :: !(Maybe ActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HandshakeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hfParentHandshakeId' - Specifies the parent handshake. Only used for handshake types that are a child of another type. If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ . The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
--
-- * 'hfActionType' - Specifies the type of handshake action. If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
handshakeFilter
    :: HandshakeFilter
handshakeFilter =
  HandshakeFilter' {_hfParentHandshakeId = Nothing, _hfActionType = Nothing}


-- | Specifies the parent handshake. Only used for handshake types that are a child of another type. If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ . The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
hfParentHandshakeId :: Lens' HandshakeFilter (Maybe Text)
hfParentHandshakeId = lens _hfParentHandshakeId (\ s a -> s{_hfParentHandshakeId = a})

-- | Specifies the type of handshake action. If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
hfActionType :: Lens' HandshakeFilter (Maybe ActionType)
hfActionType = lens _hfActionType (\ s a -> s{_hfActionType = a})

instance Hashable HandshakeFilter where

instance NFData HandshakeFilter where

instance ToJSON HandshakeFilter where
        toJSON HandshakeFilter'{..}
          = object
              (catMaybes
                 [("ParentHandshakeId" .=) <$> _hfParentHandshakeId,
                  ("ActionType" .=) <$> _hfActionType])

-- | Identifies a participant in a handshake.
--
--
--
-- /See:/ 'handshakeParty' smart constructor.
data HandshakeParty = HandshakeParty'
  { _hpId   :: !(Sensitive Text)
  , _hpType :: !HandshakePartyType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'HandshakeParty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpId' - The unique identifier (ID) for the party. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
--
-- * 'hpType' - The type of party.
handshakeParty
    :: Text -- ^ 'hpId'
    -> HandshakePartyType -- ^ 'hpType'
    -> HandshakeParty
handshakeParty pId_ pType_ =
  HandshakeParty' {_hpId = _Sensitive # pId_, _hpType = pType_}


-- | The unique identifier (ID) for the party. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
hpId :: Lens' HandshakeParty Text
hpId = lens _hpId (\ s a -> s{_hpId = a}) . _Sensitive

-- | The type of party.
hpType :: Lens' HandshakeParty HandshakePartyType
hpType = lens _hpType (\ s a -> s{_hpType = a})

instance FromJSON HandshakeParty where
        parseJSON
          = withObject "HandshakeParty"
              (\ x ->
                 HandshakeParty' <$> (x .: "Id") <*> (x .: "Type"))

instance Hashable HandshakeParty where

instance NFData HandshakeParty where

instance ToJSON HandshakeParty where
        toJSON HandshakeParty'{..}
          = object
              (catMaybes
                 [Just ("Id" .= _hpId), Just ("Type" .= _hpType)])

-- | Contains additional data that is needed to process a handshake.
--
--
--
-- /See:/ 'handshakeResource' smart constructor.
data HandshakeResource = HandshakeResource'
  { _hrValue     :: !(Maybe (Sensitive Text))
  , _hrResources :: !(Maybe [HandshakeResource])
  , _hrType      :: !(Maybe HandshakeResourceType)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'HandshakeResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hrValue' - The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
--
-- * 'hrResources' - When needed, contains an additional array of @HandshakeResource@ objects.
--
-- * 'hrType' - The type of information being passed, specifying how the value is to be interpreted by the other party:     * @ACCOUNT@ - Specifies an AWS account ID number.     * @ORGANIZATION@ - Specifies an organization ID number.     * @EMAIL@ - Specifies the email address that is associated with the account that receives the handshake.      * @OWNER_EMAIL@ - Specifies the email address associated with the master account. Included as information about an organization.      * @OWNER_NAME@ - Specifies the name associated with the master account. Included as information about an organization.      * @NOTES@ - Additional text provided by the handshake initiator and intended for the recipient to read.
handshakeResource
    :: HandshakeResource
handshakeResource =
  HandshakeResource'
    {_hrValue = Nothing, _hrResources = Nothing, _hrType = Nothing}


-- | The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
hrValue :: Lens' HandshakeResource (Maybe Text)
hrValue = lens _hrValue (\ s a -> s{_hrValue = a}) . mapping _Sensitive

-- | When needed, contains an additional array of @HandshakeResource@ objects.
hrResources :: Lens' HandshakeResource [HandshakeResource]
hrResources = lens _hrResources (\ s a -> s{_hrResources = a}) . _Default . _Coerce

-- | The type of information being passed, specifying how the value is to be interpreted by the other party:     * @ACCOUNT@ - Specifies an AWS account ID number.     * @ORGANIZATION@ - Specifies an organization ID number.     * @EMAIL@ - Specifies the email address that is associated with the account that receives the handshake.      * @OWNER_EMAIL@ - Specifies the email address associated with the master account. Included as information about an organization.      * @OWNER_NAME@ - Specifies the name associated with the master account. Included as information about an organization.      * @NOTES@ - Additional text provided by the handshake initiator and intended for the recipient to read.
hrType :: Lens' HandshakeResource (Maybe HandshakeResourceType)
hrType = lens _hrType (\ s a -> s{_hrType = a})

instance FromJSON HandshakeResource where
        parseJSON
          = withObject "HandshakeResource"
              (\ x ->
                 HandshakeResource' <$>
                   (x .:? "Value") <*> (x .:? "Resources" .!= mempty)
                     <*> (x .:? "Type"))

instance Hashable HandshakeResource where

instance NFData HandshakeResource where

-- | Contains details about an organization. An organization is a collection of accounts that are centrally managed together using consolidated billing, organized hierarchically with organizational units (OUs), and controlled with policies .
--
--
--
-- /See:/ 'organization' smart constructor.
data Organization = Organization'
  { _oARN                  :: !(Maybe Text)
  , _oMasterAccountId      :: !(Maybe Text)
  , _oMasterAccountARN     :: !(Maybe Text)
  , _oMasterAccountEmail   :: !(Maybe (Sensitive Text))
  , _oAvailablePolicyTypes :: !(Maybe [PolicyTypeSummary])
  , _oId                   :: !(Maybe Text)
  , _oFeatureSet           :: !(Maybe OrganizationFeatureSet)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Organization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oARN' - The Amazon Resource Name (ARN) of an organization. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'oMasterAccountId' - The unique identifier (ID) of the master account of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- * 'oMasterAccountARN' - The Amazon Resource Name (ARN) of the account that is designated as the master account for the organization. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'oMasterAccountEmail' - The email address that is associated with the AWS account that is designated as the master account for the organization.
--
-- * 'oAvailablePolicyTypes' - A list of policy types that are enabled for this organization. For example, if your organization has all features enabled, then service control policies (SCPs) are included in the list.
--
-- * 'oId' - The unique identifier (ID) of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lower-case letters or digits.
--
-- * 'oFeatureSet' - Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
organization
    :: Organization
organization =
  Organization'
    { _oARN = Nothing
    , _oMasterAccountId = Nothing
    , _oMasterAccountARN = Nothing
    , _oMasterAccountEmail = Nothing
    , _oAvailablePolicyTypes = Nothing
    , _oId = Nothing
    , _oFeatureSet = Nothing
    }


-- | The Amazon Resource Name (ARN) of an organization. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
oARN :: Lens' Organization (Maybe Text)
oARN = lens _oARN (\ s a -> s{_oARN = a})

-- | The unique identifier (ID) of the master account of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
oMasterAccountId :: Lens' Organization (Maybe Text)
oMasterAccountId = lens _oMasterAccountId (\ s a -> s{_oMasterAccountId = a})

-- | The Amazon Resource Name (ARN) of the account that is designated as the master account for the organization. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
oMasterAccountARN :: Lens' Organization (Maybe Text)
oMasterAccountARN = lens _oMasterAccountARN (\ s a -> s{_oMasterAccountARN = a})

-- | The email address that is associated with the AWS account that is designated as the master account for the organization.
oMasterAccountEmail :: Lens' Organization (Maybe Text)
oMasterAccountEmail = lens _oMasterAccountEmail (\ s a -> s{_oMasterAccountEmail = a}) . mapping _Sensitive

-- | A list of policy types that are enabled for this organization. For example, if your organization has all features enabled, then service control policies (SCPs) are included in the list.
oAvailablePolicyTypes :: Lens' Organization [PolicyTypeSummary]
oAvailablePolicyTypes = lens _oAvailablePolicyTypes (\ s a -> s{_oAvailablePolicyTypes = a}) . _Default . _Coerce

-- | The unique identifier (ID) of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lower-case letters or digits.
oId :: Lens' Organization (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a})

-- | Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
oFeatureSet :: Lens' Organization (Maybe OrganizationFeatureSet)
oFeatureSet = lens _oFeatureSet (\ s a -> s{_oFeatureSet = a})

instance FromJSON Organization where
        parseJSON
          = withObject "Organization"
              (\ x ->
                 Organization' <$>
                   (x .:? "Arn") <*> (x .:? "MasterAccountId") <*>
                     (x .:? "MasterAccountArn")
                     <*> (x .:? "MasterAccountEmail")
                     <*> (x .:? "AvailablePolicyTypes" .!= mempty)
                     <*> (x .:? "Id")
                     <*> (x .:? "FeatureSet"))

instance Hashable Organization where

instance NFData Organization where

-- | Contains details about an organizational unit (OU). An OU is a container of AWS accounts within a root of an organization. Policies that are attached to an OU apply to all accounts contained in that OU and in any child OUs.
--
--
--
-- /See:/ 'organizationalUnit' smart constructor.
data OrganizationalUnit = OrganizationalUnit'
  { _ouARN  :: !(Maybe Text)
  , _ouName :: !(Maybe Text)
  , _ouId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrganizationalUnit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ouARN' - The Amazon Resource Name (ARN) of this OU. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'ouName' - The friendly name of this OU. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'ouId' - The unique identifier (ID) associated with this OU. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
organizationalUnit
    :: OrganizationalUnit
organizationalUnit =
  OrganizationalUnit' {_ouARN = Nothing, _ouName = Nothing, _ouId = Nothing}


-- | The Amazon Resource Name (ARN) of this OU. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
ouARN :: Lens' OrganizationalUnit (Maybe Text)
ouARN = lens _ouARN (\ s a -> s{_ouARN = a})

-- | The friendly name of this OU. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
ouName :: Lens' OrganizationalUnit (Maybe Text)
ouName = lens _ouName (\ s a -> s{_ouName = a})

-- | The unique identifier (ID) associated with this OU. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
ouId :: Lens' OrganizationalUnit (Maybe Text)
ouId = lens _ouId (\ s a -> s{_ouId = a})

instance FromJSON OrganizationalUnit where
        parseJSON
          = withObject "OrganizationalUnit"
              (\ x ->
                 OrganizationalUnit' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable OrganizationalUnit where

instance NFData OrganizationalUnit where

-- | Contains information about either a root or an organizational unit (OU) that can contain OUs or accounts in an organization.
--
--
--
-- /See:/ 'parent' smart constructor.
data Parent = Parent'
  { _pId   :: !(Maybe Text)
  , _pType :: !(Maybe ParentType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pId' - The unique identifier (ID) of the parent entity. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'pType' - The type of the parent entity.
parent
    :: Parent
parent = Parent' {_pId = Nothing, _pType = Nothing}


-- | The unique identifier (ID) of the parent entity. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
pId :: Lens' Parent (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

-- | The type of the parent entity.
pType :: Lens' Parent (Maybe ParentType)
pType = lens _pType (\ s a -> s{_pType = a})

instance FromJSON Parent where
        parseJSON
          = withObject "Parent"
              (\ x -> Parent' <$> (x .:? "Id") <*> (x .:? "Type"))

instance Hashable Parent where

instance NFData Parent where

-- | Contains rules to be applied to the affected accounts. Policies can be attached directly to accounts, or to roots and OUs to affect all accounts in those hierarchies.
--
--
--
-- /See:/ 'policy' smart constructor.
data Policy = Policy'
  { _pContent       :: !(Maybe Text)
  , _pPolicySummary :: !(Maybe PolicySummary)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pContent' - The text content of the policy.
--
-- * 'pPolicySummary' - A structure that contains additional details about the policy.
policy
    :: Policy
policy = Policy' {_pContent = Nothing, _pPolicySummary = Nothing}


-- | The text content of the policy.
pContent :: Lens' Policy (Maybe Text)
pContent = lens _pContent (\ s a -> s{_pContent = a})

-- | A structure that contains additional details about the policy.
pPolicySummary :: Lens' Policy (Maybe PolicySummary)
pPolicySummary = lens _pPolicySummary (\ s a -> s{_pPolicySummary = a})

instance FromJSON Policy where
        parseJSON
          = withObject "Policy"
              (\ x ->
                 Policy' <$>
                   (x .:? "Content") <*> (x .:? "PolicySummary"))

instance Hashable Policy where

instance NFData Policy where

-- | Contains information about a policy, but does not include the content. To see the content of a policy, see 'DescribePolicy' .
--
--
--
-- /See:/ 'policySummary' smart constructor.
data PolicySummary = PolicySummary'
  { _psARN         :: !(Maybe Text)
  , _psName        :: !(Maybe Text)
  , _psId          :: !(Maybe Text)
  , _psAWSManaged  :: !(Maybe Bool)
  , _psType        :: !(Maybe PolicyType)
  , _psDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psARN' - The Amazon Resource Name (ARN) of the policy. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'psName' - The friendly name of the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'psId' - The unique identifier (ID) of the policy. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
--
-- * 'psAWSManaged' - A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
--
-- * 'psType' - The type of policy.
--
-- * 'psDescription' - The description of the policy.
policySummary
    :: PolicySummary
policySummary =
  PolicySummary'
    { _psARN = Nothing
    , _psName = Nothing
    , _psId = Nothing
    , _psAWSManaged = Nothing
    , _psType = Nothing
    , _psDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) of the policy. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
psARN :: Lens' PolicySummary (Maybe Text)
psARN = lens _psARN (\ s a -> s{_psARN = a})

-- | The friendly name of the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
psName :: Lens' PolicySummary (Maybe Text)
psName = lens _psName (\ s a -> s{_psName = a})

-- | The unique identifier (ID) of the policy. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
psId :: Lens' PolicySummary (Maybe Text)
psId = lens _psId (\ s a -> s{_psId = a})

-- | A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
psAWSManaged :: Lens' PolicySummary (Maybe Bool)
psAWSManaged = lens _psAWSManaged (\ s a -> s{_psAWSManaged = a})

-- | The type of policy.
psType :: Lens' PolicySummary (Maybe PolicyType)
psType = lens _psType (\ s a -> s{_psType = a})

-- | The description of the policy.
psDescription :: Lens' PolicySummary (Maybe Text)
psDescription = lens _psDescription (\ s a -> s{_psDescription = a})

instance FromJSON PolicySummary where
        parseJSON
          = withObject "PolicySummary"
              (\ x ->
                 PolicySummary' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id") <*>
                     (x .:? "AwsManaged")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable PolicySummary where

instance NFData PolicySummary where

-- | Contains information about a root, OU, or account that a policy is attached to.
--
--
--
-- /See:/ 'policyTargetSummary' smart constructor.
data PolicyTargetSummary = PolicyTargetSummary'
  { _polTargetId :: !(Maybe Text)
  , _polARN      :: !(Maybe Text)
  , _polName     :: !(Maybe Text)
  , _polType     :: !(Maybe TargetType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyTargetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'polTargetId' - The unique identifier (ID) of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'polARN' - The Amazon Resource Name (ARN) of the policy target. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'polName' - The friendly name of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'polType' - The type of the policy target.
policyTargetSummary
    :: PolicyTargetSummary
policyTargetSummary =
  PolicyTargetSummary'
    { _polTargetId = Nothing
    , _polARN = Nothing
    , _polName = Nothing
    , _polType = Nothing
    }


-- | The unique identifier (ID) of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
polTargetId :: Lens' PolicyTargetSummary (Maybe Text)
polTargetId = lens _polTargetId (\ s a -> s{_polTargetId = a})

-- | The Amazon Resource Name (ARN) of the policy target. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
polARN :: Lens' PolicyTargetSummary (Maybe Text)
polARN = lens _polARN (\ s a -> s{_polARN = a})

-- | The friendly name of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
polName :: Lens' PolicyTargetSummary (Maybe Text)
polName = lens _polName (\ s a -> s{_polName = a})

-- | The type of the policy target.
polType :: Lens' PolicyTargetSummary (Maybe TargetType)
polType = lens _polType (\ s a -> s{_polType = a})

instance FromJSON PolicyTargetSummary where
        parseJSON
          = withObject "PolicyTargetSummary"
              (\ x ->
                 PolicyTargetSummary' <$>
                   (x .:? "TargetId") <*> (x .:? "Arn") <*>
                     (x .:? "Name")
                     <*> (x .:? "Type"))

instance Hashable PolicyTargetSummary where

instance NFData PolicyTargetSummary where

-- | Contains information about a policy type and its status in the associated root.
--
--
--
-- /See:/ 'policyTypeSummary' smart constructor.
data PolicyTypeSummary = PolicyTypeSummary'
  { _ptsStatus :: !(Maybe PolicyTypeStatus)
  , _ptsType   :: !(Maybe PolicyType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyTypeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptsStatus' - The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
--
-- * 'ptsType' - The name of the policy type.
policyTypeSummary
    :: PolicyTypeSummary
policyTypeSummary =
  PolicyTypeSummary' {_ptsStatus = Nothing, _ptsType = Nothing}


-- | The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
ptsStatus :: Lens' PolicyTypeSummary (Maybe PolicyTypeStatus)
ptsStatus = lens _ptsStatus (\ s a -> s{_ptsStatus = a})

-- | The name of the policy type.
ptsType :: Lens' PolicyTypeSummary (Maybe PolicyType)
ptsType = lens _ptsType (\ s a -> s{_ptsType = a})

instance FromJSON PolicyTypeSummary where
        parseJSON
          = withObject "PolicyTypeSummary"
              (\ x ->
                 PolicyTypeSummary' <$>
                   (x .:? "Status") <*> (x .:? "Type"))

instance Hashable PolicyTypeSummary where

instance NFData PolicyTypeSummary where

-- | Contains details about a root. A root is a top-level parent node in the hierarchy of an organization that can contain organizational units (OUs) and accounts. Every root contains every AWS account in the organization. Each root enables the accounts to be organized in a different way and to have different policy types enabled for use in that root.
--
--
--
-- /See:/ 'root' smart constructor.
data Root = Root'
  { _rARN         :: !(Maybe Text)
  , _rName        :: !(Maybe Text)
  , _rId          :: !(Maybe Text)
  , _rPolicyTypes :: !(Maybe [PolicyTypeSummary])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Root' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rARN' - The Amazon Resource Name (ARN) of the root. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'rName' - The friendly name of the root. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'rId' - The unique identifier (ID) for the root. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lower-case letters or digits.
--
-- * 'rPolicyTypes' - The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
root
    :: Root
root =
  Root'
    {_rARN = Nothing, _rName = Nothing, _rId = Nothing, _rPolicyTypes = Nothing}


-- | The Amazon Resource Name (ARN) of the root. For more information about ARNs in Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
rARN :: Lens' Root (Maybe Text)
rARN = lens _rARN (\ s a -> s{_rARN = a})

-- | The friendly name of the root. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
rName :: Lens' Root (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The unique identifier (ID) for the root. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lower-case letters or digits.
rId :: Lens' Root (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a})

-- | The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
rPolicyTypes :: Lens' Root [PolicyTypeSummary]
rPolicyTypes = lens _rPolicyTypes (\ s a -> s{_rPolicyTypes = a}) . _Default . _Coerce

instance FromJSON Root where
        parseJSON
          = withObject "Root"
              (\ x ->
                 Root' <$>
                   (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id") <*>
                     (x .:? "PolicyTypes" .!= mempty))

instance Hashable Root where

instance NFData Root where
