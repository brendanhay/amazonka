{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Handshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Handshake where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.ActionType
import Network.AWS.Organizations.Types.HandshakeParty
import Network.AWS.Organizations.Types.HandshakeResource
import Network.AWS.Organizations.Types.HandshakeState
import Network.AWS.Prelude

-- | Contains information that must be exchanged to securely establish a relationship between two accounts (an /originator/ and a /recipient/ ). For example, when a management account (the originator) invites another account (the recipient) to join its organization, the two accounts exchange information as a series of handshake requests and responses.
--
--
-- __Note:__ Handshakes that are CANCELED, ACCEPTED, or DECLINED show up in lists for only 30 days after entering that state After that they are deleted.
--
--
-- /See:/ 'handshake' smart constructor.
data Handshake = Handshake'
  { _hState :: !(Maybe HandshakeState),
    _hARN :: !(Maybe Text),
    _hAction :: !(Maybe ActionType),
    _hResources :: !(Maybe [HandshakeResource]),
    _hId :: !(Maybe Text),
    _hExpirationTimestamp :: !(Maybe POSIX),
    _hParties :: !(Maybe [HandshakeParty]),
    _hRequestedTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Handshake' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hState' - The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
--
-- * 'hARN' - The Amazon Resource Name (ARN) of a handshake. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'hAction' - The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the management account to only non-member accounts.     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the management account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's management account and approval is inferred.     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management account and signals the master that it can finalize the process to enable all features.
--
-- * 'hResources' - Additional information that is needed to process the handshake.
--
-- * 'hId' - The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- * 'hExpirationTimestamp' - The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
--
-- * 'hParties' - Information about the two accounts that are participating in the handshake.
--
-- * 'hRequestedTimestamp' - The date and time that the handshake request was made.
handshake ::
  Handshake
handshake =
  Handshake'
    { _hState = Nothing,
      _hARN = Nothing,
      _hAction = Nothing,
      _hResources = Nothing,
      _hId = Nothing,
      _hExpirationTimestamp = Nothing,
      _hParties = Nothing,
      _hRequestedTimestamp = Nothing
    }

-- | The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
hState :: Lens' Handshake (Maybe HandshakeState)
hState = lens _hState (\s a -> s {_hState = a})

-- | The Amazon Resource Name (ARN) of a handshake. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
hARN :: Lens' Handshake (Maybe Text)
hARN = lens _hARN (\s a -> s {_hARN = a})

-- | The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the management account to only non-member accounts.     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the management account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's management account and approval is inferred.     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management account and signals the master that it can finalize the process to enable all features.
hAction :: Lens' Handshake (Maybe ActionType)
hAction = lens _hAction (\s a -> s {_hAction = a})

-- | Additional information that is needed to process the handshake.
hResources :: Lens' Handshake [HandshakeResource]
hResources = lens _hResources (\s a -> s {_hResources = a}) . _Default . _Coerce

-- | The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
hId :: Lens' Handshake (Maybe Text)
hId = lens _hId (\s a -> s {_hId = a})

-- | The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
hExpirationTimestamp :: Lens' Handshake (Maybe UTCTime)
hExpirationTimestamp = lens _hExpirationTimestamp (\s a -> s {_hExpirationTimestamp = a}) . mapping _Time

-- | Information about the two accounts that are participating in the handshake.
hParties :: Lens' Handshake [HandshakeParty]
hParties = lens _hParties (\s a -> s {_hParties = a}) . _Default . _Coerce

-- | The date and time that the handshake request was made.
hRequestedTimestamp :: Lens' Handshake (Maybe UTCTime)
hRequestedTimestamp = lens _hRequestedTimestamp (\s a -> s {_hRequestedTimestamp = a}) . mapping _Time

instance FromJSON Handshake where
  parseJSON =
    withObject
      "Handshake"
      ( \x ->
          Handshake'
            <$> (x .:? "State")
            <*> (x .:? "Arn")
            <*> (x .:? "Action")
            <*> (x .:? "Resources" .!= mempty)
            <*> (x .:? "Id")
            <*> (x .:? "ExpirationTimestamp")
            <*> (x .:? "Parties" .!= mempty)
            <*> (x .:? "RequestedTimestamp")
      )

instance Hashable Handshake

instance NFData Handshake
