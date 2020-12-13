{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Handshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Handshake
  ( Handshake (..),

    -- * Smart constructor
    mkHandshake,

    -- * Lenses
    hState,
    hARN,
    hAction,
    hResources,
    hId,
    hExpirationTimestamp,
    hParties,
    hRequestedTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.ActionType
import Network.AWS.Organizations.Types.HandshakeParty
import Network.AWS.Organizations.Types.HandshakeResource
import Network.AWS.Organizations.Types.HandshakeState
import qualified Network.AWS.Prelude as Lude

-- | Contains information that must be exchanged to securely establish a relationship between two accounts (an /originator/ and a /recipient/ ). For example, when a management account (the originator) invites another account (the recipient) to join its organization, the two accounts exchange information as a series of handshake requests and responses.
--
-- __Note:__ Handshakes that are CANCELED, ACCEPTED, or DECLINED show up in lists for only 30 days after entering that state After that they are deleted.
--
-- /See:/ 'mkHandshake' smart constructor.
data Handshake = Handshake'
  { -- | The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:
    --
    --
    --     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.
    --
    --
    --     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.
    --
    --
    --     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.
    --
    --
    --     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.
    --
    --
    --     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.
    --
    --
    --     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
    state :: Lude.Maybe HandshakeState,
    -- | The Amazon Resource Name (ARN) of a handshake.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Lude.Maybe Lude.Text,
    -- | The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:
    --
    --
    --     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the management account to only non-member accounts.
    --
    --
    --     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the management account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's management account and approval is inferred.
    --
    --
    --     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management account and signals the master that it can finalize the process to enable all features.
    action :: Lude.Maybe ActionType,
    -- | Additional information that is needed to process the handshake.
    resources :: Lude.Maybe [HandshakeResource],
    -- | The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    id :: Lude.Maybe Lude.Text,
    -- | The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
    expirationTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | Information about the two accounts that are participating in the handshake.
    parties :: Lude.Maybe [HandshakeParty],
    -- | The date and time that the handshake request was made.
    requestedTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Handshake' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:
--
--
--     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.
--
--
--     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.
--
--
--     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.
--
--
--     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.
--
--
--     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.
--
--
--     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
--
--
-- * 'arn' - The Amazon Resource Name (ARN) of a handshake.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'action' - The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:
--
--
--     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the management account to only non-member accounts.
--
--
--     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the management account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's management account and approval is inferred.
--
--
--     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management account and signals the master that it can finalize the process to enable all features.
--
--
-- * 'resources' - Additional information that is needed to process the handshake.
-- * 'id' - The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
-- * 'expirationTimestamp' - The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
-- * 'parties' - Information about the two accounts that are participating in the handshake.
-- * 'requestedTimestamp' - The date and time that the handshake request was made.
mkHandshake ::
  Handshake
mkHandshake =
  Handshake'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      action = Lude.Nothing,
      resources = Lude.Nothing,
      id = Lude.Nothing,
      expirationTimestamp = Lude.Nothing,
      parties = Lude.Nothing,
      requestedTimestamp = Lude.Nothing
    }

-- | The current state of the handshake. Use the state to trace the flow of the handshake through the process from its creation to its acceptance. The meaning of each of the valid values is as follows:
--
--
--     * __REQUESTED__ : This handshake was sent to multiple recipients (applicable to only some handshake types) and not all recipients have responded yet. The request stays in this state until all recipients respond.
--
--
--     * __OPEN__ : This handshake was sent to multiple recipients (applicable to only some policy types) and all recipients have responded, allowing the originator to complete the handshake action.
--
--
--     * __CANCELED__ : This handshake is no longer active because it was canceled by the originating account.
--
--
--     * __ACCEPTED__ : This handshake is complete because it has been accepted by the recipient.
--
--
--     * __DECLINED__ : This handshake is no longer active because it was declined by the recipient account.
--
--
--     * __EXPIRED__ : This handshake is no longer active because the originator did not receive a response of any kind from the recipient before the expiration time (15 days).
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hState :: Lens.Lens' Handshake (Lude.Maybe HandshakeState)
hState = Lens.lens (state :: Handshake -> Lude.Maybe HandshakeState) (\s a -> s {state = a} :: Handshake)
{-# DEPRECATED hState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of a handshake.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hARN :: Lens.Lens' Handshake (Lude.Maybe Lude.Text)
hARN = Lens.lens (arn :: Handshake -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Handshake)
{-# DEPRECATED hARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:
--
--
--     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the management account to only non-member accounts.
--
--
--     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the management account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's management account and approval is inferred.
--
--
--     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management account and signals the master that it can finalize the process to enable all features.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAction :: Lens.Lens' Handshake (Lude.Maybe ActionType)
hAction = Lens.lens (action :: Handshake -> Lude.Maybe ActionType) (\s a -> s {action = a} :: Handshake)
{-# DEPRECATED hAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Additional information that is needed to process the handshake.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hResources :: Lens.Lens' Handshake (Lude.Maybe [HandshakeResource])
hResources = Lens.lens (resources :: Handshake -> Lude.Maybe [HandshakeResource]) (\s a -> s {resources = a} :: Handshake)
{-# DEPRECATED hResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hId :: Lens.Lens' Handshake (Lude.Maybe Lude.Text)
hId = Lens.lens (id :: Handshake -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Handshake)
{-# DEPRECATED hId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
--
-- /Note:/ Consider using 'expirationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hExpirationTimestamp :: Lens.Lens' Handshake (Lude.Maybe Lude.Timestamp)
hExpirationTimestamp = Lens.lens (expirationTimestamp :: Handshake -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTimestamp = a} :: Handshake)
{-# DEPRECATED hExpirationTimestamp "Use generic-lens or generic-optics with 'expirationTimestamp' instead." #-}

-- | Information about the two accounts that are participating in the handshake.
--
-- /Note:/ Consider using 'parties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParties :: Lens.Lens' Handshake (Lude.Maybe [HandshakeParty])
hParties = Lens.lens (parties :: Handshake -> Lude.Maybe [HandshakeParty]) (\s a -> s {parties = a} :: Handshake)
{-# DEPRECATED hParties "Use generic-lens or generic-optics with 'parties' instead." #-}

-- | The date and time that the handshake request was made.
--
-- /Note:/ Consider using 'requestedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRequestedTimestamp :: Lens.Lens' Handshake (Lude.Maybe Lude.Timestamp)
hRequestedTimestamp = Lens.lens (requestedTimestamp :: Handshake -> Lude.Maybe Lude.Timestamp) (\s a -> s {requestedTimestamp = a} :: Handshake)
{-# DEPRECATED hRequestedTimestamp "Use generic-lens or generic-optics with 'requestedTimestamp' instead." #-}

instance Lude.FromJSON Handshake where
  parseJSON =
    Lude.withObject
      "Handshake"
      ( \x ->
          Handshake'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "Resources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "ExpirationTimestamp")
            Lude.<*> (x Lude..:? "Parties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RequestedTimestamp")
      )
