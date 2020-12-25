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
    hAction,
    hArn,
    hExpirationTimestamp,
    hId,
    hParties,
    hRequestedTimestamp,
    hResources,
    hState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.ActionType as Types
import qualified Network.AWS.Organizations.Types.Arn as Types
import qualified Network.AWS.Organizations.Types.HandshakeParty as Types
import qualified Network.AWS.Organizations.Types.HandshakeResource as Types
import qualified Network.AWS.Organizations.Types.HandshakeState as Types
import qualified Network.AWS.Organizations.Types.Id as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information that must be exchanged to securely establish a relationship between two accounts (an /originator/ and a /recipient/ ). For example, when a management account (the originator) invites another account (the recipient) to join its organization, the two accounts exchange information as a series of handshake requests and responses.
--
-- __Note:__ Handshakes that are CANCELED, ACCEPTED, or DECLINED show up in lists for only 30 days after entering that state After that they are deleted.
--
-- /See:/ 'mkHandshake' smart constructor.
data Handshake = Handshake'
  { -- | The type of handshake, indicating what action occurs when the recipient accepts the handshake. The following handshake types are supported:
    --
    --
    --     * __INVITE__ : This type of handshake represents a request to join an organization. It is always sent from the management account to only non-member accounts.
    --
    --
    --     * __ENABLE_ALL_FEATURES__ : This type of handshake represents a request to enable all features in an organization. It is always sent from the management account to only /invited/ member accounts. Created accounts do not receive this because those accounts were created by the organization's management account and approval is inferred.
    --
    --
    --     * __APPROVE_ALL_FEATURES__ : This type of handshake is sent from the Organizations service when all member accounts have approved the @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management account and signals the master that it can finalize the process to enable all features.
    action :: Core.Maybe Types.ActionType,
    -- | The Amazon Resource Name (ARN) of a handshake.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Core.Maybe Types.Arn,
    -- | The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
    expirationTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    id :: Core.Maybe Types.Id,
    -- | Information about the two accounts that are participating in the handshake.
    parties :: Core.Maybe [Types.HandshakeParty],
    -- | The date and time that the handshake request was made.
    requestedTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | Additional information that is needed to process the handshake.
    resources :: Core.Maybe [Types.HandshakeResource],
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
    state :: Core.Maybe Types.HandshakeState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Handshake' value with any optional fields omitted.
mkHandshake ::
  Handshake
mkHandshake =
  Handshake'
    { action = Core.Nothing,
      arn = Core.Nothing,
      expirationTimestamp = Core.Nothing,
      id = Core.Nothing,
      parties = Core.Nothing,
      requestedTimestamp = Core.Nothing,
      resources = Core.Nothing,
      state = Core.Nothing
    }

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
hAction :: Lens.Lens' Handshake (Core.Maybe Types.ActionType)
hAction = Lens.field @"action"
{-# DEPRECATED hAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The Amazon Resource Name (ARN) of a handshake.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hArn :: Lens.Lens' Handshake (Core.Maybe Types.Arn)
hArn = Lens.field @"arn"
{-# DEPRECATED hArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time that the handshake expires. If the recipient of the handshake request fails to respond before the specified date and time, the handshake becomes inactive and is no longer valid.
--
-- /Note:/ Consider using 'expirationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hExpirationTimestamp :: Lens.Lens' Handshake (Core.Maybe Core.NominalDiffTime)
hExpirationTimestamp = Lens.field @"expirationTimestamp"
{-# DEPRECATED hExpirationTimestamp "Use generic-lens or generic-optics with 'expirationTimestamp' instead." #-}

-- | The unique identifier (ID) of a handshake. The originating account creates the ID when it initiates the handshake.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hId :: Lens.Lens' Handshake (Core.Maybe Types.Id)
hId = Lens.field @"id"
{-# DEPRECATED hId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Information about the two accounts that are participating in the handshake.
--
-- /Note:/ Consider using 'parties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hParties :: Lens.Lens' Handshake (Core.Maybe [Types.HandshakeParty])
hParties = Lens.field @"parties"
{-# DEPRECATED hParties "Use generic-lens or generic-optics with 'parties' instead." #-}

-- | The date and time that the handshake request was made.
--
-- /Note:/ Consider using 'requestedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRequestedTimestamp :: Lens.Lens' Handshake (Core.Maybe Core.NominalDiffTime)
hRequestedTimestamp = Lens.field @"requestedTimestamp"
{-# DEPRECATED hRequestedTimestamp "Use generic-lens or generic-optics with 'requestedTimestamp' instead." #-}

-- | Additional information that is needed to process the handshake.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hResources :: Lens.Lens' Handshake (Core.Maybe [Types.HandshakeResource])
hResources = Lens.field @"resources"
{-# DEPRECATED hResources "Use generic-lens or generic-optics with 'resources' instead." #-}

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
hState :: Lens.Lens' Handshake (Core.Maybe Types.HandshakeState)
hState = Lens.field @"state"
{-# DEPRECATED hState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON Handshake where
  parseJSON =
    Core.withObject "Handshake" Core.$
      \x ->
        Handshake'
          Core.<$> (x Core..:? "Action")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "ExpirationTimestamp")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Parties")
          Core.<*> (x Core..:? "RequestedTimestamp")
          Core.<*> (x Core..:? "Resources")
          Core.<*> (x Core..:? "State")
