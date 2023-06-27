{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Organizations.Types.Handshake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.Handshake where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.ActionType
import Amazonka.Organizations.Types.HandshakeParty
import Amazonka.Organizations.Types.HandshakeResource
import Amazonka.Organizations.Types.HandshakeState
import qualified Amazonka.Prelude as Prelude

-- | Contains information that must be exchanged to securely establish a
-- relationship between two accounts (an /originator/ and a /recipient/).
-- For example, when a management account (the originator) invites another
-- account (the recipient) to join its organization, the two accounts
-- exchange information as a series of handshake requests and responses.
--
-- __Note:__ Handshakes that are @CANCELED@, @ACCEPTED@, @DECLINED@, or
-- @EXPIRED@ show up in lists for only 30 days after entering that state
-- After that they are deleted.
--
-- /See:/ 'newHandshake' smart constructor.
data Handshake = Handshake'
  { -- | The type of handshake, indicating what action occurs when the recipient
    -- accepts the handshake. The following handshake types are supported:
    --
    -- -   __INVITE__: This type of handshake represents a request to join an
    --     organization. It is always sent from the management account to only
    --     non-member accounts.
    --
    -- -   __ENABLE_ALL_FEATURES__: This type of handshake represents a request
    --     to enable all features in an organization. It is always sent from
    --     the management account to only /invited/ member accounts. Created
    --     accounts do not receive this because those accounts were created by
    --     the organization\'s management account and approval is inferred.
    --
    -- -   __APPROVE_ALL_FEATURES__: This type of handshake is sent from the
    --     Organizations service when all member accounts have approved the
    --     @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management
    --     account and signals the master that it can finalize the process to
    --     enable all features.
    action :: Prelude.Maybe ActionType,
    -- | The Amazon Resource Name (ARN) of a handshake.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /Amazon Web Services Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the handshake expires. If the recipient of the
    -- handshake request fails to respond before the specified date and time,
    -- the handshake becomes inactive and is no longer valid.
    expirationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier (ID) of a handshake. The originating account
    -- creates the ID when it initiates the handshake.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the two accounts that are participating in the
    -- handshake.
    parties :: Prelude.Maybe [HandshakeParty],
    -- | The date and time that the handshake request was made.
    requestedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Additional information that is needed to process the handshake.
    resources :: Prelude.Maybe [HandshakeResource],
    -- | The current state of the handshake. Use the state to trace the flow of
    -- the handshake through the process from its creation to its acceptance.
    -- The meaning of each of the valid values is as follows:
    --
    -- -   __REQUESTED__: This handshake was sent to multiple recipients
    --     (applicable to only some handshake types) and not all recipients
    --     have responded yet. The request stays in this state until all
    --     recipients respond.
    --
    -- -   __OPEN__: This handshake was sent to multiple recipients (applicable
    --     to only some policy types) and all recipients have responded,
    --     allowing the originator to complete the handshake action.
    --
    -- -   __CANCELED__: This handshake is no longer active because it was
    --     canceled by the originating account.
    --
    -- -   __ACCEPTED__: This handshake is complete because it has been
    --     accepted by the recipient.
    --
    -- -   __DECLINED__: This handshake is no longer active because it was
    --     declined by the recipient account.
    --
    -- -   __EXPIRED__: This handshake is no longer active because the
    --     originator did not receive a response of any kind from the recipient
    --     before the expiration time (15 days).
    state :: Prelude.Maybe HandshakeState
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Handshake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'handshake_action' - The type of handshake, indicating what action occurs when the recipient
-- accepts the handshake. The following handshake types are supported:
--
-- -   __INVITE__: This type of handshake represents a request to join an
--     organization. It is always sent from the management account to only
--     non-member accounts.
--
-- -   __ENABLE_ALL_FEATURES__: This type of handshake represents a request
--     to enable all features in an organization. It is always sent from
--     the management account to only /invited/ member accounts. Created
--     accounts do not receive this because those accounts were created by
--     the organization\'s management account and approval is inferred.
--
-- -   __APPROVE_ALL_FEATURES__: This type of handshake is sent from the
--     Organizations service when all member accounts have approved the
--     @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management
--     account and signals the master that it can finalize the process to
--     enable all features.
--
-- 'arn', 'handshake_arn' - The Amazon Resource Name (ARN) of a handshake.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
--
-- 'expirationTimestamp', 'handshake_expirationTimestamp' - The date and time that the handshake expires. If the recipient of the
-- handshake request fails to respond before the specified date and time,
-- the handshake becomes inactive and is no longer valid.
--
-- 'id', 'handshake_id' - The unique identifier (ID) of a handshake. The originating account
-- creates the ID when it initiates the handshake.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
--
-- 'parties', 'handshake_parties' - Information about the two accounts that are participating in the
-- handshake.
--
-- 'requestedTimestamp', 'handshake_requestedTimestamp' - The date and time that the handshake request was made.
--
-- 'resources', 'handshake_resources' - Additional information that is needed to process the handshake.
--
-- 'state', 'handshake_state' - The current state of the handshake. Use the state to trace the flow of
-- the handshake through the process from its creation to its acceptance.
-- The meaning of each of the valid values is as follows:
--
-- -   __REQUESTED__: This handshake was sent to multiple recipients
--     (applicable to only some handshake types) and not all recipients
--     have responded yet. The request stays in this state until all
--     recipients respond.
--
-- -   __OPEN__: This handshake was sent to multiple recipients (applicable
--     to only some policy types) and all recipients have responded,
--     allowing the originator to complete the handshake action.
--
-- -   __CANCELED__: This handshake is no longer active because it was
--     canceled by the originating account.
--
-- -   __ACCEPTED__: This handshake is complete because it has been
--     accepted by the recipient.
--
-- -   __DECLINED__: This handshake is no longer active because it was
--     declined by the recipient account.
--
-- -   __EXPIRED__: This handshake is no longer active because the
--     originator did not receive a response of any kind from the recipient
--     before the expiration time (15 days).
newHandshake ::
  Handshake
newHandshake =
  Handshake'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      expirationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      parties = Prelude.Nothing,
      requestedTimestamp = Prelude.Nothing,
      resources = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The type of handshake, indicating what action occurs when the recipient
-- accepts the handshake. The following handshake types are supported:
--
-- -   __INVITE__: This type of handshake represents a request to join an
--     organization. It is always sent from the management account to only
--     non-member accounts.
--
-- -   __ENABLE_ALL_FEATURES__: This type of handshake represents a request
--     to enable all features in an organization. It is always sent from
--     the management account to only /invited/ member accounts. Created
--     accounts do not receive this because those accounts were created by
--     the organization\'s management account and approval is inferred.
--
-- -   __APPROVE_ALL_FEATURES__: This type of handshake is sent from the
--     Organizations service when all member accounts have approved the
--     @ENABLE_ALL_FEATURES@ invitation. It is sent only to the management
--     account and signals the master that it can finalize the process to
--     enable all features.
handshake_action :: Lens.Lens' Handshake (Prelude.Maybe ActionType)
handshake_action = Lens.lens (\Handshake' {action} -> action) (\s@Handshake' {} a -> s {action = a} :: Handshake)

-- | The Amazon Resource Name (ARN) of a handshake.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
handshake_arn :: Lens.Lens' Handshake (Prelude.Maybe Prelude.Text)
handshake_arn = Lens.lens (\Handshake' {arn} -> arn) (\s@Handshake' {} a -> s {arn = a} :: Handshake)

-- | The date and time that the handshake expires. If the recipient of the
-- handshake request fails to respond before the specified date and time,
-- the handshake becomes inactive and is no longer valid.
handshake_expirationTimestamp :: Lens.Lens' Handshake (Prelude.Maybe Prelude.UTCTime)
handshake_expirationTimestamp = Lens.lens (\Handshake' {expirationTimestamp} -> expirationTimestamp) (\s@Handshake' {} a -> s {expirationTimestamp = a} :: Handshake) Prelude.. Lens.mapping Data._Time

-- | The unique identifier (ID) of a handshake. The originating account
-- creates the ID when it initiates the handshake.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
handshake_id :: Lens.Lens' Handshake (Prelude.Maybe Prelude.Text)
handshake_id = Lens.lens (\Handshake' {id} -> id) (\s@Handshake' {} a -> s {id = a} :: Handshake)

-- | Information about the two accounts that are participating in the
-- handshake.
handshake_parties :: Lens.Lens' Handshake (Prelude.Maybe [HandshakeParty])
handshake_parties = Lens.lens (\Handshake' {parties} -> parties) (\s@Handshake' {} a -> s {parties = a} :: Handshake) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the handshake request was made.
handshake_requestedTimestamp :: Lens.Lens' Handshake (Prelude.Maybe Prelude.UTCTime)
handshake_requestedTimestamp = Lens.lens (\Handshake' {requestedTimestamp} -> requestedTimestamp) (\s@Handshake' {} a -> s {requestedTimestamp = a} :: Handshake) Prelude.. Lens.mapping Data._Time

-- | Additional information that is needed to process the handshake.
handshake_resources :: Lens.Lens' Handshake (Prelude.Maybe [HandshakeResource])
handshake_resources = Lens.lens (\Handshake' {resources} -> resources) (\s@Handshake' {} a -> s {resources = a} :: Handshake) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the handshake. Use the state to trace the flow of
-- the handshake through the process from its creation to its acceptance.
-- The meaning of each of the valid values is as follows:
--
-- -   __REQUESTED__: This handshake was sent to multiple recipients
--     (applicable to only some handshake types) and not all recipients
--     have responded yet. The request stays in this state until all
--     recipients respond.
--
-- -   __OPEN__: This handshake was sent to multiple recipients (applicable
--     to only some policy types) and all recipients have responded,
--     allowing the originator to complete the handshake action.
--
-- -   __CANCELED__: This handshake is no longer active because it was
--     canceled by the originating account.
--
-- -   __ACCEPTED__: This handshake is complete because it has been
--     accepted by the recipient.
--
-- -   __DECLINED__: This handshake is no longer active because it was
--     declined by the recipient account.
--
-- -   __EXPIRED__: This handshake is no longer active because the
--     originator did not receive a response of any kind from the recipient
--     before the expiration time (15 days).
handshake_state :: Lens.Lens' Handshake (Prelude.Maybe HandshakeState)
handshake_state = Lens.lens (\Handshake' {state} -> state) (\s@Handshake' {} a -> s {state = a} :: Handshake)

instance Data.FromJSON Handshake where
  parseJSON =
    Data.withObject
      "Handshake"
      ( \x ->
          Handshake'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ExpirationTimestamp")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Parties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RequestedTimestamp")
            Prelude.<*> (x Data..:? "Resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Handshake where
  hashWithSalt _salt Handshake' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` expirationTimestamp
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` parties
      `Prelude.hashWithSalt` requestedTimestamp
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` state

instance Prelude.NFData Handshake where
  rnf Handshake' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf expirationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf parties
      `Prelude.seq` Prelude.rnf requestedTimestamp
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf state
