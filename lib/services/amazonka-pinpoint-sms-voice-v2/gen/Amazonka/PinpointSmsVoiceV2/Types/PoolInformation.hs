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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PoolInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PoolInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types.MessageType
import Amazonka.PinpointSmsVoiceV2.Types.PoolStatus
import qualified Amazonka.Prelude as Prelude

-- | The information for a pool in an Amazon Web Services account.
--
-- /See:/ 'newPoolInformation' smart constructor.
data PoolInformation = PoolInformation'
  { -- | The Amazon Resource Name (ARN) of the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the pool.
    poolArn :: Prelude.Text,
    -- | The unique identifier for the pool.
    poolId :: Prelude.Text,
    -- | The current status of the pool.
    status :: PoolStatus,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: MessageType,
    -- | When set to true you can receive incoming text messages from your end
    -- recipients using the TwoWayChannelArn.
    twoWayEnabled :: Prelude.Bool,
    -- | When set to false, an end recipient sends a message that begins with
    -- HELP or STOP to one of your dedicated numbers, Amazon Pinpoint
    -- automatically replies with a customizable message and adds the end
    -- recipient to the OptOutList. When set to true you\'re responsible for
    -- responding to HELP and STOP requests. You\'re also responsible for
    -- tracking and honoring opt-out requests. For more information see
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-managing.html#settings-account-sms-self-managed-opt-out Self-managed opt-outs>
    selfManagedOptOutsEnabled :: Prelude.Bool,
    -- | The name of the OptOutList associated with the pool.
    optOutListName :: Prelude.Text,
    -- | Allows you to enable shared routes on your pool.
    --
    -- By default, this is set to @False@. If you set this value to @True@,
    -- your messages are sent using phone numbers or sender IDs (depending on
    -- the country) that are shared with other Amazon Pinpoint users. In some
    -- countries, such as the United States, senders aren\'t allowed to use
    -- shared routes and must use a dedicated phone number or short code.
    sharedRoutesEnabled :: Prelude.Bool,
    -- | When set to true the pool can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Bool,
    -- | The time when the pool was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PoolInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'twoWayChannelArn', 'poolInformation_twoWayChannelArn' - The Amazon Resource Name (ARN) of the two way channel.
--
-- 'poolArn', 'poolInformation_poolArn' - The Amazon Resource Name (ARN) for the pool.
--
-- 'poolId', 'poolInformation_poolId' - The unique identifier for the pool.
--
-- 'status', 'poolInformation_status' - The current status of the pool.
--
-- 'messageType', 'poolInformation_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'twoWayEnabled', 'poolInformation_twoWayEnabled' - When set to true you can receive incoming text messages from your end
-- recipients using the TwoWayChannelArn.
--
-- 'selfManagedOptOutsEnabled', 'poolInformation_selfManagedOptOutsEnabled' - When set to false, an end recipient sends a message that begins with
-- HELP or STOP to one of your dedicated numbers, Amazon Pinpoint
-- automatically replies with a customizable message and adds the end
-- recipient to the OptOutList. When set to true you\'re responsible for
-- responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests. For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-managing.html#settings-account-sms-self-managed-opt-out Self-managed opt-outs>
--
-- 'optOutListName', 'poolInformation_optOutListName' - The name of the OptOutList associated with the pool.
--
-- 'sharedRoutesEnabled', 'poolInformation_sharedRoutesEnabled' - Allows you to enable shared routes on your pool.
--
-- By default, this is set to @False@. If you set this value to @True@,
-- your messages are sent using phone numbers or sender IDs (depending on
-- the country) that are shared with other Amazon Pinpoint users. In some
-- countries, such as the United States, senders aren\'t allowed to use
-- shared routes and must use a dedicated phone number or short code.
--
-- 'deletionProtectionEnabled', 'poolInformation_deletionProtectionEnabled' - When set to true the pool can\'t be deleted.
--
-- 'createdTimestamp', 'poolInformation_createdTimestamp' - The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
newPoolInformation ::
  -- | 'poolArn'
  Prelude.Text ->
  -- | 'poolId'
  Prelude.Text ->
  -- | 'status'
  PoolStatus ->
  -- | 'messageType'
  MessageType ->
  -- | 'twoWayEnabled'
  Prelude.Bool ->
  -- | 'selfManagedOptOutsEnabled'
  Prelude.Bool ->
  -- | 'optOutListName'
  Prelude.Text ->
  -- | 'sharedRoutesEnabled'
  Prelude.Bool ->
  -- | 'deletionProtectionEnabled'
  Prelude.Bool ->
  -- | 'createdTimestamp'
  Prelude.UTCTime ->
  PoolInformation
newPoolInformation
  pPoolArn_
  pPoolId_
  pStatus_
  pMessageType_
  pTwoWayEnabled_
  pSelfManagedOptOutsEnabled_
  pOptOutListName_
  pSharedRoutesEnabled_
  pDeletionProtectionEnabled_
  pCreatedTimestamp_ =
    PoolInformation'
      { twoWayChannelArn =
          Prelude.Nothing,
        poolArn = pPoolArn_,
        poolId = pPoolId_,
        status = pStatus_,
        messageType = pMessageType_,
        twoWayEnabled = pTwoWayEnabled_,
        selfManagedOptOutsEnabled =
          pSelfManagedOptOutsEnabled_,
        optOutListName = pOptOutListName_,
        sharedRoutesEnabled = pSharedRoutesEnabled_,
        deletionProtectionEnabled =
          pDeletionProtectionEnabled_,
        createdTimestamp =
          Core._Time Lens.# pCreatedTimestamp_
      }

-- | The Amazon Resource Name (ARN) of the two way channel.
poolInformation_twoWayChannelArn :: Lens.Lens' PoolInformation (Prelude.Maybe Prelude.Text)
poolInformation_twoWayChannelArn = Lens.lens (\PoolInformation' {twoWayChannelArn} -> twoWayChannelArn) (\s@PoolInformation' {} a -> s {twoWayChannelArn = a} :: PoolInformation)

-- | The Amazon Resource Name (ARN) for the pool.
poolInformation_poolArn :: Lens.Lens' PoolInformation Prelude.Text
poolInformation_poolArn = Lens.lens (\PoolInformation' {poolArn} -> poolArn) (\s@PoolInformation' {} a -> s {poolArn = a} :: PoolInformation)

-- | The unique identifier for the pool.
poolInformation_poolId :: Lens.Lens' PoolInformation Prelude.Text
poolInformation_poolId = Lens.lens (\PoolInformation' {poolId} -> poolId) (\s@PoolInformation' {} a -> s {poolId = a} :: PoolInformation)

-- | The current status of the pool.
poolInformation_status :: Lens.Lens' PoolInformation PoolStatus
poolInformation_status = Lens.lens (\PoolInformation' {status} -> status) (\s@PoolInformation' {} a -> s {status = a} :: PoolInformation)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
poolInformation_messageType :: Lens.Lens' PoolInformation MessageType
poolInformation_messageType = Lens.lens (\PoolInformation' {messageType} -> messageType) (\s@PoolInformation' {} a -> s {messageType = a} :: PoolInformation)

-- | When set to true you can receive incoming text messages from your end
-- recipients using the TwoWayChannelArn.
poolInformation_twoWayEnabled :: Lens.Lens' PoolInformation Prelude.Bool
poolInformation_twoWayEnabled = Lens.lens (\PoolInformation' {twoWayEnabled} -> twoWayEnabled) (\s@PoolInformation' {} a -> s {twoWayEnabled = a} :: PoolInformation)

-- | When set to false, an end recipient sends a message that begins with
-- HELP or STOP to one of your dedicated numbers, Amazon Pinpoint
-- automatically replies with a customizable message and adds the end
-- recipient to the OptOutList. When set to true you\'re responsible for
-- responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests. For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-managing.html#settings-account-sms-self-managed-opt-out Self-managed opt-outs>
poolInformation_selfManagedOptOutsEnabled :: Lens.Lens' PoolInformation Prelude.Bool
poolInformation_selfManagedOptOutsEnabled = Lens.lens (\PoolInformation' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@PoolInformation' {} a -> s {selfManagedOptOutsEnabled = a} :: PoolInformation)

-- | The name of the OptOutList associated with the pool.
poolInformation_optOutListName :: Lens.Lens' PoolInformation Prelude.Text
poolInformation_optOutListName = Lens.lens (\PoolInformation' {optOutListName} -> optOutListName) (\s@PoolInformation' {} a -> s {optOutListName = a} :: PoolInformation)

-- | Allows you to enable shared routes on your pool.
--
-- By default, this is set to @False@. If you set this value to @True@,
-- your messages are sent using phone numbers or sender IDs (depending on
-- the country) that are shared with other Amazon Pinpoint users. In some
-- countries, such as the United States, senders aren\'t allowed to use
-- shared routes and must use a dedicated phone number or short code.
poolInformation_sharedRoutesEnabled :: Lens.Lens' PoolInformation Prelude.Bool
poolInformation_sharedRoutesEnabled = Lens.lens (\PoolInformation' {sharedRoutesEnabled} -> sharedRoutesEnabled) (\s@PoolInformation' {} a -> s {sharedRoutesEnabled = a} :: PoolInformation)

-- | When set to true the pool can\'t be deleted.
poolInformation_deletionProtectionEnabled :: Lens.Lens' PoolInformation Prelude.Bool
poolInformation_deletionProtectionEnabled = Lens.lens (\PoolInformation' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@PoolInformation' {} a -> s {deletionProtectionEnabled = a} :: PoolInformation)

-- | The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
poolInformation_createdTimestamp :: Lens.Lens' PoolInformation Prelude.UTCTime
poolInformation_createdTimestamp = Lens.lens (\PoolInformation' {createdTimestamp} -> createdTimestamp) (\s@PoolInformation' {} a -> s {createdTimestamp = a} :: PoolInformation) Prelude.. Core._Time

instance Core.FromJSON PoolInformation where
  parseJSON =
    Core.withObject
      "PoolInformation"
      ( \x ->
          PoolInformation'
            Prelude.<$> (x Core..:? "TwoWayChannelArn")
            Prelude.<*> (x Core..: "PoolArn")
            Prelude.<*> (x Core..: "PoolId")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "MessageType")
            Prelude.<*> (x Core..: "TwoWayEnabled")
            Prelude.<*> (x Core..: "SelfManagedOptOutsEnabled")
            Prelude.<*> (x Core..: "OptOutListName")
            Prelude.<*> (x Core..: "SharedRoutesEnabled")
            Prelude.<*> (x Core..: "DeletionProtectionEnabled")
            Prelude.<*> (x Core..: "CreatedTimestamp")
      )

instance Prelude.Hashable PoolInformation where
  hashWithSalt _salt PoolInformation' {..} =
    _salt `Prelude.hashWithSalt` twoWayChannelArn
      `Prelude.hashWithSalt` poolArn
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` twoWayEnabled
      `Prelude.hashWithSalt` selfManagedOptOutsEnabled
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` sharedRoutesEnabled
      `Prelude.hashWithSalt` deletionProtectionEnabled
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData PoolInformation where
  rnf PoolInformation' {..} =
    Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf poolArn
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf sharedRoutesEnabled
      `Prelude.seq` Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf createdTimestamp
