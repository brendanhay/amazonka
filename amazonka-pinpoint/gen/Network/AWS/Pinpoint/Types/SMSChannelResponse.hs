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
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSChannelResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the status and settings of the SMS channel
-- for an application.
--
-- /See:/ 'newSMSChannelResponse' smart constructor.
data SMSChannelResponse = SMSChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the SMS channel was last
    -- modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the application that the SMS channel applies
    -- to.
    applicationId :: Core.Maybe Core.Text,
    -- | The maximum number of promotional messages that you can send through the
    -- SMS channel each second.
    promotionalMessagesPerSecond :: Core.Maybe Core.Int,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the SMS channel. This property is
    -- retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the SMS channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The current version of the SMS channel.
    version :: Core.Maybe Core.Int,
    -- | The registered short code to use when you send messages through the SMS
    -- channel.
    shortCode :: Core.Maybe Core.Text,
    -- | Specifies whether the SMS channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The identity that displays on recipients\' devices when they receive
    -- messages from the SMS channel.
    senderId :: Core.Maybe Core.Text,
    -- | The maximum number of transactional messages that you can send through
    -- the SMS channel each second.
    transactionalMessagesPerSecond :: Core.Maybe Core.Int,
    -- | The user who last modified the SMS channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- SMS channel, this value is SMS.
    platform :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SMSChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'sMSChannelResponse_lastModifiedDate' - The date and time, in ISO 8601 format, when the SMS channel was last
-- modified.
--
-- 'applicationId', 'sMSChannelResponse_applicationId' - The unique identifier for the application that the SMS channel applies
-- to.
--
-- 'promotionalMessagesPerSecond', 'sMSChannelResponse_promotionalMessagesPerSecond' - The maximum number of promotional messages that you can send through the
-- SMS channel each second.
--
-- 'hasCredential', 'sMSChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'sMSChannelResponse_id' - (Deprecated) An identifier for the SMS channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'sMSChannelResponse_creationDate' - The date and time, in ISO 8601 format, when the SMS channel was enabled.
--
-- 'enabled', 'sMSChannelResponse_enabled' - Specifies whether the SMS channel is enabled for the application.
--
-- 'version', 'sMSChannelResponse_version' - The current version of the SMS channel.
--
-- 'shortCode', 'sMSChannelResponse_shortCode' - The registered short code to use when you send messages through the SMS
-- channel.
--
-- 'isArchived', 'sMSChannelResponse_isArchived' - Specifies whether the SMS channel is archived.
--
-- 'senderId', 'sMSChannelResponse_senderId' - The identity that displays on recipients\' devices when they receive
-- messages from the SMS channel.
--
-- 'transactionalMessagesPerSecond', 'sMSChannelResponse_transactionalMessagesPerSecond' - The maximum number of transactional messages that you can send through
-- the SMS channel each second.
--
-- 'lastModifiedBy', 'sMSChannelResponse_lastModifiedBy' - The user who last modified the SMS channel.
--
-- 'platform', 'sMSChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- SMS channel, this value is SMS.
newSMSChannelResponse ::
  -- | 'platform'
  Core.Text ->
  SMSChannelResponse
newSMSChannelResponse pPlatform_ =
  SMSChannelResponse'
    { lastModifiedDate =
        Core.Nothing,
      applicationId = Core.Nothing,
      promotionalMessagesPerSecond = Core.Nothing,
      hasCredential = Core.Nothing,
      id = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      version = Core.Nothing,
      shortCode = Core.Nothing,
      isArchived = Core.Nothing,
      senderId = Core.Nothing,
      transactionalMessagesPerSecond = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      platform = pPlatform_
    }

-- | The date and time, in ISO 8601 format, when the SMS channel was last
-- modified.
sMSChannelResponse_lastModifiedDate :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_lastModifiedDate = Lens.lens (\SMSChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@SMSChannelResponse' {} a -> s {lastModifiedDate = a} :: SMSChannelResponse)

-- | The unique identifier for the application that the SMS channel applies
-- to.
sMSChannelResponse_applicationId :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_applicationId = Lens.lens (\SMSChannelResponse' {applicationId} -> applicationId) (\s@SMSChannelResponse' {} a -> s {applicationId = a} :: SMSChannelResponse)

-- | The maximum number of promotional messages that you can send through the
-- SMS channel each second.
sMSChannelResponse_promotionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Int)
sMSChannelResponse_promotionalMessagesPerSecond = Lens.lens (\SMSChannelResponse' {promotionalMessagesPerSecond} -> promotionalMessagesPerSecond) (\s@SMSChannelResponse' {} a -> s {promotionalMessagesPerSecond = a} :: SMSChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
sMSChannelResponse_hasCredential :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Bool)
sMSChannelResponse_hasCredential = Lens.lens (\SMSChannelResponse' {hasCredential} -> hasCredential) (\s@SMSChannelResponse' {} a -> s {hasCredential = a} :: SMSChannelResponse)

-- | (Deprecated) An identifier for the SMS channel. This property is
-- retained only for backward compatibility.
sMSChannelResponse_id :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_id = Lens.lens (\SMSChannelResponse' {id} -> id) (\s@SMSChannelResponse' {} a -> s {id = a} :: SMSChannelResponse)

-- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
sMSChannelResponse_creationDate :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_creationDate = Lens.lens (\SMSChannelResponse' {creationDate} -> creationDate) (\s@SMSChannelResponse' {} a -> s {creationDate = a} :: SMSChannelResponse)

-- | Specifies whether the SMS channel is enabled for the application.
sMSChannelResponse_enabled :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Bool)
sMSChannelResponse_enabled = Lens.lens (\SMSChannelResponse' {enabled} -> enabled) (\s@SMSChannelResponse' {} a -> s {enabled = a} :: SMSChannelResponse)

-- | The current version of the SMS channel.
sMSChannelResponse_version :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Int)
sMSChannelResponse_version = Lens.lens (\SMSChannelResponse' {version} -> version) (\s@SMSChannelResponse' {} a -> s {version = a} :: SMSChannelResponse)

-- | The registered short code to use when you send messages through the SMS
-- channel.
sMSChannelResponse_shortCode :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_shortCode = Lens.lens (\SMSChannelResponse' {shortCode} -> shortCode) (\s@SMSChannelResponse' {} a -> s {shortCode = a} :: SMSChannelResponse)

-- | Specifies whether the SMS channel is archived.
sMSChannelResponse_isArchived :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Bool)
sMSChannelResponse_isArchived = Lens.lens (\SMSChannelResponse' {isArchived} -> isArchived) (\s@SMSChannelResponse' {} a -> s {isArchived = a} :: SMSChannelResponse)

-- | The identity that displays on recipients\' devices when they receive
-- messages from the SMS channel.
sMSChannelResponse_senderId :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_senderId = Lens.lens (\SMSChannelResponse' {senderId} -> senderId) (\s@SMSChannelResponse' {} a -> s {senderId = a} :: SMSChannelResponse)

-- | The maximum number of transactional messages that you can send through
-- the SMS channel each second.
sMSChannelResponse_transactionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Int)
sMSChannelResponse_transactionalMessagesPerSecond = Lens.lens (\SMSChannelResponse' {transactionalMessagesPerSecond} -> transactionalMessagesPerSecond) (\s@SMSChannelResponse' {} a -> s {transactionalMessagesPerSecond = a} :: SMSChannelResponse)

-- | The user who last modified the SMS channel.
sMSChannelResponse_lastModifiedBy :: Lens.Lens' SMSChannelResponse (Core.Maybe Core.Text)
sMSChannelResponse_lastModifiedBy = Lens.lens (\SMSChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@SMSChannelResponse' {} a -> s {lastModifiedBy = a} :: SMSChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- SMS channel, this value is SMS.
sMSChannelResponse_platform :: Lens.Lens' SMSChannelResponse Core.Text
sMSChannelResponse_platform = Lens.lens (\SMSChannelResponse' {platform} -> platform) (\s@SMSChannelResponse' {} a -> s {platform = a} :: SMSChannelResponse)

instance Core.FromJSON SMSChannelResponse where
  parseJSON =
    Core.withObject
      "SMSChannelResponse"
      ( \x ->
          SMSChannelResponse'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "PromotionalMessagesPerSecond")
            Core.<*> (x Core..:? "HasCredential")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "ShortCode")
            Core.<*> (x Core..:? "IsArchived")
            Core.<*> (x Core..:? "SenderId")
            Core.<*> (x Core..:? "TransactionalMessagesPerSecond")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..: "Platform")
      )

instance Core.Hashable SMSChannelResponse

instance Core.NFData SMSChannelResponse
