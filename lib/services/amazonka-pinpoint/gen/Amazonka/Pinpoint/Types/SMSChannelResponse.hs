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
-- Module      : Amazonka.Pinpoint.Types.SMSChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SMSChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the SMS channel
-- for an application.
--
-- /See:/ 'newSMSChannelResponse' smart constructor.
data SMSChannelResponse = SMSChannelResponse'
  { -- | The registered short code to use when you send messages through the SMS
    -- channel.
    shortCode :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was last
    -- modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the SMS channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The identity that displays on recipients\' devices when they receive
    -- messages from the SMS channel.
    senderId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of transactional messages that you can send through
    -- the SMS channel each second.
    transactionalMessagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of promotional messages that you can send through the
    -- SMS channel each second.
    promotionalMessagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the SMS channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the application that the SMS channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The current version of the SMS channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | (Deprecated) An identifier for the SMS channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The user who last modified the SMS channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | The type of messaging or notification platform for the channel. For the
    -- SMS channel, this value is SMS.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMSChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shortCode', 'sMSChannelResponse_shortCode' - The registered short code to use when you send messages through the SMS
-- channel.
--
-- 'lastModifiedDate', 'sMSChannelResponse_lastModifiedDate' - The date and time, in ISO 8601 format, when the SMS channel was last
-- modified.
--
-- 'enabled', 'sMSChannelResponse_enabled' - Specifies whether the SMS channel is enabled for the application.
--
-- 'senderId', 'sMSChannelResponse_senderId' - The identity that displays on recipients\' devices when they receive
-- messages from the SMS channel.
--
-- 'transactionalMessagesPerSecond', 'sMSChannelResponse_transactionalMessagesPerSecond' - The maximum number of transactional messages that you can send through
-- the SMS channel each second.
--
-- 'promotionalMessagesPerSecond', 'sMSChannelResponse_promotionalMessagesPerSecond' - The maximum number of promotional messages that you can send through the
-- SMS channel each second.
--
-- 'isArchived', 'sMSChannelResponse_isArchived' - Specifies whether the SMS channel is archived.
--
-- 'applicationId', 'sMSChannelResponse_applicationId' - The unique identifier for the application that the SMS channel applies
-- to.
--
-- 'version', 'sMSChannelResponse_version' - The current version of the SMS channel.
--
-- 'id', 'sMSChannelResponse_id' - (Deprecated) An identifier for the SMS channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'sMSChannelResponse_creationDate' - The date and time, in ISO 8601 format, when the SMS channel was enabled.
--
-- 'lastModifiedBy', 'sMSChannelResponse_lastModifiedBy' - The user who last modified the SMS channel.
--
-- 'hasCredential', 'sMSChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'platform', 'sMSChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- SMS channel, this value is SMS.
newSMSChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  SMSChannelResponse
newSMSChannelResponse pPlatform_ =
  SMSChannelResponse'
    { shortCode = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      senderId = Prelude.Nothing,
      transactionalMessagesPerSecond = Prelude.Nothing,
      promotionalMessagesPerSecond = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      version = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The registered short code to use when you send messages through the SMS
-- channel.
sMSChannelResponse_shortCode :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_shortCode = Lens.lens (\SMSChannelResponse' {shortCode} -> shortCode) (\s@SMSChannelResponse' {} a -> s {shortCode = a} :: SMSChannelResponse)

-- | The date and time, in ISO 8601 format, when the SMS channel was last
-- modified.
sMSChannelResponse_lastModifiedDate :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_lastModifiedDate = Lens.lens (\SMSChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@SMSChannelResponse' {} a -> s {lastModifiedDate = a} :: SMSChannelResponse)

-- | Specifies whether the SMS channel is enabled for the application.
sMSChannelResponse_enabled :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Bool)
sMSChannelResponse_enabled = Lens.lens (\SMSChannelResponse' {enabled} -> enabled) (\s@SMSChannelResponse' {} a -> s {enabled = a} :: SMSChannelResponse)

-- | The identity that displays on recipients\' devices when they receive
-- messages from the SMS channel.
sMSChannelResponse_senderId :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_senderId = Lens.lens (\SMSChannelResponse' {senderId} -> senderId) (\s@SMSChannelResponse' {} a -> s {senderId = a} :: SMSChannelResponse)

-- | The maximum number of transactional messages that you can send through
-- the SMS channel each second.
sMSChannelResponse_transactionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Int)
sMSChannelResponse_transactionalMessagesPerSecond = Lens.lens (\SMSChannelResponse' {transactionalMessagesPerSecond} -> transactionalMessagesPerSecond) (\s@SMSChannelResponse' {} a -> s {transactionalMessagesPerSecond = a} :: SMSChannelResponse)

-- | The maximum number of promotional messages that you can send through the
-- SMS channel each second.
sMSChannelResponse_promotionalMessagesPerSecond :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Int)
sMSChannelResponse_promotionalMessagesPerSecond = Lens.lens (\SMSChannelResponse' {promotionalMessagesPerSecond} -> promotionalMessagesPerSecond) (\s@SMSChannelResponse' {} a -> s {promotionalMessagesPerSecond = a} :: SMSChannelResponse)

-- | Specifies whether the SMS channel is archived.
sMSChannelResponse_isArchived :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Bool)
sMSChannelResponse_isArchived = Lens.lens (\SMSChannelResponse' {isArchived} -> isArchived) (\s@SMSChannelResponse' {} a -> s {isArchived = a} :: SMSChannelResponse)

-- | The unique identifier for the application that the SMS channel applies
-- to.
sMSChannelResponse_applicationId :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_applicationId = Lens.lens (\SMSChannelResponse' {applicationId} -> applicationId) (\s@SMSChannelResponse' {} a -> s {applicationId = a} :: SMSChannelResponse)

-- | The current version of the SMS channel.
sMSChannelResponse_version :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Int)
sMSChannelResponse_version = Lens.lens (\SMSChannelResponse' {version} -> version) (\s@SMSChannelResponse' {} a -> s {version = a} :: SMSChannelResponse)

-- | (Deprecated) An identifier for the SMS channel. This property is
-- retained only for backward compatibility.
sMSChannelResponse_id :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_id = Lens.lens (\SMSChannelResponse' {id} -> id) (\s@SMSChannelResponse' {} a -> s {id = a} :: SMSChannelResponse)

-- | The date and time, in ISO 8601 format, when the SMS channel was enabled.
sMSChannelResponse_creationDate :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_creationDate = Lens.lens (\SMSChannelResponse' {creationDate} -> creationDate) (\s@SMSChannelResponse' {} a -> s {creationDate = a} :: SMSChannelResponse)

-- | The user who last modified the SMS channel.
sMSChannelResponse_lastModifiedBy :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Text)
sMSChannelResponse_lastModifiedBy = Lens.lens (\SMSChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@SMSChannelResponse' {} a -> s {lastModifiedBy = a} :: SMSChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
sMSChannelResponse_hasCredential :: Lens.Lens' SMSChannelResponse (Prelude.Maybe Prelude.Bool)
sMSChannelResponse_hasCredential = Lens.lens (\SMSChannelResponse' {hasCredential} -> hasCredential) (\s@SMSChannelResponse' {} a -> s {hasCredential = a} :: SMSChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- SMS channel, this value is SMS.
sMSChannelResponse_platform :: Lens.Lens' SMSChannelResponse Prelude.Text
sMSChannelResponse_platform = Lens.lens (\SMSChannelResponse' {platform} -> platform) (\s@SMSChannelResponse' {} a -> s {platform = a} :: SMSChannelResponse)

instance Core.FromJSON SMSChannelResponse where
  parseJSON =
    Core.withObject
      "SMSChannelResponse"
      ( \x ->
          SMSChannelResponse'
            Prelude.<$> (x Core..:? "ShortCode")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "SenderId")
            Prelude.<*> (x Core..:? "TransactionalMessagesPerSecond")
            Prelude.<*> (x Core..:? "PromotionalMessagesPerSecond")
            Prelude.<*> (x Core..:? "IsArchived")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "HasCredential")
            Prelude.<*> (x Core..: "Platform")
      )

instance Prelude.Hashable SMSChannelResponse

instance Prelude.NFData SMSChannelResponse
