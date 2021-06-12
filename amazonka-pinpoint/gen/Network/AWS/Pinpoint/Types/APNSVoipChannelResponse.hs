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
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) VoIP channel for an application.
--
-- /See:/ 'newAPNSVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { -- | The date and time when the APNs VoIP channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the application that the APNs VoIP channel
    -- applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs VoIP channel is configured to communicate
    -- with APNs by using APNs tokens. To provide an authentication key for
    -- APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs VoIP channel. This property is
    -- retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | The date and time when the APNs VoIP channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs VoIP channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The current version of the APNs VoIP channel.
    version :: Core.Maybe Core.Int,
    -- | Specifies whether the APNs VoIP channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs VoIP channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs VoIP channel, this value is APNS_VOIP.
    platform :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'APNSVoipChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'aPNSVoipChannelResponse_lastModifiedDate' - The date and time when the APNs VoIP channel was last modified.
--
-- 'applicationId', 'aPNSVoipChannelResponse_applicationId' - The unique identifier for the application that the APNs VoIP channel
-- applies to.
--
-- 'defaultAuthenticationMethod', 'aPNSVoipChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
--
-- 'hasCredential', 'aPNSVoipChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'hasTokenKey', 'aPNSVoipChannelResponse_hasTokenKey' - Specifies whether the APNs VoIP channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
--
-- 'id', 'aPNSVoipChannelResponse_id' - (Deprecated) An identifier for the APNs VoIP channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'aPNSVoipChannelResponse_creationDate' - The date and time when the APNs VoIP channel was enabled.
--
-- 'enabled', 'aPNSVoipChannelResponse_enabled' - Specifies whether the APNs VoIP channel is enabled for the application.
--
-- 'version', 'aPNSVoipChannelResponse_version' - The current version of the APNs VoIP channel.
--
-- 'isArchived', 'aPNSVoipChannelResponse_isArchived' - Specifies whether the APNs VoIP channel is archived.
--
-- 'lastModifiedBy', 'aPNSVoipChannelResponse_lastModifiedBy' - The user who last modified the APNs VoIP channel.
--
-- 'platform', 'aPNSVoipChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs VoIP channel, this value is APNS_VOIP.
newAPNSVoipChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSVoipChannelResponse
newAPNSVoipChannelResponse pPlatform_ =
  APNSVoipChannelResponse'
    { lastModifiedDate =
        Core.Nothing,
      applicationId = Core.Nothing,
      defaultAuthenticationMethod = Core.Nothing,
      hasCredential = Core.Nothing,
      hasTokenKey = Core.Nothing,
      id = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      version = Core.Nothing,
      isArchived = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      platform = pPlatform_
    }

-- | The date and time when the APNs VoIP channel was last modified.
aPNSVoipChannelResponse_lastModifiedDate :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
aPNSVoipChannelResponse_lastModifiedDate = Lens.lens (\APNSVoipChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSVoipChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSVoipChannelResponse)

-- | The unique identifier for the application that the APNs VoIP channel
-- applies to.
aPNSVoipChannelResponse_applicationId :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
aPNSVoipChannelResponse_applicationId = Lens.lens (\APNSVoipChannelResponse' {applicationId} -> applicationId) (\s@APNSVoipChannelResponse' {} a -> s {applicationId = a} :: APNSVoipChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
aPNSVoipChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
aPNSVoipChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSVoipChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSVoipChannelResponse_hasCredential :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
aPNSVoipChannelResponse_hasCredential = Lens.lens (\APNSVoipChannelResponse' {hasCredential} -> hasCredential) (\s@APNSVoipChannelResponse' {} a -> s {hasCredential = a} :: APNSVoipChannelResponse)

-- | Specifies whether the APNs VoIP channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
aPNSVoipChannelResponse_hasTokenKey :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
aPNSVoipChannelResponse_hasTokenKey = Lens.lens (\APNSVoipChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSVoipChannelResponse' {} a -> s {hasTokenKey = a} :: APNSVoipChannelResponse)

-- | (Deprecated) An identifier for the APNs VoIP channel. This property is
-- retained only for backward compatibility.
aPNSVoipChannelResponse_id :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
aPNSVoipChannelResponse_id = Lens.lens (\APNSVoipChannelResponse' {id} -> id) (\s@APNSVoipChannelResponse' {} a -> s {id = a} :: APNSVoipChannelResponse)

-- | The date and time when the APNs VoIP channel was enabled.
aPNSVoipChannelResponse_creationDate :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
aPNSVoipChannelResponse_creationDate = Lens.lens (\APNSVoipChannelResponse' {creationDate} -> creationDate) (\s@APNSVoipChannelResponse' {} a -> s {creationDate = a} :: APNSVoipChannelResponse)

-- | Specifies whether the APNs VoIP channel is enabled for the application.
aPNSVoipChannelResponse_enabled :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
aPNSVoipChannelResponse_enabled = Lens.lens (\APNSVoipChannelResponse' {enabled} -> enabled) (\s@APNSVoipChannelResponse' {} a -> s {enabled = a} :: APNSVoipChannelResponse)

-- | The current version of the APNs VoIP channel.
aPNSVoipChannelResponse_version :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Int)
aPNSVoipChannelResponse_version = Lens.lens (\APNSVoipChannelResponse' {version} -> version) (\s@APNSVoipChannelResponse' {} a -> s {version = a} :: APNSVoipChannelResponse)

-- | Specifies whether the APNs VoIP channel is archived.
aPNSVoipChannelResponse_isArchived :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Bool)
aPNSVoipChannelResponse_isArchived = Lens.lens (\APNSVoipChannelResponse' {isArchived} -> isArchived) (\s@APNSVoipChannelResponse' {} a -> s {isArchived = a} :: APNSVoipChannelResponse)

-- | The user who last modified the APNs VoIP channel.
aPNSVoipChannelResponse_lastModifiedBy :: Lens.Lens' APNSVoipChannelResponse (Core.Maybe Core.Text)
aPNSVoipChannelResponse_lastModifiedBy = Lens.lens (\APNSVoipChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSVoipChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSVoipChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs VoIP channel, this value is APNS_VOIP.
aPNSVoipChannelResponse_platform :: Lens.Lens' APNSVoipChannelResponse Core.Text
aPNSVoipChannelResponse_platform = Lens.lens (\APNSVoipChannelResponse' {platform} -> platform) (\s@APNSVoipChannelResponse' {} a -> s {platform = a} :: APNSVoipChannelResponse)

instance Core.FromJSON APNSVoipChannelResponse where
  parseJSON =
    Core.withObject
      "APNSVoipChannelResponse"
      ( \x ->
          APNSVoipChannelResponse'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "DefaultAuthenticationMethod")
            Core.<*> (x Core..:? "HasCredential")
            Core.<*> (x Core..:? "HasTokenKey")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "IsArchived")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..: "Platform")
      )

instance Core.Hashable APNSVoipChannelResponse

instance Core.NFData APNSVoipChannelResponse
