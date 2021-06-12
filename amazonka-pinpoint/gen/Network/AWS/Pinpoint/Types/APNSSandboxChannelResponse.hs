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
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) sandbox channel for an application.
--
-- /See:/ 'newAPNSSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { -- | The date and time when the APNs sandbox channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the application that the APNs sandbox channel
    -- applies to.
    applicationId :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with the APNs sandbox environment for this channel, key or
    -- certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs sandbox channel is configured to communicate
    -- with APNs by using APNs tokens. To provide an authentication key for
    -- APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs sandbox channel. This property
    -- is retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | The date and time when the APNs sandbox channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs sandbox channel is enabled for the
    -- application.
    enabled :: Core.Maybe Core.Bool,
    -- | The current version of the APNs sandbox channel.
    version :: Core.Maybe Core.Int,
    -- | Specifies whether the APNs sandbox channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs sandbox channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs sandbox channel, this value is APNS_SANDBOX.
    platform :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'APNSSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'aPNSSandboxChannelResponse_lastModifiedDate' - The date and time when the APNs sandbox channel was last modified.
--
-- 'applicationId', 'aPNSSandboxChannelResponse_applicationId' - The unique identifier for the application that the APNs sandbox channel
-- applies to.
--
-- 'defaultAuthenticationMethod', 'aPNSSandboxChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
--
-- 'hasCredential', 'aPNSSandboxChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'hasTokenKey', 'aPNSSandboxChannelResponse_hasTokenKey' - Specifies whether the APNs sandbox channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
--
-- 'id', 'aPNSSandboxChannelResponse_id' - (Deprecated) An identifier for the APNs sandbox channel. This property
-- is retained only for backward compatibility.
--
-- 'creationDate', 'aPNSSandboxChannelResponse_creationDate' - The date and time when the APNs sandbox channel was enabled.
--
-- 'enabled', 'aPNSSandboxChannelResponse_enabled' - Specifies whether the APNs sandbox channel is enabled for the
-- application.
--
-- 'version', 'aPNSSandboxChannelResponse_version' - The current version of the APNs sandbox channel.
--
-- 'isArchived', 'aPNSSandboxChannelResponse_isArchived' - Specifies whether the APNs sandbox channel is archived.
--
-- 'lastModifiedBy', 'aPNSSandboxChannelResponse_lastModifiedBy' - The user who last modified the APNs sandbox channel.
--
-- 'platform', 'aPNSSandboxChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs sandbox channel, this value is APNS_SANDBOX.
newAPNSSandboxChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSSandboxChannelResponse
newAPNSSandboxChannelResponse pPlatform_ =
  APNSSandboxChannelResponse'
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

-- | The date and time when the APNs sandbox channel was last modified.
aPNSSandboxChannelResponse_lastModifiedDate :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
aPNSSandboxChannelResponse_lastModifiedDate = Lens.lens (\APNSSandboxChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSSandboxChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSSandboxChannelResponse)

-- | The unique identifier for the application that the APNs sandbox channel
-- applies to.
aPNSSandboxChannelResponse_applicationId :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
aPNSSandboxChannelResponse_applicationId = Lens.lens (\APNSSandboxChannelResponse' {applicationId} -> applicationId) (\s@APNSSandboxChannelResponse' {} a -> s {applicationId = a} :: APNSSandboxChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
aPNSSandboxChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
aPNSSandboxChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSSandboxChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSSandboxChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSSandboxChannelResponse_hasCredential :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
aPNSSandboxChannelResponse_hasCredential = Lens.lens (\APNSSandboxChannelResponse' {hasCredential} -> hasCredential) (\s@APNSSandboxChannelResponse' {} a -> s {hasCredential = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
aPNSSandboxChannelResponse_hasTokenKey :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
aPNSSandboxChannelResponse_hasTokenKey = Lens.lens (\APNSSandboxChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSSandboxChannelResponse' {} a -> s {hasTokenKey = a} :: APNSSandboxChannelResponse)

-- | (Deprecated) An identifier for the APNs sandbox channel. This property
-- is retained only for backward compatibility.
aPNSSandboxChannelResponse_id :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
aPNSSandboxChannelResponse_id = Lens.lens (\APNSSandboxChannelResponse' {id} -> id) (\s@APNSSandboxChannelResponse' {} a -> s {id = a} :: APNSSandboxChannelResponse)

-- | The date and time when the APNs sandbox channel was enabled.
aPNSSandboxChannelResponse_creationDate :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
aPNSSandboxChannelResponse_creationDate = Lens.lens (\APNSSandboxChannelResponse' {creationDate} -> creationDate) (\s@APNSSandboxChannelResponse' {} a -> s {creationDate = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is enabled for the
-- application.
aPNSSandboxChannelResponse_enabled :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
aPNSSandboxChannelResponse_enabled = Lens.lens (\APNSSandboxChannelResponse' {enabled} -> enabled) (\s@APNSSandboxChannelResponse' {} a -> s {enabled = a} :: APNSSandboxChannelResponse)

-- | The current version of the APNs sandbox channel.
aPNSSandboxChannelResponse_version :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Int)
aPNSSandboxChannelResponse_version = Lens.lens (\APNSSandboxChannelResponse' {version} -> version) (\s@APNSSandboxChannelResponse' {} a -> s {version = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is archived.
aPNSSandboxChannelResponse_isArchived :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Bool)
aPNSSandboxChannelResponse_isArchived = Lens.lens (\APNSSandboxChannelResponse' {isArchived} -> isArchived) (\s@APNSSandboxChannelResponse' {} a -> s {isArchived = a} :: APNSSandboxChannelResponse)

-- | The user who last modified the APNs sandbox channel.
aPNSSandboxChannelResponse_lastModifiedBy :: Lens.Lens' APNSSandboxChannelResponse (Core.Maybe Core.Text)
aPNSSandboxChannelResponse_lastModifiedBy = Lens.lens (\APNSSandboxChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSSandboxChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSSandboxChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs sandbox channel, this value is APNS_SANDBOX.
aPNSSandboxChannelResponse_platform :: Lens.Lens' APNSSandboxChannelResponse Core.Text
aPNSSandboxChannelResponse_platform = Lens.lens (\APNSSandboxChannelResponse' {platform} -> platform) (\s@APNSSandboxChannelResponse' {} a -> s {platform = a} :: APNSSandboxChannelResponse)

instance Core.FromJSON APNSSandboxChannelResponse where
  parseJSON =
    Core.withObject
      "APNSSandboxChannelResponse"
      ( \x ->
          APNSSandboxChannelResponse'
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

instance Core.Hashable APNSSandboxChannelResponse

instance Core.NFData APNSSandboxChannelResponse
