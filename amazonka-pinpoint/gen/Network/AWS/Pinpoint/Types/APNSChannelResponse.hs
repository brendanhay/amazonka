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
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) channel for an application.
--
-- /See:/ 'newAPNSChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { -- | The date and time when the APNs channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the application that the APNs channel applies
    -- to.
    applicationId :: Core.Maybe Core.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Core.Maybe Core.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | Specifies whether the APNs channel is configured to communicate with
    -- APNs by using APNs tokens. To provide an authentication key for APNs
    -- tokens, set the TokenKey property of the channel.
    hasTokenKey :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the APNs channel. This property is
    -- retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | The date and time when the APNs channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the APNs channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The current version of the APNs channel.
    version :: Core.Maybe Core.Int,
    -- | Specifies whether the APNs channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the APNs channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs channel, this value is APNS.
    platform :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'APNSChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'aPNSChannelResponse_lastModifiedDate' - The date and time when the APNs channel was last modified.
--
-- 'applicationId', 'aPNSChannelResponse_applicationId' - The unique identifier for the application that the APNs channel applies
-- to.
--
-- 'defaultAuthenticationMethod', 'aPNSChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
--
-- 'hasCredential', 'aPNSChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'hasTokenKey', 'aPNSChannelResponse_hasTokenKey' - Specifies whether the APNs channel is configured to communicate with
-- APNs by using APNs tokens. To provide an authentication key for APNs
-- tokens, set the TokenKey property of the channel.
--
-- 'id', 'aPNSChannelResponse_id' - (Deprecated) An identifier for the APNs channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'aPNSChannelResponse_creationDate' - The date and time when the APNs channel was enabled.
--
-- 'enabled', 'aPNSChannelResponse_enabled' - Specifies whether the APNs channel is enabled for the application.
--
-- 'version', 'aPNSChannelResponse_version' - The current version of the APNs channel.
--
-- 'isArchived', 'aPNSChannelResponse_isArchived' - Specifies whether the APNs channel is archived.
--
-- 'lastModifiedBy', 'aPNSChannelResponse_lastModifiedBy' - The user who last modified the APNs channel.
--
-- 'platform', 'aPNSChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs channel, this value is APNS.
newAPNSChannelResponse ::
  -- | 'platform'
  Core.Text ->
  APNSChannelResponse
newAPNSChannelResponse pPlatform_ =
  APNSChannelResponse'
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

-- | The date and time when the APNs channel was last modified.
aPNSChannelResponse_lastModifiedDate :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
aPNSChannelResponse_lastModifiedDate = Lens.lens (\APNSChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSChannelResponse)

-- | The unique identifier for the application that the APNs channel applies
-- to.
aPNSChannelResponse_applicationId :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
aPNSChannelResponse_applicationId = Lens.lens (\APNSChannelResponse' {applicationId} -> applicationId) (\s@APNSChannelResponse' {} a -> s {applicationId = a} :: APNSChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
aPNSChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
aPNSChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSChannelResponse_hasCredential :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
aPNSChannelResponse_hasCredential = Lens.lens (\APNSChannelResponse' {hasCredential} -> hasCredential) (\s@APNSChannelResponse' {} a -> s {hasCredential = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is configured to communicate with
-- APNs by using APNs tokens. To provide an authentication key for APNs
-- tokens, set the TokenKey property of the channel.
aPNSChannelResponse_hasTokenKey :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
aPNSChannelResponse_hasTokenKey = Lens.lens (\APNSChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSChannelResponse' {} a -> s {hasTokenKey = a} :: APNSChannelResponse)

-- | (Deprecated) An identifier for the APNs channel. This property is
-- retained only for backward compatibility.
aPNSChannelResponse_id :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
aPNSChannelResponse_id = Lens.lens (\APNSChannelResponse' {id} -> id) (\s@APNSChannelResponse' {} a -> s {id = a} :: APNSChannelResponse)

-- | The date and time when the APNs channel was enabled.
aPNSChannelResponse_creationDate :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
aPNSChannelResponse_creationDate = Lens.lens (\APNSChannelResponse' {creationDate} -> creationDate) (\s@APNSChannelResponse' {} a -> s {creationDate = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is enabled for the application.
aPNSChannelResponse_enabled :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
aPNSChannelResponse_enabled = Lens.lens (\APNSChannelResponse' {enabled} -> enabled) (\s@APNSChannelResponse' {} a -> s {enabled = a} :: APNSChannelResponse)

-- | The current version of the APNs channel.
aPNSChannelResponse_version :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Int)
aPNSChannelResponse_version = Lens.lens (\APNSChannelResponse' {version} -> version) (\s@APNSChannelResponse' {} a -> s {version = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is archived.
aPNSChannelResponse_isArchived :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Bool)
aPNSChannelResponse_isArchived = Lens.lens (\APNSChannelResponse' {isArchived} -> isArchived) (\s@APNSChannelResponse' {} a -> s {isArchived = a} :: APNSChannelResponse)

-- | The user who last modified the APNs channel.
aPNSChannelResponse_lastModifiedBy :: Lens.Lens' APNSChannelResponse (Core.Maybe Core.Text)
aPNSChannelResponse_lastModifiedBy = Lens.lens (\APNSChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs channel, this value is APNS.
aPNSChannelResponse_platform :: Lens.Lens' APNSChannelResponse Core.Text
aPNSChannelResponse_platform = Lens.lens (\APNSChannelResponse' {platform} -> platform) (\s@APNSChannelResponse' {} a -> s {platform = a} :: APNSChannelResponse)

instance Core.FromJSON APNSChannelResponse where
  parseJSON =
    Core.withObject
      "APNSChannelResponse"
      ( \x ->
          APNSChannelResponse'
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

instance Core.Hashable APNSChannelResponse

instance Core.NFData APNSChannelResponse
