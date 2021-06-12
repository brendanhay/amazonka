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
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the status and settings of the GCM channel
-- for an application. The GCM channel enables Amazon Pinpoint to send push
-- notifications through the Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), service.
--
-- /See:/ 'newGCMChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
  { -- | The date and time when the GCM channel was last modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the application that the GCM channel applies
    -- to.
    applicationId :: Core.Maybe Core.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | (Deprecated) An identifier for the GCM channel. This property is
    -- retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | The date and time when the GCM channel was enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the GCM channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The current version of the GCM channel.
    version :: Core.Maybe Core.Int,
    -- | Specifies whether the GCM channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The user who last modified the GCM channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The Web API Key, also referred to as an /API_KEY/ or /server key/, that
    -- you received from Google to communicate with Google services.
    credential :: Core.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- GCM channel, this value is GCM.
    platform :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GCMChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'gCMChannelResponse_lastModifiedDate' - The date and time when the GCM channel was last modified.
--
-- 'applicationId', 'gCMChannelResponse_applicationId' - The unique identifier for the application that the GCM channel applies
-- to.
--
-- 'hasCredential', 'gCMChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'gCMChannelResponse_id' - (Deprecated) An identifier for the GCM channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'gCMChannelResponse_creationDate' - The date and time when the GCM channel was enabled.
--
-- 'enabled', 'gCMChannelResponse_enabled' - Specifies whether the GCM channel is enabled for the application.
--
-- 'version', 'gCMChannelResponse_version' - The current version of the GCM channel.
--
-- 'isArchived', 'gCMChannelResponse_isArchived' - Specifies whether the GCM channel is archived.
--
-- 'lastModifiedBy', 'gCMChannelResponse_lastModifiedBy' - The user who last modified the GCM channel.
--
-- 'credential', 'gCMChannelResponse_credential' - The Web API Key, also referred to as an /API_KEY/ or /server key/, that
-- you received from Google to communicate with Google services.
--
-- 'platform', 'gCMChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- GCM channel, this value is GCM.
newGCMChannelResponse ::
  -- | 'credential'
  Core.Text ->
  -- | 'platform'
  Core.Text ->
  GCMChannelResponse
newGCMChannelResponse pCredential_ pPlatform_ =
  GCMChannelResponse'
    { lastModifiedDate =
        Core.Nothing,
      applicationId = Core.Nothing,
      hasCredential = Core.Nothing,
      id = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      version = Core.Nothing,
      isArchived = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      credential = pCredential_,
      platform = pPlatform_
    }

-- | The date and time when the GCM channel was last modified.
gCMChannelResponse_lastModifiedDate :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gCMChannelResponse_lastModifiedDate = Lens.lens (\GCMChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@GCMChannelResponse' {} a -> s {lastModifiedDate = a} :: GCMChannelResponse)

-- | The unique identifier for the application that the GCM channel applies
-- to.
gCMChannelResponse_applicationId :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gCMChannelResponse_applicationId = Lens.lens (\GCMChannelResponse' {applicationId} -> applicationId) (\s@GCMChannelResponse' {} a -> s {applicationId = a} :: GCMChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
gCMChannelResponse_hasCredential :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Bool)
gCMChannelResponse_hasCredential = Lens.lens (\GCMChannelResponse' {hasCredential} -> hasCredential) (\s@GCMChannelResponse' {} a -> s {hasCredential = a} :: GCMChannelResponse)

-- | (Deprecated) An identifier for the GCM channel. This property is
-- retained only for backward compatibility.
gCMChannelResponse_id :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gCMChannelResponse_id = Lens.lens (\GCMChannelResponse' {id} -> id) (\s@GCMChannelResponse' {} a -> s {id = a} :: GCMChannelResponse)

-- | The date and time when the GCM channel was enabled.
gCMChannelResponse_creationDate :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gCMChannelResponse_creationDate = Lens.lens (\GCMChannelResponse' {creationDate} -> creationDate) (\s@GCMChannelResponse' {} a -> s {creationDate = a} :: GCMChannelResponse)

-- | Specifies whether the GCM channel is enabled for the application.
gCMChannelResponse_enabled :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Bool)
gCMChannelResponse_enabled = Lens.lens (\GCMChannelResponse' {enabled} -> enabled) (\s@GCMChannelResponse' {} a -> s {enabled = a} :: GCMChannelResponse)

-- | The current version of the GCM channel.
gCMChannelResponse_version :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Int)
gCMChannelResponse_version = Lens.lens (\GCMChannelResponse' {version} -> version) (\s@GCMChannelResponse' {} a -> s {version = a} :: GCMChannelResponse)

-- | Specifies whether the GCM channel is archived.
gCMChannelResponse_isArchived :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Bool)
gCMChannelResponse_isArchived = Lens.lens (\GCMChannelResponse' {isArchived} -> isArchived) (\s@GCMChannelResponse' {} a -> s {isArchived = a} :: GCMChannelResponse)

-- | The user who last modified the GCM channel.
gCMChannelResponse_lastModifiedBy :: Lens.Lens' GCMChannelResponse (Core.Maybe Core.Text)
gCMChannelResponse_lastModifiedBy = Lens.lens (\GCMChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@GCMChannelResponse' {} a -> s {lastModifiedBy = a} :: GCMChannelResponse)

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/, that
-- you received from Google to communicate with Google services.
gCMChannelResponse_credential :: Lens.Lens' GCMChannelResponse Core.Text
gCMChannelResponse_credential = Lens.lens (\GCMChannelResponse' {credential} -> credential) (\s@GCMChannelResponse' {} a -> s {credential = a} :: GCMChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- GCM channel, this value is GCM.
gCMChannelResponse_platform :: Lens.Lens' GCMChannelResponse Core.Text
gCMChannelResponse_platform = Lens.lens (\GCMChannelResponse' {platform} -> platform) (\s@GCMChannelResponse' {} a -> s {platform = a} :: GCMChannelResponse)

instance Core.FromJSON GCMChannelResponse where
  parseJSON =
    Core.withObject
      "GCMChannelResponse"
      ( \x ->
          GCMChannelResponse'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "HasCredential")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "IsArchived")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..: "Credential")
            Core.<*> (x Core..: "Platform")
      )

instance Core.Hashable GCMChannelResponse

instance Core.NFData GCMChannelResponse
