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
-- Module      : Amazonka.Pinpoint.Types.VoiceChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.VoiceChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the voice channel
-- for an application.
--
-- /See:/ 'newVoiceChannelResponse' smart constructor.
data VoiceChannelResponse = VoiceChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the voice channel was last
    -- modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the voice channel was
    -- enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the voice channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the voice channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the voice channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the voice channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the application that the voice channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The current version of the voice channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The type of messaging or notification platform for the channel. For the
    -- voice channel, this value is VOICE.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'voiceChannelResponse_lastModifiedDate' - The date and time, in ISO 8601 format, when the voice channel was last
-- modified.
--
-- 'creationDate', 'voiceChannelResponse_creationDate' - The date and time, in ISO 8601 format, when the voice channel was
-- enabled.
--
-- 'hasCredential', 'voiceChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'voiceChannelResponse_id' - (Deprecated) An identifier for the voice channel. This property is
-- retained only for backward compatibility.
--
-- 'enabled', 'voiceChannelResponse_enabled' - Specifies whether the voice channel is enabled for the application.
--
-- 'lastModifiedBy', 'voiceChannelResponse_lastModifiedBy' - The user who last modified the voice channel.
--
-- 'isArchived', 'voiceChannelResponse_isArchived' - Specifies whether the voice channel is archived.
--
-- 'applicationId', 'voiceChannelResponse_applicationId' - The unique identifier for the application that the voice channel applies
-- to.
--
-- 'version', 'voiceChannelResponse_version' - The current version of the voice channel.
--
-- 'platform', 'voiceChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- voice channel, this value is VOICE.
newVoiceChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  VoiceChannelResponse
newVoiceChannelResponse pPlatform_ =
  VoiceChannelResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      id = Prelude.Nothing,
      enabled = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      version = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The date and time, in ISO 8601 format, when the voice channel was last
-- modified.
voiceChannelResponse_lastModifiedDate :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Text)
voiceChannelResponse_lastModifiedDate = Lens.lens (\VoiceChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@VoiceChannelResponse' {} a -> s {lastModifiedDate = a} :: VoiceChannelResponse)

-- | The date and time, in ISO 8601 format, when the voice channel was
-- enabled.
voiceChannelResponse_creationDate :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Text)
voiceChannelResponse_creationDate = Lens.lens (\VoiceChannelResponse' {creationDate} -> creationDate) (\s@VoiceChannelResponse' {} a -> s {creationDate = a} :: VoiceChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
voiceChannelResponse_hasCredential :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Bool)
voiceChannelResponse_hasCredential = Lens.lens (\VoiceChannelResponse' {hasCredential} -> hasCredential) (\s@VoiceChannelResponse' {} a -> s {hasCredential = a} :: VoiceChannelResponse)

-- | (Deprecated) An identifier for the voice channel. This property is
-- retained only for backward compatibility.
voiceChannelResponse_id :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Text)
voiceChannelResponse_id = Lens.lens (\VoiceChannelResponse' {id} -> id) (\s@VoiceChannelResponse' {} a -> s {id = a} :: VoiceChannelResponse)

-- | Specifies whether the voice channel is enabled for the application.
voiceChannelResponse_enabled :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Bool)
voiceChannelResponse_enabled = Lens.lens (\VoiceChannelResponse' {enabled} -> enabled) (\s@VoiceChannelResponse' {} a -> s {enabled = a} :: VoiceChannelResponse)

-- | The user who last modified the voice channel.
voiceChannelResponse_lastModifiedBy :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Text)
voiceChannelResponse_lastModifiedBy = Lens.lens (\VoiceChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@VoiceChannelResponse' {} a -> s {lastModifiedBy = a} :: VoiceChannelResponse)

-- | Specifies whether the voice channel is archived.
voiceChannelResponse_isArchived :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Bool)
voiceChannelResponse_isArchived = Lens.lens (\VoiceChannelResponse' {isArchived} -> isArchived) (\s@VoiceChannelResponse' {} a -> s {isArchived = a} :: VoiceChannelResponse)

-- | The unique identifier for the application that the voice channel applies
-- to.
voiceChannelResponse_applicationId :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Text)
voiceChannelResponse_applicationId = Lens.lens (\VoiceChannelResponse' {applicationId} -> applicationId) (\s@VoiceChannelResponse' {} a -> s {applicationId = a} :: VoiceChannelResponse)

-- | The current version of the voice channel.
voiceChannelResponse_version :: Lens.Lens' VoiceChannelResponse (Prelude.Maybe Prelude.Int)
voiceChannelResponse_version = Lens.lens (\VoiceChannelResponse' {version} -> version) (\s@VoiceChannelResponse' {} a -> s {version = a} :: VoiceChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- voice channel, this value is VOICE.
voiceChannelResponse_platform :: Lens.Lens' VoiceChannelResponse Prelude.Text
voiceChannelResponse_platform = Lens.lens (\VoiceChannelResponse' {platform} -> platform) (\s@VoiceChannelResponse' {} a -> s {platform = a} :: VoiceChannelResponse)

instance Core.FromJSON VoiceChannelResponse where
  parseJSON =
    Core.withObject
      "VoiceChannelResponse"
      ( \x ->
          VoiceChannelResponse'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "HasCredential")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "IsArchived")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..: "Platform")
      )

instance Prelude.Hashable VoiceChannelResponse where
  hashWithSalt _salt VoiceChannelResponse' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` platform

instance Prelude.NFData VoiceChannelResponse where
  rnf VoiceChannelResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf platform
