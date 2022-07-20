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
-- Module      : Amazonka.Pinpoint.Types.ChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the general settings and status of a channel
-- for an application.
--
-- /See:/ 'newChannelResponse' smart constructor.
data ChannelResponse = ChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the channel was last
    -- modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the channel. This property is retained
    -- only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The current version of the channel.
    version :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'channelResponse_lastModifiedDate' - The date and time, in ISO 8601 format, when the channel was last
-- modified.
--
-- 'creationDate', 'channelResponse_creationDate' - The date and time, in ISO 8601 format, when the channel was enabled.
--
-- 'hasCredential', 'channelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'channelResponse_id' - (Deprecated) An identifier for the channel. This property is retained
-- only for backward compatibility.
--
-- 'enabled', 'channelResponse_enabled' - Specifies whether the channel is enabled for the application.
--
-- 'lastModifiedBy', 'channelResponse_lastModifiedBy' - The user who last modified the channel.
--
-- 'isArchived', 'channelResponse_isArchived' - Specifies whether the channel is archived.
--
-- 'applicationId', 'channelResponse_applicationId' - The unique identifier for the application.
--
-- 'version', 'channelResponse_version' - The current version of the channel.
newChannelResponse ::
  ChannelResponse
newChannelResponse =
  ChannelResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      id = Prelude.Nothing,
      enabled = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The date and time, in ISO 8601 format, when the channel was last
-- modified.
channelResponse_lastModifiedDate :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_lastModifiedDate = Lens.lens (\ChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@ChannelResponse' {} a -> s {lastModifiedDate = a} :: ChannelResponse)

-- | The date and time, in ISO 8601 format, when the channel was enabled.
channelResponse_creationDate :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_creationDate = Lens.lens (\ChannelResponse' {creationDate} -> creationDate) (\s@ChannelResponse' {} a -> s {creationDate = a} :: ChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
channelResponse_hasCredential :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Bool)
channelResponse_hasCredential = Lens.lens (\ChannelResponse' {hasCredential} -> hasCredential) (\s@ChannelResponse' {} a -> s {hasCredential = a} :: ChannelResponse)

-- | (Deprecated) An identifier for the channel. This property is retained
-- only for backward compatibility.
channelResponse_id :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_id = Lens.lens (\ChannelResponse' {id} -> id) (\s@ChannelResponse' {} a -> s {id = a} :: ChannelResponse)

-- | Specifies whether the channel is enabled for the application.
channelResponse_enabled :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Bool)
channelResponse_enabled = Lens.lens (\ChannelResponse' {enabled} -> enabled) (\s@ChannelResponse' {} a -> s {enabled = a} :: ChannelResponse)

-- | The user who last modified the channel.
channelResponse_lastModifiedBy :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_lastModifiedBy = Lens.lens (\ChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@ChannelResponse' {} a -> s {lastModifiedBy = a} :: ChannelResponse)

-- | Specifies whether the channel is archived.
channelResponse_isArchived :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Bool)
channelResponse_isArchived = Lens.lens (\ChannelResponse' {isArchived} -> isArchived) (\s@ChannelResponse' {} a -> s {isArchived = a} :: ChannelResponse)

-- | The unique identifier for the application.
channelResponse_applicationId :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_applicationId = Lens.lens (\ChannelResponse' {applicationId} -> applicationId) (\s@ChannelResponse' {} a -> s {applicationId = a} :: ChannelResponse)

-- | The current version of the channel.
channelResponse_version :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Int)
channelResponse_version = Lens.lens (\ChannelResponse' {version} -> version) (\s@ChannelResponse' {} a -> s {version = a} :: ChannelResponse)

instance Core.FromJSON ChannelResponse where
  parseJSON =
    Core.withObject
      "ChannelResponse"
      ( \x ->
          ChannelResponse'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "HasCredential")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "IsArchived")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Version")
      )

instance Prelude.Hashable ChannelResponse where
  hashWithSalt _salt ChannelResponse' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` version

instance Prelude.NFData ChannelResponse where
  rnf ChannelResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf version
