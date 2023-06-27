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
-- Module      : Amazonka.Pinpoint.Types.ADMChannelResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ADMChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the ADM (Amazon
-- Device Messaging) channel for an application.
--
-- /See:/ 'newADMChannelResponse' smart constructor.
data ADMChannelResponse = ADMChannelResponse'
  { -- | The unique identifier for the application that the ADM channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the ADM channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the ADM channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the ADM channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the ADM channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the ADM channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the ADM channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The current version of the ADM channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The type of messaging or notification platform for the channel. For the
    -- ADM channel, this value is ADM.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ADMChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'aDMChannelResponse_applicationId' - The unique identifier for the application that the ADM channel applies
-- to.
--
-- 'creationDate', 'aDMChannelResponse_creationDate' - The date and time when the ADM channel was enabled.
--
-- 'enabled', 'aDMChannelResponse_enabled' - Specifies whether the ADM channel is enabled for the application.
--
-- 'hasCredential', 'aDMChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'aDMChannelResponse_id' - (Deprecated) An identifier for the ADM channel. This property is
-- retained only for backward compatibility.
--
-- 'isArchived', 'aDMChannelResponse_isArchived' - Specifies whether the ADM channel is archived.
--
-- 'lastModifiedBy', 'aDMChannelResponse_lastModifiedBy' - The user who last modified the ADM channel.
--
-- 'lastModifiedDate', 'aDMChannelResponse_lastModifiedDate' - The date and time when the ADM channel was last modified.
--
-- 'version', 'aDMChannelResponse_version' - The current version of the ADM channel.
--
-- 'platform', 'aDMChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- ADM channel, this value is ADM.
newADMChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  ADMChannelResponse
newADMChannelResponse pPlatform_ =
  ADMChannelResponse'
    { applicationId =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      id = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      version = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The unique identifier for the application that the ADM channel applies
-- to.
aDMChannelResponse_applicationId :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Text)
aDMChannelResponse_applicationId = Lens.lens (\ADMChannelResponse' {applicationId} -> applicationId) (\s@ADMChannelResponse' {} a -> s {applicationId = a} :: ADMChannelResponse)

-- | The date and time when the ADM channel was enabled.
aDMChannelResponse_creationDate :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Text)
aDMChannelResponse_creationDate = Lens.lens (\ADMChannelResponse' {creationDate} -> creationDate) (\s@ADMChannelResponse' {} a -> s {creationDate = a} :: ADMChannelResponse)

-- | Specifies whether the ADM channel is enabled for the application.
aDMChannelResponse_enabled :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Bool)
aDMChannelResponse_enabled = Lens.lens (\ADMChannelResponse' {enabled} -> enabled) (\s@ADMChannelResponse' {} a -> s {enabled = a} :: ADMChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aDMChannelResponse_hasCredential :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Bool)
aDMChannelResponse_hasCredential = Lens.lens (\ADMChannelResponse' {hasCredential} -> hasCredential) (\s@ADMChannelResponse' {} a -> s {hasCredential = a} :: ADMChannelResponse)

-- | (Deprecated) An identifier for the ADM channel. This property is
-- retained only for backward compatibility.
aDMChannelResponse_id :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Text)
aDMChannelResponse_id = Lens.lens (\ADMChannelResponse' {id} -> id) (\s@ADMChannelResponse' {} a -> s {id = a} :: ADMChannelResponse)

-- | Specifies whether the ADM channel is archived.
aDMChannelResponse_isArchived :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Bool)
aDMChannelResponse_isArchived = Lens.lens (\ADMChannelResponse' {isArchived} -> isArchived) (\s@ADMChannelResponse' {} a -> s {isArchived = a} :: ADMChannelResponse)

-- | The user who last modified the ADM channel.
aDMChannelResponse_lastModifiedBy :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Text)
aDMChannelResponse_lastModifiedBy = Lens.lens (\ADMChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@ADMChannelResponse' {} a -> s {lastModifiedBy = a} :: ADMChannelResponse)

-- | The date and time when the ADM channel was last modified.
aDMChannelResponse_lastModifiedDate :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Text)
aDMChannelResponse_lastModifiedDate = Lens.lens (\ADMChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@ADMChannelResponse' {} a -> s {lastModifiedDate = a} :: ADMChannelResponse)

-- | The current version of the ADM channel.
aDMChannelResponse_version :: Lens.Lens' ADMChannelResponse (Prelude.Maybe Prelude.Int)
aDMChannelResponse_version = Lens.lens (\ADMChannelResponse' {version} -> version) (\s@ADMChannelResponse' {} a -> s {version = a} :: ADMChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- ADM channel, this value is ADM.
aDMChannelResponse_platform :: Lens.Lens' ADMChannelResponse Prelude.Text
aDMChannelResponse_platform = Lens.lens (\ADMChannelResponse' {platform} -> platform) (\s@ADMChannelResponse' {} a -> s {platform = a} :: ADMChannelResponse)

instance Data.FromJSON ADMChannelResponse where
  parseJSON =
    Data.withObject
      "ADMChannelResponse"
      ( \x ->
          ADMChannelResponse'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "HasCredential")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "IsArchived")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Platform")
      )

instance Prelude.Hashable ADMChannelResponse where
  hashWithSalt _salt ADMChannelResponse' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` platform

instance Prelude.NFData ADMChannelResponse where
  rnf ADMChannelResponse' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf platform
