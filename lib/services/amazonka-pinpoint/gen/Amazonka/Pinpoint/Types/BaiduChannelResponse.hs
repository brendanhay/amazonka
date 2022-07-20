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
-- Module      : Amazonka.Pinpoint.Types.BaiduChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.BaiduChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the Baidu (Baidu
-- Cloud Push) channel for an application.
--
-- /See:/ 'newBaiduChannelResponse' smart constructor.
data BaiduChannelResponse = BaiduChannelResponse'
  { -- | The date and time when the Baidu channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the Baidu channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the Baidu channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the Baidu channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the Baidu channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the Baidu channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the application that the Baidu channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The current version of the Baidu channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The API key that you received from the Baidu Cloud Push service to
    -- communicate with the service.
    credential :: Prelude.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- Baidu channel, this value is BAIDU.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaiduChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'baiduChannelResponse_lastModifiedDate' - The date and time when the Baidu channel was last modified.
--
-- 'creationDate', 'baiduChannelResponse_creationDate' - The date and time when the Baidu channel was enabled.
--
-- 'hasCredential', 'baiduChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'baiduChannelResponse_id' - (Deprecated) An identifier for the Baidu channel. This property is
-- retained only for backward compatibility.
--
-- 'enabled', 'baiduChannelResponse_enabled' - Specifies whether the Baidu channel is enabled for the application.
--
-- 'lastModifiedBy', 'baiduChannelResponse_lastModifiedBy' - The user who last modified the Baidu channel.
--
-- 'isArchived', 'baiduChannelResponse_isArchived' - Specifies whether the Baidu channel is archived.
--
-- 'applicationId', 'baiduChannelResponse_applicationId' - The unique identifier for the application that the Baidu channel applies
-- to.
--
-- 'version', 'baiduChannelResponse_version' - The current version of the Baidu channel.
--
-- 'credential', 'baiduChannelResponse_credential' - The API key that you received from the Baidu Cloud Push service to
-- communicate with the service.
--
-- 'platform', 'baiduChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- Baidu channel, this value is BAIDU.
newBaiduChannelResponse ::
  -- | 'credential'
  Prelude.Text ->
  -- | 'platform'
  Prelude.Text ->
  BaiduChannelResponse
newBaiduChannelResponse pCredential_ pPlatform_ =
  BaiduChannelResponse'
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
      credential = pCredential_,
      platform = pPlatform_
    }

-- | The date and time when the Baidu channel was last modified.
baiduChannelResponse_lastModifiedDate :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Text)
baiduChannelResponse_lastModifiedDate = Lens.lens (\BaiduChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@BaiduChannelResponse' {} a -> s {lastModifiedDate = a} :: BaiduChannelResponse)

-- | The date and time when the Baidu channel was enabled.
baiduChannelResponse_creationDate :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Text)
baiduChannelResponse_creationDate = Lens.lens (\BaiduChannelResponse' {creationDate} -> creationDate) (\s@BaiduChannelResponse' {} a -> s {creationDate = a} :: BaiduChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
baiduChannelResponse_hasCredential :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Bool)
baiduChannelResponse_hasCredential = Lens.lens (\BaiduChannelResponse' {hasCredential} -> hasCredential) (\s@BaiduChannelResponse' {} a -> s {hasCredential = a} :: BaiduChannelResponse)

-- | (Deprecated) An identifier for the Baidu channel. This property is
-- retained only for backward compatibility.
baiduChannelResponse_id :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Text)
baiduChannelResponse_id = Lens.lens (\BaiduChannelResponse' {id} -> id) (\s@BaiduChannelResponse' {} a -> s {id = a} :: BaiduChannelResponse)

-- | Specifies whether the Baidu channel is enabled for the application.
baiduChannelResponse_enabled :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Bool)
baiduChannelResponse_enabled = Lens.lens (\BaiduChannelResponse' {enabled} -> enabled) (\s@BaiduChannelResponse' {} a -> s {enabled = a} :: BaiduChannelResponse)

-- | The user who last modified the Baidu channel.
baiduChannelResponse_lastModifiedBy :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Text)
baiduChannelResponse_lastModifiedBy = Lens.lens (\BaiduChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@BaiduChannelResponse' {} a -> s {lastModifiedBy = a} :: BaiduChannelResponse)

-- | Specifies whether the Baidu channel is archived.
baiduChannelResponse_isArchived :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Bool)
baiduChannelResponse_isArchived = Lens.lens (\BaiduChannelResponse' {isArchived} -> isArchived) (\s@BaiduChannelResponse' {} a -> s {isArchived = a} :: BaiduChannelResponse)

-- | The unique identifier for the application that the Baidu channel applies
-- to.
baiduChannelResponse_applicationId :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Text)
baiduChannelResponse_applicationId = Lens.lens (\BaiduChannelResponse' {applicationId} -> applicationId) (\s@BaiduChannelResponse' {} a -> s {applicationId = a} :: BaiduChannelResponse)

-- | The current version of the Baidu channel.
baiduChannelResponse_version :: Lens.Lens' BaiduChannelResponse (Prelude.Maybe Prelude.Int)
baiduChannelResponse_version = Lens.lens (\BaiduChannelResponse' {version} -> version) (\s@BaiduChannelResponse' {} a -> s {version = a} :: BaiduChannelResponse)

-- | The API key that you received from the Baidu Cloud Push service to
-- communicate with the service.
baiduChannelResponse_credential :: Lens.Lens' BaiduChannelResponse Prelude.Text
baiduChannelResponse_credential = Lens.lens (\BaiduChannelResponse' {credential} -> credential) (\s@BaiduChannelResponse' {} a -> s {credential = a} :: BaiduChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- Baidu channel, this value is BAIDU.
baiduChannelResponse_platform :: Lens.Lens' BaiduChannelResponse Prelude.Text
baiduChannelResponse_platform = Lens.lens (\BaiduChannelResponse' {platform} -> platform) (\s@BaiduChannelResponse' {} a -> s {platform = a} :: BaiduChannelResponse)

instance Core.FromJSON BaiduChannelResponse where
  parseJSON =
    Core.withObject
      "BaiduChannelResponse"
      ( \x ->
          BaiduChannelResponse'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "HasCredential")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "IsArchived")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..: "Credential")
            Prelude.<*> (x Core..: "Platform")
      )

instance Prelude.Hashable BaiduChannelResponse where
  hashWithSalt _salt BaiduChannelResponse' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` credential
      `Prelude.hashWithSalt` platform

instance Prelude.NFData BaiduChannelResponse where
  rnf BaiduChannelResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf credential
      `Prelude.seq` Prelude.rnf platform
