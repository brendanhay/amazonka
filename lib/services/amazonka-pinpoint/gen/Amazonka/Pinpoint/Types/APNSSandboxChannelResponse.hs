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
-- Module      : Amazonka.Pinpoint.Types.APNSSandboxChannelResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSSandboxChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) sandbox channel for an application.
--
-- /See:/ 'newAPNSSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { -- | The unique identifier for the application that the APNs sandbox channel
    -- applies to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs sandbox channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with the APNs sandbox environment for this channel, key or
    -- certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs sandbox channel is enabled for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the APNs sandbox channel is configured to communicate
    -- with APNs by using APNs tokens. To provide an authentication key for
    -- APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs sandbox channel. This property
    -- is retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs sandbox channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the APNs sandbox channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs sandbox channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The current version of the APNs sandbox channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs sandbox channel, this value is APNS_SANDBOX.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'aPNSSandboxChannelResponse_applicationId' - The unique identifier for the application that the APNs sandbox channel
-- applies to.
--
-- 'creationDate', 'aPNSSandboxChannelResponse_creationDate' - The date and time when the APNs sandbox channel was enabled.
--
-- 'defaultAuthenticationMethod', 'aPNSSandboxChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
--
-- 'enabled', 'aPNSSandboxChannelResponse_enabled' - Specifies whether the APNs sandbox channel is enabled for the
-- application.
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
-- 'isArchived', 'aPNSSandboxChannelResponse_isArchived' - Specifies whether the APNs sandbox channel is archived.
--
-- 'lastModifiedBy', 'aPNSSandboxChannelResponse_lastModifiedBy' - The user who last modified the APNs sandbox channel.
--
-- 'lastModifiedDate', 'aPNSSandboxChannelResponse_lastModifiedDate' - The date and time when the APNs sandbox channel was last modified.
--
-- 'version', 'aPNSSandboxChannelResponse_version' - The current version of the APNs sandbox channel.
--
-- 'platform', 'aPNSSandboxChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs sandbox channel, this value is APNS_SANDBOX.
newAPNSSandboxChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  APNSSandboxChannelResponse
newAPNSSandboxChannelResponse pPlatform_ =
  APNSSandboxChannelResponse'
    { applicationId =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      defaultAuthenticationMethod = Prelude.Nothing,
      enabled = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      hasTokenKey = Prelude.Nothing,
      id = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      version = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The unique identifier for the application that the APNs sandbox channel
-- applies to.
aPNSSandboxChannelResponse_applicationId :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_applicationId = Lens.lens (\APNSSandboxChannelResponse' {applicationId} -> applicationId) (\s@APNSSandboxChannelResponse' {} a -> s {applicationId = a} :: APNSSandboxChannelResponse)

-- | The date and time when the APNs sandbox channel was enabled.
aPNSSandboxChannelResponse_creationDate :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_creationDate = Lens.lens (\APNSSandboxChannelResponse' {creationDate} -> creationDate) (\s@APNSSandboxChannelResponse' {} a -> s {creationDate = a} :: APNSSandboxChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
aPNSSandboxChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSSandboxChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSSandboxChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is enabled for the
-- application.
aPNSSandboxChannelResponse_enabled :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelResponse_enabled = Lens.lens (\APNSSandboxChannelResponse' {enabled} -> enabled) (\s@APNSSandboxChannelResponse' {} a -> s {enabled = a} :: APNSSandboxChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSSandboxChannelResponse_hasCredential :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelResponse_hasCredential = Lens.lens (\APNSSandboxChannelResponse' {hasCredential} -> hasCredential) (\s@APNSSandboxChannelResponse' {} a -> s {hasCredential = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
aPNSSandboxChannelResponse_hasTokenKey :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelResponse_hasTokenKey = Lens.lens (\APNSSandboxChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSSandboxChannelResponse' {} a -> s {hasTokenKey = a} :: APNSSandboxChannelResponse)

-- | (Deprecated) An identifier for the APNs sandbox channel. This property
-- is retained only for backward compatibility.
aPNSSandboxChannelResponse_id :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_id = Lens.lens (\APNSSandboxChannelResponse' {id} -> id) (\s@APNSSandboxChannelResponse' {} a -> s {id = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is archived.
aPNSSandboxChannelResponse_isArchived :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelResponse_isArchived = Lens.lens (\APNSSandboxChannelResponse' {isArchived} -> isArchived) (\s@APNSSandboxChannelResponse' {} a -> s {isArchived = a} :: APNSSandboxChannelResponse)

-- | The user who last modified the APNs sandbox channel.
aPNSSandboxChannelResponse_lastModifiedBy :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_lastModifiedBy = Lens.lens (\APNSSandboxChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSSandboxChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSSandboxChannelResponse)

-- | The date and time when the APNs sandbox channel was last modified.
aPNSSandboxChannelResponse_lastModifiedDate :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_lastModifiedDate = Lens.lens (\APNSSandboxChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSSandboxChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSSandboxChannelResponse)

-- | The current version of the APNs sandbox channel.
aPNSSandboxChannelResponse_version :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Int)
aPNSSandboxChannelResponse_version = Lens.lens (\APNSSandboxChannelResponse' {version} -> version) (\s@APNSSandboxChannelResponse' {} a -> s {version = a} :: APNSSandboxChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs sandbox channel, this value is APNS_SANDBOX.
aPNSSandboxChannelResponse_platform :: Lens.Lens' APNSSandboxChannelResponse Prelude.Text
aPNSSandboxChannelResponse_platform = Lens.lens (\APNSSandboxChannelResponse' {platform} -> platform) (\s@APNSSandboxChannelResponse' {} a -> s {platform = a} :: APNSSandboxChannelResponse)

instance Data.FromJSON APNSSandboxChannelResponse where
  parseJSON =
    Data.withObject
      "APNSSandboxChannelResponse"
      ( \x ->
          APNSSandboxChannelResponse'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "DefaultAuthenticationMethod")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "HasCredential")
            Prelude.<*> (x Data..:? "HasTokenKey")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "IsArchived")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Platform")
      )

instance Prelude.Hashable APNSSandboxChannelResponse where
  hashWithSalt _salt APNSSandboxChannelResponse' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` defaultAuthenticationMethod
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` hasTokenKey
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` platform

instance Prelude.NFData APNSSandboxChannelResponse where
  rnf APNSSandboxChannelResponse' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf defaultAuthenticationMethod
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf hasTokenKey
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf platform
