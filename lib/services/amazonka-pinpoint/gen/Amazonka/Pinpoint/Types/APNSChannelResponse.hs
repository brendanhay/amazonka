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
-- Module      : Amazonka.Pinpoint.Types.APNSChannelResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) channel for an application.
--
-- /See:/ 'newAPNSChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { -- | The unique identifier for the application that the APNs channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the APNs channel is configured to communicate with
    -- APNs by using APNs tokens. To provide an authentication key for APNs
    -- tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the APNs channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The current version of the APNs channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs channel, this value is APNS.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'aPNSChannelResponse_applicationId' - The unique identifier for the application that the APNs channel applies
-- to.
--
-- 'creationDate', 'aPNSChannelResponse_creationDate' - The date and time when the APNs channel was enabled.
--
-- 'defaultAuthenticationMethod', 'aPNSChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
--
-- 'enabled', 'aPNSChannelResponse_enabled' - Specifies whether the APNs channel is enabled for the application.
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
-- 'isArchived', 'aPNSChannelResponse_isArchived' - Specifies whether the APNs channel is archived.
--
-- 'lastModifiedBy', 'aPNSChannelResponse_lastModifiedBy' - The user who last modified the APNs channel.
--
-- 'lastModifiedDate', 'aPNSChannelResponse_lastModifiedDate' - The date and time when the APNs channel was last modified.
--
-- 'version', 'aPNSChannelResponse_version' - The current version of the APNs channel.
--
-- 'platform', 'aPNSChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs channel, this value is APNS.
newAPNSChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  APNSChannelResponse
newAPNSChannelResponse pPlatform_ =
  APNSChannelResponse'
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

-- | The unique identifier for the application that the APNs channel applies
-- to.
aPNSChannelResponse_applicationId :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_applicationId = Lens.lens (\APNSChannelResponse' {applicationId} -> applicationId) (\s@APNSChannelResponse' {} a -> s {applicationId = a} :: APNSChannelResponse)

-- | The date and time when the APNs channel was enabled.
aPNSChannelResponse_creationDate :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_creationDate = Lens.lens (\APNSChannelResponse' {creationDate} -> creationDate) (\s@APNSChannelResponse' {} a -> s {creationDate = a} :: APNSChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
aPNSChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is enabled for the application.
aPNSChannelResponse_enabled :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_enabled = Lens.lens (\APNSChannelResponse' {enabled} -> enabled) (\s@APNSChannelResponse' {} a -> s {enabled = a} :: APNSChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSChannelResponse_hasCredential :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_hasCredential = Lens.lens (\APNSChannelResponse' {hasCredential} -> hasCredential) (\s@APNSChannelResponse' {} a -> s {hasCredential = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is configured to communicate with
-- APNs by using APNs tokens. To provide an authentication key for APNs
-- tokens, set the TokenKey property of the channel.
aPNSChannelResponse_hasTokenKey :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_hasTokenKey = Lens.lens (\APNSChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSChannelResponse' {} a -> s {hasTokenKey = a} :: APNSChannelResponse)

-- | (Deprecated) An identifier for the APNs channel. This property is
-- retained only for backward compatibility.
aPNSChannelResponse_id :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_id = Lens.lens (\APNSChannelResponse' {id} -> id) (\s@APNSChannelResponse' {} a -> s {id = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is archived.
aPNSChannelResponse_isArchived :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_isArchived = Lens.lens (\APNSChannelResponse' {isArchived} -> isArchived) (\s@APNSChannelResponse' {} a -> s {isArchived = a} :: APNSChannelResponse)

-- | The user who last modified the APNs channel.
aPNSChannelResponse_lastModifiedBy :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_lastModifiedBy = Lens.lens (\APNSChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSChannelResponse)

-- | The date and time when the APNs channel was last modified.
aPNSChannelResponse_lastModifiedDate :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_lastModifiedDate = Lens.lens (\APNSChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSChannelResponse)

-- | The current version of the APNs channel.
aPNSChannelResponse_version :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Int)
aPNSChannelResponse_version = Lens.lens (\APNSChannelResponse' {version} -> version) (\s@APNSChannelResponse' {} a -> s {version = a} :: APNSChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs channel, this value is APNS.
aPNSChannelResponse_platform :: Lens.Lens' APNSChannelResponse Prelude.Text
aPNSChannelResponse_platform = Lens.lens (\APNSChannelResponse' {platform} -> platform) (\s@APNSChannelResponse' {} a -> s {platform = a} :: APNSChannelResponse)

instance Data.FromJSON APNSChannelResponse where
  parseJSON =
    Data.withObject
      "APNSChannelResponse"
      ( \x ->
          APNSChannelResponse'
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

instance Prelude.Hashable APNSChannelResponse where
  hashWithSalt _salt APNSChannelResponse' {..} =
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

instance Prelude.NFData APNSChannelResponse where
  rnf APNSChannelResponse' {..} =
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
