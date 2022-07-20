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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) channel for an application.
--
-- /See:/ 'newAPNSChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { -- | The date and time when the APNs channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs channel is configured to communicate with
    -- APNs by using APNs tokens. To provide an authentication key for APNs
    -- tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the APNs channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | The user who last modified the APNs channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the application that the APNs channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
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
-- 'lastModifiedDate', 'aPNSChannelResponse_lastModifiedDate' - The date and time when the APNs channel was last modified.
--
-- 'hasTokenKey', 'aPNSChannelResponse_hasTokenKey' - Specifies whether the APNs channel is configured to communicate with
-- APNs by using APNs tokens. To provide an authentication key for APNs
-- tokens, set the TokenKey property of the channel.
--
-- 'creationDate', 'aPNSChannelResponse_creationDate' - The date and time when the APNs channel was enabled.
--
-- 'hasCredential', 'aPNSChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'id', 'aPNSChannelResponse_id' - (Deprecated) An identifier for the APNs channel. This property is
-- retained only for backward compatibility.
--
-- 'enabled', 'aPNSChannelResponse_enabled' - Specifies whether the APNs channel is enabled for the application.
--
-- 'defaultAuthenticationMethod', 'aPNSChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
--
-- 'lastModifiedBy', 'aPNSChannelResponse_lastModifiedBy' - The user who last modified the APNs channel.
--
-- 'isArchived', 'aPNSChannelResponse_isArchived' - Specifies whether the APNs channel is archived.
--
-- 'applicationId', 'aPNSChannelResponse_applicationId' - The unique identifier for the application that the APNs channel applies
-- to.
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
    { lastModifiedDate =
        Prelude.Nothing,
      hasTokenKey = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      id = Prelude.Nothing,
      enabled = Prelude.Nothing,
      defaultAuthenticationMethod = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      version = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The date and time when the APNs channel was last modified.
aPNSChannelResponse_lastModifiedDate :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_lastModifiedDate = Lens.lens (\APNSChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is configured to communicate with
-- APNs by using APNs tokens. To provide an authentication key for APNs
-- tokens, set the TokenKey property of the channel.
aPNSChannelResponse_hasTokenKey :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_hasTokenKey = Lens.lens (\APNSChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSChannelResponse' {} a -> s {hasTokenKey = a} :: APNSChannelResponse)

-- | The date and time when the APNs channel was enabled.
aPNSChannelResponse_creationDate :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_creationDate = Lens.lens (\APNSChannelResponse' {creationDate} -> creationDate) (\s@APNSChannelResponse' {} a -> s {creationDate = a} :: APNSChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSChannelResponse_hasCredential :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_hasCredential = Lens.lens (\APNSChannelResponse' {hasCredential} -> hasCredential) (\s@APNSChannelResponse' {} a -> s {hasCredential = a} :: APNSChannelResponse)

-- | (Deprecated) An identifier for the APNs channel. This property is
-- retained only for backward compatibility.
aPNSChannelResponse_id :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_id = Lens.lens (\APNSChannelResponse' {id} -> id) (\s@APNSChannelResponse' {} a -> s {id = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is enabled for the application.
aPNSChannelResponse_enabled :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_enabled = Lens.lens (\APNSChannelResponse' {enabled} -> enabled) (\s@APNSChannelResponse' {} a -> s {enabled = a} :: APNSChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
aPNSChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSChannelResponse)

-- | The user who last modified the APNs channel.
aPNSChannelResponse_lastModifiedBy :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_lastModifiedBy = Lens.lens (\APNSChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSChannelResponse)

-- | Specifies whether the APNs channel is archived.
aPNSChannelResponse_isArchived :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSChannelResponse_isArchived = Lens.lens (\APNSChannelResponse' {isArchived} -> isArchived) (\s@APNSChannelResponse' {} a -> s {isArchived = a} :: APNSChannelResponse)

-- | The unique identifier for the application that the APNs channel applies
-- to.
aPNSChannelResponse_applicationId :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Text)
aPNSChannelResponse_applicationId = Lens.lens (\APNSChannelResponse' {applicationId} -> applicationId) (\s@APNSChannelResponse' {} a -> s {applicationId = a} :: APNSChannelResponse)

-- | The current version of the APNs channel.
aPNSChannelResponse_version :: Lens.Lens' APNSChannelResponse (Prelude.Maybe Prelude.Int)
aPNSChannelResponse_version = Lens.lens (\APNSChannelResponse' {version} -> version) (\s@APNSChannelResponse' {} a -> s {version = a} :: APNSChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs channel, this value is APNS.
aPNSChannelResponse_platform :: Lens.Lens' APNSChannelResponse Prelude.Text
aPNSChannelResponse_platform = Lens.lens (\APNSChannelResponse' {platform} -> platform) (\s@APNSChannelResponse' {} a -> s {platform = a} :: APNSChannelResponse)

instance Core.FromJSON APNSChannelResponse where
  parseJSON =
    Core.withObject
      "APNSChannelResponse"
      ( \x ->
          APNSChannelResponse'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "HasTokenKey")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "HasCredential")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "DefaultAuthenticationMethod")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "IsArchived")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..: "Platform")
      )

instance Prelude.Hashable APNSChannelResponse where
  hashWithSalt _salt APNSChannelResponse' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` hasTokenKey
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` defaultAuthenticationMethod
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` platform

instance Prelude.NFData APNSChannelResponse where
  rnf APNSChannelResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf hasTokenKey
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf defaultAuthenticationMethod
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf platform
