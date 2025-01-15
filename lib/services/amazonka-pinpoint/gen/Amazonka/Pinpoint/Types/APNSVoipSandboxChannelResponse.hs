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
-- Module      : Amazonka.Pinpoint.Types.APNSVoipSandboxChannelResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSVoipSandboxChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) VoIP sandbox channel for an application.
--
-- /See:/ 'newAPNSVoipSandboxChannelResponse' smart constructor.
data APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse'
  { -- | The unique identifier for the application that the APNs VoIP sandbox
    -- channel applies to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs VoIP sandbox channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with the APNs sandbox environment for this channel, key or
    -- certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is enabled for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the APNs VoIP sandbox channel is configured to
    -- communicate with APNs by using APNs tokens. To provide an authentication
    -- key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This
    -- property is retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the APNs VoIP sandbox channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs VoIP sandbox channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The current version of the APNs VoIP sandbox channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'aPNSVoipSandboxChannelResponse_applicationId' - The unique identifier for the application that the APNs VoIP sandbox
-- channel applies to.
--
-- 'creationDate', 'aPNSVoipSandboxChannelResponse_creationDate' - The date and time when the APNs VoIP sandbox channel was enabled.
--
-- 'defaultAuthenticationMethod', 'aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
--
-- 'enabled', 'aPNSVoipSandboxChannelResponse_enabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
--
-- 'hasCredential', 'aPNSVoipSandboxChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'hasTokenKey', 'aPNSVoipSandboxChannelResponse_hasTokenKey' - Specifies whether the APNs VoIP sandbox channel is configured to
-- communicate with APNs by using APNs tokens. To provide an authentication
-- key for APNs tokens, set the TokenKey property of the channel.
--
-- 'id', 'aPNSVoipSandboxChannelResponse_id' - (Deprecated) An identifier for the APNs VoIP sandbox channel. This
-- property is retained only for backward compatibility.
--
-- 'isArchived', 'aPNSVoipSandboxChannelResponse_isArchived' - Specifies whether the APNs VoIP sandbox channel is archived.
--
-- 'lastModifiedBy', 'aPNSVoipSandboxChannelResponse_lastModifiedBy' - The user who last modified the APNs VoIP sandbox channel.
--
-- 'lastModifiedDate', 'aPNSVoipSandboxChannelResponse_lastModifiedDate' - The date and time when the APNs VoIP sandbox channel was last modified.
--
-- 'version', 'aPNSVoipSandboxChannelResponse_version' - The current version of the APNs VoIP sandbox channel.
--
-- 'platform', 'aPNSVoipSandboxChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
newAPNSVoipSandboxChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  APNSVoipSandboxChannelResponse
newAPNSVoipSandboxChannelResponse pPlatform_ =
  APNSVoipSandboxChannelResponse'
    { applicationId =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      defaultAuthenticationMethod =
        Prelude.Nothing,
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

-- | The unique identifier for the application that the APNs VoIP sandbox
-- channel applies to.
aPNSVoipSandboxChannelResponse_applicationId :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_applicationId = Lens.lens (\APNSVoipSandboxChannelResponse' {applicationId} -> applicationId) (\s@APNSVoipSandboxChannelResponse' {} a -> s {applicationId = a} :: APNSVoipSandboxChannelResponse)

-- | The date and time when the APNs VoIP sandbox channel was enabled.
aPNSVoipSandboxChannelResponse_creationDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_creationDate = Lens.lens (\APNSVoipSandboxChannelResponse' {creationDate} -> creationDate) (\s@APNSVoipSandboxChannelResponse' {} a -> s {creationDate = a} :: APNSVoipSandboxChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSVoipSandboxChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipSandboxChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipSandboxChannelResponse)

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
aPNSVoipSandboxChannelResponse_enabled :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelResponse_enabled = Lens.lens (\APNSVoipSandboxChannelResponse' {enabled} -> enabled) (\s@APNSVoipSandboxChannelResponse' {} a -> s {enabled = a} :: APNSVoipSandboxChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSVoipSandboxChannelResponse_hasCredential :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelResponse_hasCredential = Lens.lens (\APNSVoipSandboxChannelResponse' {hasCredential} -> hasCredential) (\s@APNSVoipSandboxChannelResponse' {} a -> s {hasCredential = a} :: APNSVoipSandboxChannelResponse)

-- | Specifies whether the APNs VoIP sandbox channel is configured to
-- communicate with APNs by using APNs tokens. To provide an authentication
-- key for APNs tokens, set the TokenKey property of the channel.
aPNSVoipSandboxChannelResponse_hasTokenKey :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelResponse_hasTokenKey = Lens.lens (\APNSVoipSandboxChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSVoipSandboxChannelResponse' {} a -> s {hasTokenKey = a} :: APNSVoipSandboxChannelResponse)

-- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This
-- property is retained only for backward compatibility.
aPNSVoipSandboxChannelResponse_id :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_id = Lens.lens (\APNSVoipSandboxChannelResponse' {id} -> id) (\s@APNSVoipSandboxChannelResponse' {} a -> s {id = a} :: APNSVoipSandboxChannelResponse)

-- | Specifies whether the APNs VoIP sandbox channel is archived.
aPNSVoipSandboxChannelResponse_isArchived :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelResponse_isArchived = Lens.lens (\APNSVoipSandboxChannelResponse' {isArchived} -> isArchived) (\s@APNSVoipSandboxChannelResponse' {} a -> s {isArchived = a} :: APNSVoipSandboxChannelResponse)

-- | The user who last modified the APNs VoIP sandbox channel.
aPNSVoipSandboxChannelResponse_lastModifiedBy :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_lastModifiedBy = Lens.lens (\APNSVoipSandboxChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSVoipSandboxChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSVoipSandboxChannelResponse)

-- | The date and time when the APNs VoIP sandbox channel was last modified.
aPNSVoipSandboxChannelResponse_lastModifiedDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_lastModifiedDate = Lens.lens (\APNSVoipSandboxChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSVoipSandboxChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSVoipSandboxChannelResponse)

-- | The current version of the APNs VoIP sandbox channel.
aPNSVoipSandboxChannelResponse_version :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Int)
aPNSVoipSandboxChannelResponse_version = Lens.lens (\APNSVoipSandboxChannelResponse' {version} -> version) (\s@APNSVoipSandboxChannelResponse' {} a -> s {version = a} :: APNSVoipSandboxChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
aPNSVoipSandboxChannelResponse_platform :: Lens.Lens' APNSVoipSandboxChannelResponse Prelude.Text
aPNSVoipSandboxChannelResponse_platform = Lens.lens (\APNSVoipSandboxChannelResponse' {platform} -> platform) (\s@APNSVoipSandboxChannelResponse' {} a -> s {platform = a} :: APNSVoipSandboxChannelResponse)

instance Data.FromJSON APNSVoipSandboxChannelResponse where
  parseJSON =
    Data.withObject
      "APNSVoipSandboxChannelResponse"
      ( \x ->
          APNSVoipSandboxChannelResponse'
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

instance
  Prelude.Hashable
    APNSVoipSandboxChannelResponse
  where
  hashWithSalt
    _salt
    APNSVoipSandboxChannelResponse' {..} =
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

instance
  Prelude.NFData
    APNSVoipSandboxChannelResponse
  where
  rnf APNSVoipSandboxChannelResponse' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf creationDate `Prelude.seq`
        Prelude.rnf defaultAuthenticationMethod `Prelude.seq`
          Prelude.rnf enabled `Prelude.seq`
            Prelude.rnf hasCredential `Prelude.seq`
              Prelude.rnf hasTokenKey `Prelude.seq`
                Prelude.rnf id `Prelude.seq`
                  Prelude.rnf isArchived `Prelude.seq`
                    Prelude.rnf lastModifiedBy `Prelude.seq`
                      Prelude.rnf lastModifiedDate `Prelude.seq`
                        Prelude.rnf version `Prelude.seq`
                          Prelude.rnf platform
