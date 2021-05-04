{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) VoIP sandbox channel for an application.
--
-- /See:/ 'newAPNSVoipSandboxChannelResponse' smart constructor.
data APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse'
  { -- | The date and time when the APNs VoIP sandbox channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application that the APNs VoIP sandbox
    -- channel applies to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with the APNs sandbox environment for this channel, key or
    -- certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the APNs VoIP sandbox channel is configured to
    -- communicate with APNs by using APNs tokens. To provide an authentication
    -- key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This
    -- property is retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs VoIP sandbox channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs VoIP sandbox channel is enabled for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The current version of the APNs VoIP sandbox channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the APNs VoIP sandbox channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the APNs VoIP sandbox channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'APNSVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'aPNSVoipSandboxChannelResponse_lastModifiedDate' - The date and time when the APNs VoIP sandbox channel was last modified.
--
-- 'applicationId', 'aPNSVoipSandboxChannelResponse_applicationId' - The unique identifier for the application that the APNs VoIP sandbox
-- channel applies to.
--
-- 'defaultAuthenticationMethod', 'aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
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
-- 'creationDate', 'aPNSVoipSandboxChannelResponse_creationDate' - The date and time when the APNs VoIP sandbox channel was enabled.
--
-- 'enabled', 'aPNSVoipSandboxChannelResponse_enabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
--
-- 'version', 'aPNSVoipSandboxChannelResponse_version' - The current version of the APNs VoIP sandbox channel.
--
-- 'isArchived', 'aPNSVoipSandboxChannelResponse_isArchived' - Specifies whether the APNs VoIP sandbox channel is archived.
--
-- 'lastModifiedBy', 'aPNSVoipSandboxChannelResponse_lastModifiedBy' - The user who last modified the APNs VoIP sandbox channel.
--
-- 'platform', 'aPNSVoipSandboxChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
newAPNSVoipSandboxChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  APNSVoipSandboxChannelResponse
newAPNSVoipSandboxChannelResponse pPlatform_ =
  APNSVoipSandboxChannelResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      applicationId = Prelude.Nothing,
      defaultAuthenticationMethod =
        Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      hasTokenKey = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      version = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The date and time when the APNs VoIP sandbox channel was last modified.
aPNSVoipSandboxChannelResponse_lastModifiedDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_lastModifiedDate = Lens.lens (\APNSVoipSandboxChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSVoipSandboxChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSVoipSandboxChannelResponse)

-- | The unique identifier for the application that the APNs VoIP sandbox
-- channel applies to.
aPNSVoipSandboxChannelResponse_applicationId :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_applicationId = Lens.lens (\APNSVoipSandboxChannelResponse' {applicationId} -> applicationId) (\s@APNSVoipSandboxChannelResponse' {} a -> s {applicationId = a} :: APNSVoipSandboxChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSVoipSandboxChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipSandboxChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipSandboxChannelResponse)

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

-- | The date and time when the APNs VoIP sandbox channel was enabled.
aPNSVoipSandboxChannelResponse_creationDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_creationDate = Lens.lens (\APNSVoipSandboxChannelResponse' {creationDate} -> creationDate) (\s@APNSVoipSandboxChannelResponse' {} a -> s {creationDate = a} :: APNSVoipSandboxChannelResponse)

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the
-- application.
aPNSVoipSandboxChannelResponse_enabled :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelResponse_enabled = Lens.lens (\APNSVoipSandboxChannelResponse' {enabled} -> enabled) (\s@APNSVoipSandboxChannelResponse' {} a -> s {enabled = a} :: APNSVoipSandboxChannelResponse)

-- | The current version of the APNs VoIP sandbox channel.
aPNSVoipSandboxChannelResponse_version :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Int)
aPNSVoipSandboxChannelResponse_version = Lens.lens (\APNSVoipSandboxChannelResponse' {version} -> version) (\s@APNSVoipSandboxChannelResponse' {} a -> s {version = a} :: APNSVoipSandboxChannelResponse)

-- | Specifies whether the APNs VoIP sandbox channel is archived.
aPNSVoipSandboxChannelResponse_isArchived :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipSandboxChannelResponse_isArchived = Lens.lens (\APNSVoipSandboxChannelResponse' {isArchived} -> isArchived) (\s@APNSVoipSandboxChannelResponse' {} a -> s {isArchived = a} :: APNSVoipSandboxChannelResponse)

-- | The user who last modified the APNs VoIP sandbox channel.
aPNSVoipSandboxChannelResponse_lastModifiedBy :: Lens.Lens' APNSVoipSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipSandboxChannelResponse_lastModifiedBy = Lens.lens (\APNSVoipSandboxChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSVoipSandboxChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSVoipSandboxChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
aPNSVoipSandboxChannelResponse_platform :: Lens.Lens' APNSVoipSandboxChannelResponse Prelude.Text
aPNSVoipSandboxChannelResponse_platform = Lens.lens (\APNSVoipSandboxChannelResponse' {platform} -> platform) (\s@APNSVoipSandboxChannelResponse' {} a -> s {platform = a} :: APNSVoipSandboxChannelResponse)

instance
  Prelude.FromJSON
    APNSVoipSandboxChannelResponse
  where
  parseJSON =
    Prelude.withObject
      "APNSVoipSandboxChannelResponse"
      ( \x ->
          APNSVoipSandboxChannelResponse'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "ApplicationId")
            Prelude.<*> (x Prelude..:? "DefaultAuthenticationMethod")
            Prelude.<*> (x Prelude..:? "HasCredential")
            Prelude.<*> (x Prelude..:? "HasTokenKey")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "IsArchived")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
            Prelude.<*> (x Prelude..: "Platform")
      )

instance
  Prelude.Hashable
    APNSVoipSandboxChannelResponse

instance
  Prelude.NFData
    APNSVoipSandboxChannelResponse
