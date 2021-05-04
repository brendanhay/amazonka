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
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) VoIP channel for an application.
--
-- /See:/ 'newAPNSVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { -- | The date and time when the APNs VoIP channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application that the APNs VoIP channel
    -- applies to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the APNs VoIP channel is configured to communicate
    -- with APNs by using APNs tokens. To provide an authentication key for
    -- APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs VoIP channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs VoIP channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs VoIP channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The current version of the APNs VoIP channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the APNs VoIP channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the APNs VoIP channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs VoIP channel, this value is APNS_VOIP.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'APNSVoipChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'aPNSVoipChannelResponse_lastModifiedDate' - The date and time when the APNs VoIP channel was last modified.
--
-- 'applicationId', 'aPNSVoipChannelResponse_applicationId' - The unique identifier for the application that the APNs VoIP channel
-- applies to.
--
-- 'defaultAuthenticationMethod', 'aPNSVoipChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
--
-- 'hasCredential', 'aPNSVoipChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'hasTokenKey', 'aPNSVoipChannelResponse_hasTokenKey' - Specifies whether the APNs VoIP channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
--
-- 'id', 'aPNSVoipChannelResponse_id' - (Deprecated) An identifier for the APNs VoIP channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'aPNSVoipChannelResponse_creationDate' - The date and time when the APNs VoIP channel was enabled.
--
-- 'enabled', 'aPNSVoipChannelResponse_enabled' - Specifies whether the APNs VoIP channel is enabled for the application.
--
-- 'version', 'aPNSVoipChannelResponse_version' - The current version of the APNs VoIP channel.
--
-- 'isArchived', 'aPNSVoipChannelResponse_isArchived' - Specifies whether the APNs VoIP channel is archived.
--
-- 'lastModifiedBy', 'aPNSVoipChannelResponse_lastModifiedBy' - The user who last modified the APNs VoIP channel.
--
-- 'platform', 'aPNSVoipChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs VoIP channel, this value is APNS_VOIP.
newAPNSVoipChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  APNSVoipChannelResponse
newAPNSVoipChannelResponse pPlatform_ =
  APNSVoipChannelResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      applicationId = Prelude.Nothing,
      defaultAuthenticationMethod = Prelude.Nothing,
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

-- | The date and time when the APNs VoIP channel was last modified.
aPNSVoipChannelResponse_lastModifiedDate :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipChannelResponse_lastModifiedDate = Lens.lens (\APNSVoipChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSVoipChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSVoipChannelResponse)

-- | The unique identifier for the application that the APNs VoIP channel
-- applies to.
aPNSVoipChannelResponse_applicationId :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipChannelResponse_applicationId = Lens.lens (\APNSVoipChannelResponse' {applicationId} -> applicationId) (\s@APNSVoipChannelResponse' {} a -> s {applicationId = a} :: APNSVoipChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with APNs for this channel, key or certificate.
aPNSVoipChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSVoipChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSVoipChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSVoipChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
aPNSVoipChannelResponse_hasCredential :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipChannelResponse_hasCredential = Lens.lens (\APNSVoipChannelResponse' {hasCredential} -> hasCredential) (\s@APNSVoipChannelResponse' {} a -> s {hasCredential = a} :: APNSVoipChannelResponse)

-- | Specifies whether the APNs VoIP channel is configured to communicate
-- with APNs by using APNs tokens. To provide an authentication key for
-- APNs tokens, set the TokenKey property of the channel.
aPNSVoipChannelResponse_hasTokenKey :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipChannelResponse_hasTokenKey = Lens.lens (\APNSVoipChannelResponse' {hasTokenKey} -> hasTokenKey) (\s@APNSVoipChannelResponse' {} a -> s {hasTokenKey = a} :: APNSVoipChannelResponse)

-- | (Deprecated) An identifier for the APNs VoIP channel. This property is
-- retained only for backward compatibility.
aPNSVoipChannelResponse_id :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipChannelResponse_id = Lens.lens (\APNSVoipChannelResponse' {id} -> id) (\s@APNSVoipChannelResponse' {} a -> s {id = a} :: APNSVoipChannelResponse)

-- | The date and time when the APNs VoIP channel was enabled.
aPNSVoipChannelResponse_creationDate :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipChannelResponse_creationDate = Lens.lens (\APNSVoipChannelResponse' {creationDate} -> creationDate) (\s@APNSVoipChannelResponse' {} a -> s {creationDate = a} :: APNSVoipChannelResponse)

-- | Specifies whether the APNs VoIP channel is enabled for the application.
aPNSVoipChannelResponse_enabled :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipChannelResponse_enabled = Lens.lens (\APNSVoipChannelResponse' {enabled} -> enabled) (\s@APNSVoipChannelResponse' {} a -> s {enabled = a} :: APNSVoipChannelResponse)

-- | The current version of the APNs VoIP channel.
aPNSVoipChannelResponse_version :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Int)
aPNSVoipChannelResponse_version = Lens.lens (\APNSVoipChannelResponse' {version} -> version) (\s@APNSVoipChannelResponse' {} a -> s {version = a} :: APNSVoipChannelResponse)

-- | Specifies whether the APNs VoIP channel is archived.
aPNSVoipChannelResponse_isArchived :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSVoipChannelResponse_isArchived = Lens.lens (\APNSVoipChannelResponse' {isArchived} -> isArchived) (\s@APNSVoipChannelResponse' {} a -> s {isArchived = a} :: APNSVoipChannelResponse)

-- | The user who last modified the APNs VoIP channel.
aPNSVoipChannelResponse_lastModifiedBy :: Lens.Lens' APNSVoipChannelResponse (Prelude.Maybe Prelude.Text)
aPNSVoipChannelResponse_lastModifiedBy = Lens.lens (\APNSVoipChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSVoipChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSVoipChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs VoIP channel, this value is APNS_VOIP.
aPNSVoipChannelResponse_platform :: Lens.Lens' APNSVoipChannelResponse Prelude.Text
aPNSVoipChannelResponse_platform = Lens.lens (\APNSVoipChannelResponse' {platform} -> platform) (\s@APNSVoipChannelResponse' {} a -> s {platform = a} :: APNSVoipChannelResponse)

instance Prelude.FromJSON APNSVoipChannelResponse where
  parseJSON =
    Prelude.withObject
      "APNSVoipChannelResponse"
      ( \x ->
          APNSVoipChannelResponse'
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

instance Prelude.Hashable APNSVoipChannelResponse

instance Prelude.NFData APNSVoipChannelResponse
