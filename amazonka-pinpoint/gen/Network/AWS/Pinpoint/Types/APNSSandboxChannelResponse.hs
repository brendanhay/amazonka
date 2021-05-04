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
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the status and settings of the APNs (Apple
-- Push Notification service) sandbox channel for an application.
--
-- /See:/ 'newAPNSSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { -- | The date and time when the APNs sandbox channel was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application that the APNs sandbox channel
    -- applies to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The default authentication method that Amazon Pinpoint uses to
    -- authenticate with the APNs sandbox environment for this channel, key or
    -- certificate.
    defaultAuthenticationMethod :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the APNs sandbox channel is configured to communicate
    -- with APNs by using APNs tokens. To provide an authentication key for
    -- APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Prelude.Maybe Prelude.Bool,
    -- | (Deprecated) An identifier for the APNs sandbox channel. This property
    -- is retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the APNs sandbox channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the APNs sandbox channel is enabled for the
    -- application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The current version of the APNs sandbox channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the APNs sandbox channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The user who last modified the APNs sandbox channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- APNs sandbox channel, this value is APNS_SANDBOX.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'APNSSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'aPNSSandboxChannelResponse_lastModifiedDate' - The date and time when the APNs sandbox channel was last modified.
--
-- 'applicationId', 'aPNSSandboxChannelResponse_applicationId' - The unique identifier for the application that the APNs sandbox channel
-- applies to.
--
-- 'defaultAuthenticationMethod', 'aPNSSandboxChannelResponse_defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
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
-- 'creationDate', 'aPNSSandboxChannelResponse_creationDate' - The date and time when the APNs sandbox channel was enabled.
--
-- 'enabled', 'aPNSSandboxChannelResponse_enabled' - Specifies whether the APNs sandbox channel is enabled for the
-- application.
--
-- 'version', 'aPNSSandboxChannelResponse_version' - The current version of the APNs sandbox channel.
--
-- 'isArchived', 'aPNSSandboxChannelResponse_isArchived' - Specifies whether the APNs sandbox channel is archived.
--
-- 'lastModifiedBy', 'aPNSSandboxChannelResponse_lastModifiedBy' - The user who last modified the APNs sandbox channel.
--
-- 'platform', 'aPNSSandboxChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- APNs sandbox channel, this value is APNS_SANDBOX.
newAPNSSandboxChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  APNSSandboxChannelResponse
newAPNSSandboxChannelResponse pPlatform_ =
  APNSSandboxChannelResponse'
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

-- | The date and time when the APNs sandbox channel was last modified.
aPNSSandboxChannelResponse_lastModifiedDate :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_lastModifiedDate = Lens.lens (\APNSSandboxChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@APNSSandboxChannelResponse' {} a -> s {lastModifiedDate = a} :: APNSSandboxChannelResponse)

-- | The unique identifier for the application that the APNs sandbox channel
-- applies to.
aPNSSandboxChannelResponse_applicationId :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_applicationId = Lens.lens (\APNSSandboxChannelResponse' {applicationId} -> applicationId) (\s@APNSSandboxChannelResponse' {} a -> s {applicationId = a} :: APNSSandboxChannelResponse)

-- | The default authentication method that Amazon Pinpoint uses to
-- authenticate with the APNs sandbox environment for this channel, key or
-- certificate.
aPNSSandboxChannelResponse_defaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_defaultAuthenticationMethod = Lens.lens (\APNSSandboxChannelResponse' {defaultAuthenticationMethod} -> defaultAuthenticationMethod) (\s@APNSSandboxChannelResponse' {} a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelResponse)

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

-- | The date and time when the APNs sandbox channel was enabled.
aPNSSandboxChannelResponse_creationDate :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_creationDate = Lens.lens (\APNSSandboxChannelResponse' {creationDate} -> creationDate) (\s@APNSSandboxChannelResponse' {} a -> s {creationDate = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is enabled for the
-- application.
aPNSSandboxChannelResponse_enabled :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelResponse_enabled = Lens.lens (\APNSSandboxChannelResponse' {enabled} -> enabled) (\s@APNSSandboxChannelResponse' {} a -> s {enabled = a} :: APNSSandboxChannelResponse)

-- | The current version of the APNs sandbox channel.
aPNSSandboxChannelResponse_version :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Int)
aPNSSandboxChannelResponse_version = Lens.lens (\APNSSandboxChannelResponse' {version} -> version) (\s@APNSSandboxChannelResponse' {} a -> s {version = a} :: APNSSandboxChannelResponse)

-- | Specifies whether the APNs sandbox channel is archived.
aPNSSandboxChannelResponse_isArchived :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Bool)
aPNSSandboxChannelResponse_isArchived = Lens.lens (\APNSSandboxChannelResponse' {isArchived} -> isArchived) (\s@APNSSandboxChannelResponse' {} a -> s {isArchived = a} :: APNSSandboxChannelResponse)

-- | The user who last modified the APNs sandbox channel.
aPNSSandboxChannelResponse_lastModifiedBy :: Lens.Lens' APNSSandboxChannelResponse (Prelude.Maybe Prelude.Text)
aPNSSandboxChannelResponse_lastModifiedBy = Lens.lens (\APNSSandboxChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@APNSSandboxChannelResponse' {} a -> s {lastModifiedBy = a} :: APNSSandboxChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- APNs sandbox channel, this value is APNS_SANDBOX.
aPNSSandboxChannelResponse_platform :: Lens.Lens' APNSSandboxChannelResponse Prelude.Text
aPNSSandboxChannelResponse_platform = Lens.lens (\APNSSandboxChannelResponse' {platform} -> platform) (\s@APNSSandboxChannelResponse' {} a -> s {platform = a} :: APNSSandboxChannelResponse)

instance Prelude.FromJSON APNSSandboxChannelResponse where
  parseJSON =
    Prelude.withObject
      "APNSSandboxChannelResponse"
      ( \x ->
          APNSSandboxChannelResponse'
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

instance Prelude.Hashable APNSSandboxChannelResponse

instance Prelude.NFData APNSSandboxChannelResponse
