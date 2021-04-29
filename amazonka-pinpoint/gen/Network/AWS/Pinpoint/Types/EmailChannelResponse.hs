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
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailChannelResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the status and settings of the email channel
-- for an application.
--
-- /See:/ 'newEmailChannelResponse' smart constructor.
data EmailChannelResponse = EmailChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the email channel was last
    -- modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application that the email channel applies
    -- to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon
    -- Pinpoint uses to submit email-related event data for the channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
    -- Simple Email Service (Amazon SES), that\'s used when you send email
    -- through the channel.
    identity :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) An identifier for the email channel. This property is
    -- retained only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the email channel was
    -- enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the email channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The current version of the email channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of emails that can be sent through the channel each
    -- second.
    messagesPerSecond :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the email channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The verified email address that email is sent from when you send email
    -- through the channel.
    fromAddress :: Prelude.Maybe Prelude.Text,
    -- | The user who last modified the email channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
    -- that\'s applied to messages that are sent through the channel.
    configurationSet :: Prelude.Maybe Prelude.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- email channel, this value is EMAIL.
    platform :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EmailChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'emailChannelResponse_lastModifiedDate' - The date and time, in ISO 8601 format, when the email channel was last
-- modified.
--
-- 'applicationId', 'emailChannelResponse_applicationId' - The unique identifier for the application that the email channel applies
-- to.
--
-- 'roleArn', 'emailChannelResponse_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that Amazon
-- Pinpoint uses to submit email-related event data for the channel.
--
-- 'hasCredential', 'emailChannelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- 'identity', 'emailChannelResponse_identity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that\'s used when you send email
-- through the channel.
--
-- 'id', 'emailChannelResponse_id' - (Deprecated) An identifier for the email channel. This property is
-- retained only for backward compatibility.
--
-- 'creationDate', 'emailChannelResponse_creationDate' - The date and time, in ISO 8601 format, when the email channel was
-- enabled.
--
-- 'enabled', 'emailChannelResponse_enabled' - Specifies whether the email channel is enabled for the application.
--
-- 'version', 'emailChannelResponse_version' - The current version of the email channel.
--
-- 'messagesPerSecond', 'emailChannelResponse_messagesPerSecond' - The maximum number of emails that can be sent through the channel each
-- second.
--
-- 'isArchived', 'emailChannelResponse_isArchived' - Specifies whether the email channel is archived.
--
-- 'fromAddress', 'emailChannelResponse_fromAddress' - The verified email address that email is sent from when you send email
-- through the channel.
--
-- 'lastModifiedBy', 'emailChannelResponse_lastModifiedBy' - The user who last modified the email channel.
--
-- 'configurationSet', 'emailChannelResponse_configurationSet' - The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that\'s applied to messages that are sent through the channel.
--
-- 'platform', 'emailChannelResponse_platform' - The type of messaging or notification platform for the channel. For the
-- email channel, this value is EMAIL.
newEmailChannelResponse ::
  -- | 'platform'
  Prelude.Text ->
  EmailChannelResponse
newEmailChannelResponse pPlatform_ =
  EmailChannelResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      applicationId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      hasCredential = Prelude.Nothing,
      identity = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      version = Prelude.Nothing,
      messagesPerSecond = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      fromAddress = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      configurationSet = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The date and time, in ISO 8601 format, when the email channel was last
-- modified.
emailChannelResponse_lastModifiedDate :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_lastModifiedDate = Lens.lens (\EmailChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@EmailChannelResponse' {} a -> s {lastModifiedDate = a} :: EmailChannelResponse)

-- | The unique identifier for the application that the email channel applies
-- to.
emailChannelResponse_applicationId :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_applicationId = Lens.lens (\EmailChannelResponse' {applicationId} -> applicationId) (\s@EmailChannelResponse' {} a -> s {applicationId = a} :: EmailChannelResponse)

-- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon
-- Pinpoint uses to submit email-related event data for the channel.
emailChannelResponse_roleArn :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_roleArn = Lens.lens (\EmailChannelResponse' {roleArn} -> roleArn) (\s@EmailChannelResponse' {} a -> s {roleArn = a} :: EmailChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
emailChannelResponse_hasCredential :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Bool)
emailChannelResponse_hasCredential = Lens.lens (\EmailChannelResponse' {hasCredential} -> hasCredential) (\s@EmailChannelResponse' {} a -> s {hasCredential = a} :: EmailChannelResponse)

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that\'s used when you send email
-- through the channel.
emailChannelResponse_identity :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_identity = Lens.lens (\EmailChannelResponse' {identity} -> identity) (\s@EmailChannelResponse' {} a -> s {identity = a} :: EmailChannelResponse)

-- | (Deprecated) An identifier for the email channel. This property is
-- retained only for backward compatibility.
emailChannelResponse_id :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_id = Lens.lens (\EmailChannelResponse' {id} -> id) (\s@EmailChannelResponse' {} a -> s {id = a} :: EmailChannelResponse)

-- | The date and time, in ISO 8601 format, when the email channel was
-- enabled.
emailChannelResponse_creationDate :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_creationDate = Lens.lens (\EmailChannelResponse' {creationDate} -> creationDate) (\s@EmailChannelResponse' {} a -> s {creationDate = a} :: EmailChannelResponse)

-- | Specifies whether the email channel is enabled for the application.
emailChannelResponse_enabled :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Bool)
emailChannelResponse_enabled = Lens.lens (\EmailChannelResponse' {enabled} -> enabled) (\s@EmailChannelResponse' {} a -> s {enabled = a} :: EmailChannelResponse)

-- | The current version of the email channel.
emailChannelResponse_version :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Int)
emailChannelResponse_version = Lens.lens (\EmailChannelResponse' {version} -> version) (\s@EmailChannelResponse' {} a -> s {version = a} :: EmailChannelResponse)

-- | The maximum number of emails that can be sent through the channel each
-- second.
emailChannelResponse_messagesPerSecond :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Int)
emailChannelResponse_messagesPerSecond = Lens.lens (\EmailChannelResponse' {messagesPerSecond} -> messagesPerSecond) (\s@EmailChannelResponse' {} a -> s {messagesPerSecond = a} :: EmailChannelResponse)

-- | Specifies whether the email channel is archived.
emailChannelResponse_isArchived :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Bool)
emailChannelResponse_isArchived = Lens.lens (\EmailChannelResponse' {isArchived} -> isArchived) (\s@EmailChannelResponse' {} a -> s {isArchived = a} :: EmailChannelResponse)

-- | The verified email address that email is sent from when you send email
-- through the channel.
emailChannelResponse_fromAddress :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_fromAddress = Lens.lens (\EmailChannelResponse' {fromAddress} -> fromAddress) (\s@EmailChannelResponse' {} a -> s {fromAddress = a} :: EmailChannelResponse)

-- | The user who last modified the email channel.
emailChannelResponse_lastModifiedBy :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_lastModifiedBy = Lens.lens (\EmailChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@EmailChannelResponse' {} a -> s {lastModifiedBy = a} :: EmailChannelResponse)

-- | The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that\'s applied to messages that are sent through the channel.
emailChannelResponse_configurationSet :: Lens.Lens' EmailChannelResponse (Prelude.Maybe Prelude.Text)
emailChannelResponse_configurationSet = Lens.lens (\EmailChannelResponse' {configurationSet} -> configurationSet) (\s@EmailChannelResponse' {} a -> s {configurationSet = a} :: EmailChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- email channel, this value is EMAIL.
emailChannelResponse_platform :: Lens.Lens' EmailChannelResponse Prelude.Text
emailChannelResponse_platform = Lens.lens (\EmailChannelResponse' {platform} -> platform) (\s@EmailChannelResponse' {} a -> s {platform = a} :: EmailChannelResponse)

instance Prelude.FromJSON EmailChannelResponse where
  parseJSON =
    Prelude.withObject
      "EmailChannelResponse"
      ( \x ->
          EmailChannelResponse'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "ApplicationId")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "HasCredential")
            Prelude.<*> (x Prelude..:? "Identity")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "MessagesPerSecond")
            Prelude.<*> (x Prelude..:? "IsArchived")
            Prelude.<*> (x Prelude..:? "FromAddress")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
            Prelude.<*> (x Prelude..:? "ConfigurationSet")
            Prelude.<*> (x Prelude..: "Platform")
      )

instance Prelude.Hashable EmailChannelResponse

instance Prelude.NFData EmailChannelResponse
