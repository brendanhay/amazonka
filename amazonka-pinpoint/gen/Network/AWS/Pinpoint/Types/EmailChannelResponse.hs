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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the status and settings of the email channel
-- for an application.
--
-- /See:/ 'newEmailChannelResponse' smart constructor.
data EmailChannelResponse = EmailChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the email channel was last
    -- modified.
    lastModifiedDate :: Core.Maybe Core.Text,
    -- | The unique identifier for the application that the email channel applies
    -- to.
    applicationId :: Core.Maybe Core.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon
    -- Pinpoint uses to submit email-related event data for the channel.
    roleArn :: Core.Maybe Core.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
    -- Simple Email Service (Amazon SES), that\'s used when you send email
    -- through the channel.
    identity :: Core.Maybe Core.Text,
    -- | (Deprecated) An identifier for the email channel. This property is
    -- retained only for backward compatibility.
    id :: Core.Maybe Core.Text,
    -- | The date and time, in ISO 8601 format, when the email channel was
    -- enabled.
    creationDate :: Core.Maybe Core.Text,
    -- | Specifies whether the email channel is enabled for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The current version of the email channel.
    version :: Core.Maybe Core.Int,
    -- | The maximum number of emails that can be sent through the channel each
    -- second.
    messagesPerSecond :: Core.Maybe Core.Int,
    -- | Specifies whether the email channel is archived.
    isArchived :: Core.Maybe Core.Bool,
    -- | The verified email address that email is sent from when you send email
    -- through the channel.
    fromAddress :: Core.Maybe Core.Text,
    -- | The user who last modified the email channel.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | The
    -- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
    -- that\'s applied to messages that are sent through the channel.
    configurationSet :: Core.Maybe Core.Text,
    -- | The type of messaging or notification platform for the channel. For the
    -- email channel, this value is EMAIL.
    platform :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  EmailChannelResponse
newEmailChannelResponse pPlatform_ =
  EmailChannelResponse'
    { lastModifiedDate =
        Core.Nothing,
      applicationId = Core.Nothing,
      roleArn = Core.Nothing,
      hasCredential = Core.Nothing,
      identity = Core.Nothing,
      id = Core.Nothing,
      creationDate = Core.Nothing,
      enabled = Core.Nothing,
      version = Core.Nothing,
      messagesPerSecond = Core.Nothing,
      isArchived = Core.Nothing,
      fromAddress = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      configurationSet = Core.Nothing,
      platform = pPlatform_
    }

-- | The date and time, in ISO 8601 format, when the email channel was last
-- modified.
emailChannelResponse_lastModifiedDate :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_lastModifiedDate = Lens.lens (\EmailChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@EmailChannelResponse' {} a -> s {lastModifiedDate = a} :: EmailChannelResponse)

-- | The unique identifier for the application that the email channel applies
-- to.
emailChannelResponse_applicationId :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_applicationId = Lens.lens (\EmailChannelResponse' {applicationId} -> applicationId) (\s@EmailChannelResponse' {} a -> s {applicationId = a} :: EmailChannelResponse)

-- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon
-- Pinpoint uses to submit email-related event data for the channel.
emailChannelResponse_roleArn :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_roleArn = Lens.lens (\EmailChannelResponse' {roleArn} -> roleArn) (\s@EmailChannelResponse' {} a -> s {roleArn = a} :: EmailChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
emailChannelResponse_hasCredential :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Bool)
emailChannelResponse_hasCredential = Lens.lens (\EmailChannelResponse' {hasCredential} -> hasCredential) (\s@EmailChannelResponse' {} a -> s {hasCredential = a} :: EmailChannelResponse)

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that\'s used when you send email
-- through the channel.
emailChannelResponse_identity :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_identity = Lens.lens (\EmailChannelResponse' {identity} -> identity) (\s@EmailChannelResponse' {} a -> s {identity = a} :: EmailChannelResponse)

-- | (Deprecated) An identifier for the email channel. This property is
-- retained only for backward compatibility.
emailChannelResponse_id :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_id = Lens.lens (\EmailChannelResponse' {id} -> id) (\s@EmailChannelResponse' {} a -> s {id = a} :: EmailChannelResponse)

-- | The date and time, in ISO 8601 format, when the email channel was
-- enabled.
emailChannelResponse_creationDate :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_creationDate = Lens.lens (\EmailChannelResponse' {creationDate} -> creationDate) (\s@EmailChannelResponse' {} a -> s {creationDate = a} :: EmailChannelResponse)

-- | Specifies whether the email channel is enabled for the application.
emailChannelResponse_enabled :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Bool)
emailChannelResponse_enabled = Lens.lens (\EmailChannelResponse' {enabled} -> enabled) (\s@EmailChannelResponse' {} a -> s {enabled = a} :: EmailChannelResponse)

-- | The current version of the email channel.
emailChannelResponse_version :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Int)
emailChannelResponse_version = Lens.lens (\EmailChannelResponse' {version} -> version) (\s@EmailChannelResponse' {} a -> s {version = a} :: EmailChannelResponse)

-- | The maximum number of emails that can be sent through the channel each
-- second.
emailChannelResponse_messagesPerSecond :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Int)
emailChannelResponse_messagesPerSecond = Lens.lens (\EmailChannelResponse' {messagesPerSecond} -> messagesPerSecond) (\s@EmailChannelResponse' {} a -> s {messagesPerSecond = a} :: EmailChannelResponse)

-- | Specifies whether the email channel is archived.
emailChannelResponse_isArchived :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Bool)
emailChannelResponse_isArchived = Lens.lens (\EmailChannelResponse' {isArchived} -> isArchived) (\s@EmailChannelResponse' {} a -> s {isArchived = a} :: EmailChannelResponse)

-- | The verified email address that email is sent from when you send email
-- through the channel.
emailChannelResponse_fromAddress :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_fromAddress = Lens.lens (\EmailChannelResponse' {fromAddress} -> fromAddress) (\s@EmailChannelResponse' {} a -> s {fromAddress = a} :: EmailChannelResponse)

-- | The user who last modified the email channel.
emailChannelResponse_lastModifiedBy :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_lastModifiedBy = Lens.lens (\EmailChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@EmailChannelResponse' {} a -> s {lastModifiedBy = a} :: EmailChannelResponse)

-- | The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that\'s applied to messages that are sent through the channel.
emailChannelResponse_configurationSet :: Lens.Lens' EmailChannelResponse (Core.Maybe Core.Text)
emailChannelResponse_configurationSet = Lens.lens (\EmailChannelResponse' {configurationSet} -> configurationSet) (\s@EmailChannelResponse' {} a -> s {configurationSet = a} :: EmailChannelResponse)

-- | The type of messaging or notification platform for the channel. For the
-- email channel, this value is EMAIL.
emailChannelResponse_platform :: Lens.Lens' EmailChannelResponse Core.Text
emailChannelResponse_platform = Lens.lens (\EmailChannelResponse' {platform} -> platform) (\s@EmailChannelResponse' {} a -> s {platform = a} :: EmailChannelResponse)

instance Core.FromJSON EmailChannelResponse where
  parseJSON =
    Core.withObject
      "EmailChannelResponse"
      ( \x ->
          EmailChannelResponse'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "HasCredential")
            Core.<*> (x Core..:? "Identity")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "MessagesPerSecond")
            Core.<*> (x Core..:? "IsArchived")
            Core.<*> (x Core..:? "FromAddress")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "ConfigurationSet")
            Core.<*> (x Core..: "Platform")
      )

instance Core.Hashable EmailChannelResponse

instance Core.NFData EmailChannelResponse
