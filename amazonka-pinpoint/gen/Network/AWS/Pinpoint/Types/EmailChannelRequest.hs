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
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailChannelRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the status and settings of the email channel for an
-- application.
--
-- /See:/ 'newEmailChannelRequest' smart constructor.
data EmailChannelRequest = EmailChannelRequest'
  { -- | The ARN of the AWS Identity and Access Management (IAM) role that you
    -- want Amazon Pinpoint to use when it submits email-related event data for
    -- the channel.
    roleArn :: Core.Maybe Core.Text,
    -- | Specifies whether to enable the email channel for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The
    -- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
    -- that you want to apply to messages that you send through the channel.
    configurationSet :: Core.Maybe Core.Text,
    -- | The verified email address that you want to send email from when you
    -- send email through the channel.
    fromAddress :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
    -- Simple Email Service (Amazon SES), that you want to use when you send
    -- email through the channel.
    identity :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmailChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'emailChannelRequest_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that you
-- want Amazon Pinpoint to use when it submits email-related event data for
-- the channel.
--
-- 'enabled', 'emailChannelRequest_enabled' - Specifies whether to enable the email channel for the application.
--
-- 'configurationSet', 'emailChannelRequest_configurationSet' - The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that you want to apply to messages that you send through the channel.
--
-- 'fromAddress', 'emailChannelRequest_fromAddress' - The verified email address that you want to send email from when you
-- send email through the channel.
--
-- 'identity', 'emailChannelRequest_identity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that you want to use when you send
-- email through the channel.
newEmailChannelRequest ::
  -- | 'fromAddress'
  Core.Text ->
  -- | 'identity'
  Core.Text ->
  EmailChannelRequest
newEmailChannelRequest pFromAddress_ pIdentity_ =
  EmailChannelRequest'
    { roleArn = Core.Nothing,
      enabled = Core.Nothing,
      configurationSet = Core.Nothing,
      fromAddress = pFromAddress_,
      identity = pIdentity_
    }

-- | The ARN of the AWS Identity and Access Management (IAM) role that you
-- want Amazon Pinpoint to use when it submits email-related event data for
-- the channel.
emailChannelRequest_roleArn :: Lens.Lens' EmailChannelRequest (Core.Maybe Core.Text)
emailChannelRequest_roleArn = Lens.lens (\EmailChannelRequest' {roleArn} -> roleArn) (\s@EmailChannelRequest' {} a -> s {roleArn = a} :: EmailChannelRequest)

-- | Specifies whether to enable the email channel for the application.
emailChannelRequest_enabled :: Lens.Lens' EmailChannelRequest (Core.Maybe Core.Bool)
emailChannelRequest_enabled = Lens.lens (\EmailChannelRequest' {enabled} -> enabled) (\s@EmailChannelRequest' {} a -> s {enabled = a} :: EmailChannelRequest)

-- | The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that you want to apply to messages that you send through the channel.
emailChannelRequest_configurationSet :: Lens.Lens' EmailChannelRequest (Core.Maybe Core.Text)
emailChannelRequest_configurationSet = Lens.lens (\EmailChannelRequest' {configurationSet} -> configurationSet) (\s@EmailChannelRequest' {} a -> s {configurationSet = a} :: EmailChannelRequest)

-- | The verified email address that you want to send email from when you
-- send email through the channel.
emailChannelRequest_fromAddress :: Lens.Lens' EmailChannelRequest Core.Text
emailChannelRequest_fromAddress = Lens.lens (\EmailChannelRequest' {fromAddress} -> fromAddress) (\s@EmailChannelRequest' {} a -> s {fromAddress = a} :: EmailChannelRequest)

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that you want to use when you send
-- email through the channel.
emailChannelRequest_identity :: Lens.Lens' EmailChannelRequest Core.Text
emailChannelRequest_identity = Lens.lens (\EmailChannelRequest' {identity} -> identity) (\s@EmailChannelRequest' {} a -> s {identity = a} :: EmailChannelRequest)

instance Core.Hashable EmailChannelRequest

instance Core.NFData EmailChannelRequest

instance Core.ToJSON EmailChannelRequest where
  toJSON EmailChannelRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleArn" Core..=) Core.<$> roleArn,
            ("Enabled" Core..=) Core.<$> enabled,
            ("ConfigurationSet" Core..=)
              Core.<$> configurationSet,
            Core.Just ("FromAddress" Core..= fromAddress),
            Core.Just ("Identity" Core..= identity)
          ]
      )
