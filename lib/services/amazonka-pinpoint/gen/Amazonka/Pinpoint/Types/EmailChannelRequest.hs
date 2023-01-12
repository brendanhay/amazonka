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
-- Module      : Amazonka.Pinpoint.Types.EmailChannelRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EmailChannelRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the status and settings of the email channel for an
-- application.
--
-- /See:/ 'newEmailChannelRequest' smart constructor.
data EmailChannelRequest = EmailChannelRequest'
  { -- | The
    -- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
    -- that you want to apply to messages that you send through the channel.
    configurationSet :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to enable the email channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that you
    -- want Amazon Pinpoint to use when it submits email-related event data for
    -- the channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The verified email address that you want to send email from when you
    -- send email through the channel.
    fromAddress :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
    -- Simple Email Service (Amazon SES), that you want to use when you send
    -- email through the channel.
    identity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSet', 'emailChannelRequest_configurationSet' - The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that you want to apply to messages that you send through the channel.
--
-- 'enabled', 'emailChannelRequest_enabled' - Specifies whether to enable the email channel for the application.
--
-- 'roleArn', 'emailChannelRequest_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that you
-- want Amazon Pinpoint to use when it submits email-related event data for
-- the channel.
--
-- 'fromAddress', 'emailChannelRequest_fromAddress' - The verified email address that you want to send email from when you
-- send email through the channel.
--
-- 'identity', 'emailChannelRequest_identity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that you want to use when you send
-- email through the channel.
newEmailChannelRequest ::
  -- | 'fromAddress'
  Prelude.Text ->
  -- | 'identity'
  Prelude.Text ->
  EmailChannelRequest
newEmailChannelRequest pFromAddress_ pIdentity_ =
  EmailChannelRequest'
    { configurationSet =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      fromAddress = pFromAddress_,
      identity = pIdentity_
    }

-- | The
-- <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set>
-- that you want to apply to messages that you send through the channel.
emailChannelRequest_configurationSet :: Lens.Lens' EmailChannelRequest (Prelude.Maybe Prelude.Text)
emailChannelRequest_configurationSet = Lens.lens (\EmailChannelRequest' {configurationSet} -> configurationSet) (\s@EmailChannelRequest' {} a -> s {configurationSet = a} :: EmailChannelRequest)

-- | Specifies whether to enable the email channel for the application.
emailChannelRequest_enabled :: Lens.Lens' EmailChannelRequest (Prelude.Maybe Prelude.Bool)
emailChannelRequest_enabled = Lens.lens (\EmailChannelRequest' {enabled} -> enabled) (\s@EmailChannelRequest' {} a -> s {enabled = a} :: EmailChannelRequest)

-- | The ARN of the AWS Identity and Access Management (IAM) role that you
-- want Amazon Pinpoint to use when it submits email-related event data for
-- the channel.
emailChannelRequest_roleArn :: Lens.Lens' EmailChannelRequest (Prelude.Maybe Prelude.Text)
emailChannelRequest_roleArn = Lens.lens (\EmailChannelRequest' {roleArn} -> roleArn) (\s@EmailChannelRequest' {} a -> s {roleArn = a} :: EmailChannelRequest)

-- | The verified email address that you want to send email from when you
-- send email through the channel.
emailChannelRequest_fromAddress :: Lens.Lens' EmailChannelRequest Prelude.Text
emailChannelRequest_fromAddress = Lens.lens (\EmailChannelRequest' {fromAddress} -> fromAddress) (\s@EmailChannelRequest' {} a -> s {fromAddress = a} :: EmailChannelRequest)

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon
-- Simple Email Service (Amazon SES), that you want to use when you send
-- email through the channel.
emailChannelRequest_identity :: Lens.Lens' EmailChannelRequest Prelude.Text
emailChannelRequest_identity = Lens.lens (\EmailChannelRequest' {identity} -> identity) (\s@EmailChannelRequest' {} a -> s {identity = a} :: EmailChannelRequest)

instance Prelude.Hashable EmailChannelRequest where
  hashWithSalt _salt EmailChannelRequest' {..} =
    _salt `Prelude.hashWithSalt` configurationSet
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` fromAddress
      `Prelude.hashWithSalt` identity

instance Prelude.NFData EmailChannelRequest where
  rnf EmailChannelRequest' {..} =
    Prelude.rnf configurationSet
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf fromAddress
      `Prelude.seq` Prelude.rnf identity

instance Data.ToJSON EmailChannelRequest where
  toJSON EmailChannelRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationSet" Data..=)
              Prelude.<$> configurationSet,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("FromAddress" Data..= fromAddress),
            Prelude.Just ("Identity" Data..= identity)
          ]
      )
