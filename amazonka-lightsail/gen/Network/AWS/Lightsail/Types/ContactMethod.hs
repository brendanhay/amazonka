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
-- Module      : Network.AWS.Lightsail.Types.ContactMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethod where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContactMethodStatus
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType

-- | Describes a contact method.
--
-- A contact method is a way to send you notifications. For more
-- information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
--
-- /See:/ 'newContactMethod' smart constructor.
data ContactMethod = ContactMethod'
  { -- | The current status of the contact method.
    --
    -- A contact method has the following possible status:
    --
    -- -   @PendingVerification@ - The contact method has not yet been
    --     verified, and the verification has not yet expired.
    --
    -- -   @Valid@ - The contact method has been verified.
    --
    -- -   @InValid@ - An attempt was made to verify the contact method, but
    --     the verification has expired.
    status :: Core.Maybe ContactMethodStatus,
    -- | The destination of the contact method, such as an email address or a
    -- mobile phone number.
    contactEndpoint :: Core.Maybe Core.Text,
    -- | The timestamp when the contact method was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the contact method.
    arn :: Core.Maybe Core.Text,
    -- | The Lightsail resource type (e.g., @ContactMethod@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail contact method. This code enables
    -- our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The name of the contact method.
    name :: Core.Maybe Core.Text,
    -- | The protocol of the contact method, such as email or SMS (text
    -- messaging).
    protocol :: Core.Maybe ContactProtocol,
    location :: Core.Maybe ResourceLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContactMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'contactMethod_status' - The current status of the contact method.
--
-- A contact method has the following possible status:
--
-- -   @PendingVerification@ - The contact method has not yet been
--     verified, and the verification has not yet expired.
--
-- -   @Valid@ - The contact method has been verified.
--
-- -   @InValid@ - An attempt was made to verify the contact method, but
--     the verification has expired.
--
-- 'contactEndpoint', 'contactMethod_contactEndpoint' - The destination of the contact method, such as an email address or a
-- mobile phone number.
--
-- 'createdAt', 'contactMethod_createdAt' - The timestamp when the contact method was created.
--
-- 'arn', 'contactMethod_arn' - The Amazon Resource Name (ARN) of the contact method.
--
-- 'resourceType', 'contactMethod_resourceType' - The Lightsail resource type (e.g., @ContactMethod@).
--
-- 'supportCode', 'contactMethod_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail contact method. This code enables
-- our support team to look up your Lightsail information more easily.
--
-- 'name', 'contactMethod_name' - The name of the contact method.
--
-- 'protocol', 'contactMethod_protocol' - The protocol of the contact method, such as email or SMS (text
-- messaging).
--
-- 'location', 'contactMethod_location' - Undocumented member.
newContactMethod ::
  ContactMethod
newContactMethod =
  ContactMethod'
    { status = Core.Nothing,
      contactEndpoint = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      name = Core.Nothing,
      protocol = Core.Nothing,
      location = Core.Nothing
    }

-- | The current status of the contact method.
--
-- A contact method has the following possible status:
--
-- -   @PendingVerification@ - The contact method has not yet been
--     verified, and the verification has not yet expired.
--
-- -   @Valid@ - The contact method has been verified.
--
-- -   @InValid@ - An attempt was made to verify the contact method, but
--     the verification has expired.
contactMethod_status :: Lens.Lens' ContactMethod (Core.Maybe ContactMethodStatus)
contactMethod_status = Lens.lens (\ContactMethod' {status} -> status) (\s@ContactMethod' {} a -> s {status = a} :: ContactMethod)

-- | The destination of the contact method, such as an email address or a
-- mobile phone number.
contactMethod_contactEndpoint :: Lens.Lens' ContactMethod (Core.Maybe Core.Text)
contactMethod_contactEndpoint = Lens.lens (\ContactMethod' {contactEndpoint} -> contactEndpoint) (\s@ContactMethod' {} a -> s {contactEndpoint = a} :: ContactMethod)

-- | The timestamp when the contact method was created.
contactMethod_createdAt :: Lens.Lens' ContactMethod (Core.Maybe Core.UTCTime)
contactMethod_createdAt = Lens.lens (\ContactMethod' {createdAt} -> createdAt) (\s@ContactMethod' {} a -> s {createdAt = a} :: ContactMethod) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the contact method.
contactMethod_arn :: Lens.Lens' ContactMethod (Core.Maybe Core.Text)
contactMethod_arn = Lens.lens (\ContactMethod' {arn} -> arn) (\s@ContactMethod' {} a -> s {arn = a} :: ContactMethod)

-- | The Lightsail resource type (e.g., @ContactMethod@).
contactMethod_resourceType :: Lens.Lens' ContactMethod (Core.Maybe ResourceType)
contactMethod_resourceType = Lens.lens (\ContactMethod' {resourceType} -> resourceType) (\s@ContactMethod' {} a -> s {resourceType = a} :: ContactMethod)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail contact method. This code enables
-- our support team to look up your Lightsail information more easily.
contactMethod_supportCode :: Lens.Lens' ContactMethod (Core.Maybe Core.Text)
contactMethod_supportCode = Lens.lens (\ContactMethod' {supportCode} -> supportCode) (\s@ContactMethod' {} a -> s {supportCode = a} :: ContactMethod)

-- | The name of the contact method.
contactMethod_name :: Lens.Lens' ContactMethod (Core.Maybe Core.Text)
contactMethod_name = Lens.lens (\ContactMethod' {name} -> name) (\s@ContactMethod' {} a -> s {name = a} :: ContactMethod)

-- | The protocol of the contact method, such as email or SMS (text
-- messaging).
contactMethod_protocol :: Lens.Lens' ContactMethod (Core.Maybe ContactProtocol)
contactMethod_protocol = Lens.lens (\ContactMethod' {protocol} -> protocol) (\s@ContactMethod' {} a -> s {protocol = a} :: ContactMethod)

-- | Undocumented member.
contactMethod_location :: Lens.Lens' ContactMethod (Core.Maybe ResourceLocation)
contactMethod_location = Lens.lens (\ContactMethod' {location} -> location) (\s@ContactMethod' {} a -> s {location = a} :: ContactMethod)

instance Core.FromJSON ContactMethod where
  parseJSON =
    Core.withObject
      "ContactMethod"
      ( \x ->
          ContactMethod'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "contactEndpoint")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "protocol")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable ContactMethod

instance Core.NFData ContactMethod
