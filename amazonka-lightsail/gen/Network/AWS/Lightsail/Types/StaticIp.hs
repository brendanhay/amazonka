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
-- Module      : Network.AWS.Lightsail.Types.StaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.StaticIp where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType

-- | Describes the static IP.
--
-- /See:/ 'newStaticIp' smart constructor.
data StaticIp = StaticIp'
  { -- | A Boolean value indicating whether the static IP is attached.
    isAttached :: Core.Maybe Core.Bool,
    -- | The timestamp when the static IP was created (e.g., @1479735304.222@).
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the static IP (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:StaticIp\/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@).
    arn :: Core.Maybe Core.Text,
    -- | The resource type (usually @StaticIp@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@).
    name :: Core.Maybe Core.Text,
    -- | The static IP address.
    ipAddress :: Core.Maybe Core.Text,
    -- | The instance where the static IP is attached (e.g.,
    -- @Amazon_Linux-1GB-Ohio-1@).
    attachedTo :: Core.Maybe Core.Text,
    -- | The region and Availability Zone where the static IP was created.
    location :: Core.Maybe ResourceLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isAttached', 'staticIp_isAttached' - A Boolean value indicating whether the static IP is attached.
--
-- 'createdAt', 'staticIp_createdAt' - The timestamp when the static IP was created (e.g., @1479735304.222@).
--
-- 'arn', 'staticIp_arn' - The Amazon Resource Name (ARN) of the static IP (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:StaticIp\/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@).
--
-- 'resourceType', 'staticIp_resourceType' - The resource type (usually @StaticIp@).
--
-- 'supportCode', 'staticIp_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'name', 'staticIp_name' - The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@).
--
-- 'ipAddress', 'staticIp_ipAddress' - The static IP address.
--
-- 'attachedTo', 'staticIp_attachedTo' - The instance where the static IP is attached (e.g.,
-- @Amazon_Linux-1GB-Ohio-1@).
--
-- 'location', 'staticIp_location' - The region and Availability Zone where the static IP was created.
newStaticIp ::
  StaticIp
newStaticIp =
  StaticIp'
    { isAttached = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      name = Core.Nothing,
      ipAddress = Core.Nothing,
      attachedTo = Core.Nothing,
      location = Core.Nothing
    }

-- | A Boolean value indicating whether the static IP is attached.
staticIp_isAttached :: Lens.Lens' StaticIp (Core.Maybe Core.Bool)
staticIp_isAttached = Lens.lens (\StaticIp' {isAttached} -> isAttached) (\s@StaticIp' {} a -> s {isAttached = a} :: StaticIp)

-- | The timestamp when the static IP was created (e.g., @1479735304.222@).
staticIp_createdAt :: Lens.Lens' StaticIp (Core.Maybe Core.UTCTime)
staticIp_createdAt = Lens.lens (\StaticIp' {createdAt} -> createdAt) (\s@StaticIp' {} a -> s {createdAt = a} :: StaticIp) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the static IP (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:StaticIp\/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@).
staticIp_arn :: Lens.Lens' StaticIp (Core.Maybe Core.Text)
staticIp_arn = Lens.lens (\StaticIp' {arn} -> arn) (\s@StaticIp' {} a -> s {arn = a} :: StaticIp)

-- | The resource type (usually @StaticIp@).
staticIp_resourceType :: Lens.Lens' StaticIp (Core.Maybe ResourceType)
staticIp_resourceType = Lens.lens (\StaticIp' {resourceType} -> resourceType) (\s@StaticIp' {} a -> s {resourceType = a} :: StaticIp)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
staticIp_supportCode :: Lens.Lens' StaticIp (Core.Maybe Core.Text)
staticIp_supportCode = Lens.lens (\StaticIp' {supportCode} -> supportCode) (\s@StaticIp' {} a -> s {supportCode = a} :: StaticIp)

-- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@).
staticIp_name :: Lens.Lens' StaticIp (Core.Maybe Core.Text)
staticIp_name = Lens.lens (\StaticIp' {name} -> name) (\s@StaticIp' {} a -> s {name = a} :: StaticIp)

-- | The static IP address.
staticIp_ipAddress :: Lens.Lens' StaticIp (Core.Maybe Core.Text)
staticIp_ipAddress = Lens.lens (\StaticIp' {ipAddress} -> ipAddress) (\s@StaticIp' {} a -> s {ipAddress = a} :: StaticIp)

-- | The instance where the static IP is attached (e.g.,
-- @Amazon_Linux-1GB-Ohio-1@).
staticIp_attachedTo :: Lens.Lens' StaticIp (Core.Maybe Core.Text)
staticIp_attachedTo = Lens.lens (\StaticIp' {attachedTo} -> attachedTo) (\s@StaticIp' {} a -> s {attachedTo = a} :: StaticIp)

-- | The region and Availability Zone where the static IP was created.
staticIp_location :: Lens.Lens' StaticIp (Core.Maybe ResourceLocation)
staticIp_location = Lens.lens (\StaticIp' {location} -> location) (\s@StaticIp' {} a -> s {location = a} :: StaticIp)

instance Core.FromJSON StaticIp where
  parseJSON =
    Core.withObject
      "StaticIp"
      ( \x ->
          StaticIp'
            Core.<$> (x Core..:? "isAttached")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "ipAddress")
            Core.<*> (x Core..:? "attachedTo")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable StaticIp

instance Core.NFData StaticIp
