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
-- Module      : Amazonka.Lightsail.Types.StaticIp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.StaticIp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes a static IP.
--
-- /See:/ 'newStaticIp' smart constructor.
data StaticIp = StaticIp'
  { -- | The Amazon Resource Name (ARN) of the static IP (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:StaticIp\/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The instance where the static IP is attached (e.g.,
    -- @Amazon_Linux-1GB-Ohio-1@).
    attachedTo :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the static IP was created (e.g., @1479735304.222@).
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The static IP address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the static IP is attached.
    isAttached :: Prelude.Maybe Prelude.Bool,
    -- | The region and Availability Zone where the static IP was created.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The resource type (usually @StaticIp@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'staticIp_arn' - The Amazon Resource Name (ARN) of the static IP (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:StaticIp\/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@).
--
-- 'attachedTo', 'staticIp_attachedTo' - The instance where the static IP is attached (e.g.,
-- @Amazon_Linux-1GB-Ohio-1@).
--
-- 'createdAt', 'staticIp_createdAt' - The timestamp when the static IP was created (e.g., @1479735304.222@).
--
-- 'ipAddress', 'staticIp_ipAddress' - The static IP address.
--
-- 'isAttached', 'staticIp_isAttached' - A Boolean value indicating whether the static IP is attached.
--
-- 'location', 'staticIp_location' - The region and Availability Zone where the static IP was created.
--
-- 'name', 'staticIp_name' - The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@).
--
-- 'resourceType', 'staticIp_resourceType' - The resource type (usually @StaticIp@).
--
-- 'supportCode', 'staticIp_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
newStaticIp ::
  StaticIp
newStaticIp =
  StaticIp'
    { arn = Prelude.Nothing,
      attachedTo = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      isAttached = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the static IP (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:StaticIp\/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@).
staticIp_arn :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.Text)
staticIp_arn = Lens.lens (\StaticIp' {arn} -> arn) (\s@StaticIp' {} a -> s {arn = a} :: StaticIp)

-- | The instance where the static IP is attached (e.g.,
-- @Amazon_Linux-1GB-Ohio-1@).
staticIp_attachedTo :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.Text)
staticIp_attachedTo = Lens.lens (\StaticIp' {attachedTo} -> attachedTo) (\s@StaticIp' {} a -> s {attachedTo = a} :: StaticIp)

-- | The timestamp when the static IP was created (e.g., @1479735304.222@).
staticIp_createdAt :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.UTCTime)
staticIp_createdAt = Lens.lens (\StaticIp' {createdAt} -> createdAt) (\s@StaticIp' {} a -> s {createdAt = a} :: StaticIp) Prelude.. Lens.mapping Data._Time

-- | The static IP address.
staticIp_ipAddress :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.Text)
staticIp_ipAddress = Lens.lens (\StaticIp' {ipAddress} -> ipAddress) (\s@StaticIp' {} a -> s {ipAddress = a} :: StaticIp)

-- | A Boolean value indicating whether the static IP is attached.
staticIp_isAttached :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.Bool)
staticIp_isAttached = Lens.lens (\StaticIp' {isAttached} -> isAttached) (\s@StaticIp' {} a -> s {isAttached = a} :: StaticIp)

-- | The region and Availability Zone where the static IP was created.
staticIp_location :: Lens.Lens' StaticIp (Prelude.Maybe ResourceLocation)
staticIp_location = Lens.lens (\StaticIp' {location} -> location) (\s@StaticIp' {} a -> s {location = a} :: StaticIp)

-- | The name of the static IP (e.g., @StaticIP-Ohio-EXAMPLE@).
staticIp_name :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.Text)
staticIp_name = Lens.lens (\StaticIp' {name} -> name) (\s@StaticIp' {} a -> s {name = a} :: StaticIp)

-- | The resource type (usually @StaticIp@).
staticIp_resourceType :: Lens.Lens' StaticIp (Prelude.Maybe ResourceType)
staticIp_resourceType = Lens.lens (\StaticIp' {resourceType} -> resourceType) (\s@StaticIp' {} a -> s {resourceType = a} :: StaticIp)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
staticIp_supportCode :: Lens.Lens' StaticIp (Prelude.Maybe Prelude.Text)
staticIp_supportCode = Lens.lens (\StaticIp' {supportCode} -> supportCode) (\s@StaticIp' {} a -> s {supportCode = a} :: StaticIp)

instance Data.FromJSON StaticIp where
  parseJSON =
    Data.withObject
      "StaticIp"
      ( \x ->
          StaticIp'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "attachedTo")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "ipAddress")
            Prelude.<*> (x Data..:? "isAttached")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "supportCode")
      )

instance Prelude.Hashable StaticIp where
  hashWithSalt _salt StaticIp' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` attachedTo
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` isAttached
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` supportCode

instance Prelude.NFData StaticIp where
  rnf StaticIp' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf attachedTo
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf isAttached
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf supportCode
