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
-- Module      : Network.AWS.Lightsail.Types.KeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.KeyPair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag

-- | Describes the SSH key pair.
--
-- /See:/ 'newKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The timestamp when the key pair was created (e.g., @1479816991.349@).
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the key pair (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
    arn :: Core.Maybe Core.Text,
    -- | The resource type (usually @KeyPair@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The friendly name of the SSH key pair.
    name :: Core.Maybe Core.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | The RSA fingerprint of the key pair.
    fingerprint :: Core.Maybe Core.Text,
    -- | The region name and Availability Zone where the key pair was created.
    location :: Core.Maybe ResourceLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'keyPair_createdAt' - The timestamp when the key pair was created (e.g., @1479816991.349@).
--
-- 'arn', 'keyPair_arn' - The Amazon Resource Name (ARN) of the key pair (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
--
-- 'resourceType', 'keyPair_resourceType' - The resource type (usually @KeyPair@).
--
-- 'supportCode', 'keyPair_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'name', 'keyPair_name' - The friendly name of the SSH key pair.
--
-- 'tags', 'keyPair_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'fingerprint', 'keyPair_fingerprint' - The RSA fingerprint of the key pair.
--
-- 'location', 'keyPair_location' - The region name and Availability Zone where the key pair was created.
newKeyPair ::
  KeyPair
newKeyPair =
  KeyPair'
    { createdAt = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      fingerprint = Core.Nothing,
      location = Core.Nothing
    }

-- | The timestamp when the key pair was created (e.g., @1479816991.349@).
keyPair_createdAt :: Lens.Lens' KeyPair (Core.Maybe Core.UTCTime)
keyPair_createdAt = Lens.lens (\KeyPair' {createdAt} -> createdAt) (\s@KeyPair' {} a -> s {createdAt = a} :: KeyPair) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the key pair (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
keyPair_arn :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
keyPair_arn = Lens.lens (\KeyPair' {arn} -> arn) (\s@KeyPair' {} a -> s {arn = a} :: KeyPair)

-- | The resource type (usually @KeyPair@).
keyPair_resourceType :: Lens.Lens' KeyPair (Core.Maybe ResourceType)
keyPair_resourceType = Lens.lens (\KeyPair' {resourceType} -> resourceType) (\s@KeyPair' {} a -> s {resourceType = a} :: KeyPair)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
keyPair_supportCode :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
keyPair_supportCode = Lens.lens (\KeyPair' {supportCode} -> supportCode) (\s@KeyPair' {} a -> s {supportCode = a} :: KeyPair)

-- | The friendly name of the SSH key pair.
keyPair_name :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
keyPair_name = Lens.lens (\KeyPair' {name} -> name) (\s@KeyPair' {} a -> s {name = a} :: KeyPair)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
keyPair_tags :: Lens.Lens' KeyPair (Core.Maybe [Tag])
keyPair_tags = Lens.lens (\KeyPair' {tags} -> tags) (\s@KeyPair' {} a -> s {tags = a} :: KeyPair) Core.. Lens.mapping Lens._Coerce

-- | The RSA fingerprint of the key pair.
keyPair_fingerprint :: Lens.Lens' KeyPair (Core.Maybe Core.Text)
keyPair_fingerprint = Lens.lens (\KeyPair' {fingerprint} -> fingerprint) (\s@KeyPair' {} a -> s {fingerprint = a} :: KeyPair)

-- | The region name and Availability Zone where the key pair was created.
keyPair_location :: Lens.Lens' KeyPair (Core.Maybe ResourceLocation)
keyPair_location = Lens.lens (\KeyPair' {location} -> location) (\s@KeyPair' {} a -> s {location = a} :: KeyPair)

instance Core.FromJSON KeyPair where
  parseJSON =
    Core.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Core.<$> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fingerprint")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable KeyPair

instance Core.NFData KeyPair
