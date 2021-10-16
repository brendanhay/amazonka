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
import qualified Network.AWS.Prelude as Prelude

-- | Describes an SSH key pair.
--
-- /See:/ 'newKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The timestamp when the key pair was created (e.g., @1479816991.349@).
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the key pair (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The resource type (usually @KeyPair@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The friendly name of the SSH key pair.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The RSA fingerprint of the key pair.
    fingerprint :: Prelude.Maybe Prelude.Text,
    -- | The region name and Availability Zone where the key pair was created.
    location :: Prelude.Maybe ResourceLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'supportCode', 'keyPair_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'resourceType', 'keyPair_resourceType' - The resource type (usually @KeyPair@).
--
-- 'name', 'keyPair_name' - The friendly name of the SSH key pair.
--
-- 'tags', 'keyPair_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'fingerprint', 'keyPair_fingerprint' - The RSA fingerprint of the key pair.
--
-- 'location', 'keyPair_location' - The region name and Availability Zone where the key pair was created.
newKeyPair ::
  KeyPair
newKeyPair =
  KeyPair'
    { createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      fingerprint = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The timestamp when the key pair was created (e.g., @1479816991.349@).
keyPair_createdAt :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.UTCTime)
keyPair_createdAt = Lens.lens (\KeyPair' {createdAt} -> createdAt) (\s@KeyPair' {} a -> s {createdAt = a} :: KeyPair) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the key pair (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
keyPair_arn :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_arn = Lens.lens (\KeyPair' {arn} -> arn) (\s@KeyPair' {} a -> s {arn = a} :: KeyPair)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
keyPair_supportCode :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_supportCode = Lens.lens (\KeyPair' {supportCode} -> supportCode) (\s@KeyPair' {} a -> s {supportCode = a} :: KeyPair)

-- | The resource type (usually @KeyPair@).
keyPair_resourceType :: Lens.Lens' KeyPair (Prelude.Maybe ResourceType)
keyPair_resourceType = Lens.lens (\KeyPair' {resourceType} -> resourceType) (\s@KeyPair' {} a -> s {resourceType = a} :: KeyPair)

-- | The friendly name of the SSH key pair.
keyPair_name :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_name = Lens.lens (\KeyPair' {name} -> name) (\s@KeyPair' {} a -> s {name = a} :: KeyPair)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
keyPair_tags :: Lens.Lens' KeyPair (Prelude.Maybe [Tag])
keyPair_tags = Lens.lens (\KeyPair' {tags} -> tags) (\s@KeyPair' {} a -> s {tags = a} :: KeyPair) Prelude.. Lens.mapping Lens._Coerce

-- | The RSA fingerprint of the key pair.
keyPair_fingerprint :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_fingerprint = Lens.lens (\KeyPair' {fingerprint} -> fingerprint) (\s@KeyPair' {} a -> s {fingerprint = a} :: KeyPair)

-- | The region name and Availability Zone where the key pair was created.
keyPair_location :: Lens.Lens' KeyPair (Prelude.Maybe ResourceLocation)
keyPair_location = Lens.lens (\KeyPair' {location} -> location) (\s@KeyPair' {} a -> s {location = a} :: KeyPair)

instance Core.FromJSON KeyPair where
  parseJSON =
    Core.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Prelude.<$> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "fingerprint")
            Prelude.<*> (x Core..:? "location")
      )

instance Prelude.Hashable KeyPair

instance Prelude.NFData KeyPair
