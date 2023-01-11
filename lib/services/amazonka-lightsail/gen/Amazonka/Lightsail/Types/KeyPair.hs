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
-- Module      : Amazonka.Lightsail.Types.KeyPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.KeyPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an SSH key pair.
--
-- /See:/ 'newKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The Amazon Resource Name (ARN) of the key pair (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the key pair was created (e.g., @1479816991.349@).
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The RSA fingerprint of the key pair.
    fingerprint :: Prelude.Maybe Prelude.Text,
    -- | The region name and Availability Zone where the key pair was created.
    location :: Prelude.Maybe ResourceLocation,
    -- | The friendly name of the SSH key pair.
    name :: Prelude.Maybe Prelude.Text,
    -- | The resource type (usually @KeyPair@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag]
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
-- 'arn', 'keyPair_arn' - The Amazon Resource Name (ARN) of the key pair (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
--
-- 'createdAt', 'keyPair_createdAt' - The timestamp when the key pair was created (e.g., @1479816991.349@).
--
-- 'fingerprint', 'keyPair_fingerprint' - The RSA fingerprint of the key pair.
--
-- 'location', 'keyPair_location' - The region name and Availability Zone where the key pair was created.
--
-- 'name', 'keyPair_name' - The friendly name of the SSH key pair.
--
-- 'resourceType', 'keyPair_resourceType' - The resource type (usually @KeyPair@).
--
-- 'supportCode', 'keyPair_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'tags', 'keyPair_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newKeyPair ::
  KeyPair
newKeyPair =
  KeyPair'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      fingerprint = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the key pair (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:KeyPair\/05859e3d-331d-48ba-9034-12345EXAMPLE@).
keyPair_arn :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_arn = Lens.lens (\KeyPair' {arn} -> arn) (\s@KeyPair' {} a -> s {arn = a} :: KeyPair)

-- | The timestamp when the key pair was created (e.g., @1479816991.349@).
keyPair_createdAt :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.UTCTime)
keyPair_createdAt = Lens.lens (\KeyPair' {createdAt} -> createdAt) (\s@KeyPair' {} a -> s {createdAt = a} :: KeyPair) Prelude.. Lens.mapping Data._Time

-- | The RSA fingerprint of the key pair.
keyPair_fingerprint :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_fingerprint = Lens.lens (\KeyPair' {fingerprint} -> fingerprint) (\s@KeyPair' {} a -> s {fingerprint = a} :: KeyPair)

-- | The region name and Availability Zone where the key pair was created.
keyPair_location :: Lens.Lens' KeyPair (Prelude.Maybe ResourceLocation)
keyPair_location = Lens.lens (\KeyPair' {location} -> location) (\s@KeyPair' {} a -> s {location = a} :: KeyPair)

-- | The friendly name of the SSH key pair.
keyPair_name :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_name = Lens.lens (\KeyPair' {name} -> name) (\s@KeyPair' {} a -> s {name = a} :: KeyPair)

-- | The resource type (usually @KeyPair@).
keyPair_resourceType :: Lens.Lens' KeyPair (Prelude.Maybe ResourceType)
keyPair_resourceType = Lens.lens (\KeyPair' {resourceType} -> resourceType) (\s@KeyPair' {} a -> s {resourceType = a} :: KeyPair)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
keyPair_supportCode :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_supportCode = Lens.lens (\KeyPair' {supportCode} -> supportCode) (\s@KeyPair' {} a -> s {supportCode = a} :: KeyPair)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
keyPair_tags :: Lens.Lens' KeyPair (Prelude.Maybe [Tag])
keyPair_tags = Lens.lens (\KeyPair' {tags} -> tags) (\s@KeyPair' {} a -> s {tags = a} :: KeyPair) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON KeyPair where
  parseJSON =
    Data.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "fingerprint")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable KeyPair where
  hashWithSalt _salt KeyPair' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` fingerprint
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` tags

instance Prelude.NFData KeyPair where
  rnf KeyPair' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf fingerprint
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf tags
