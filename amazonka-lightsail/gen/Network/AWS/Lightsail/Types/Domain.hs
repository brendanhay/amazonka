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
-- Module      : Network.AWS.Lightsail.Types.Domain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Domain where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes a domain where you are storing recordsets in Lightsail.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The date when the domain recordset was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the domain recordset (e.g.,
    -- @arn:aws:lightsail:global:123456789101:Domain\/824cede0-abc7-4f84-8dbc-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The AWS Region and Availability Zones where the domain recordset was
    -- created.
    location :: Prelude.Maybe ResourceLocation,
    -- | An array of key-value pairs containing information about the domain
    -- entries.
    domainEntries :: Prelude.Maybe [DomainEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Domain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'domain_createdAt' - The date when the domain recordset was created.
--
-- 'arn', 'domain_arn' - The Amazon Resource Name (ARN) of the domain recordset (e.g.,
-- @arn:aws:lightsail:global:123456789101:Domain\/824cede0-abc7-4f84-8dbc-12345EXAMPLE@).
--
-- 'resourceType', 'domain_resourceType' - The resource type.
--
-- 'supportCode', 'domain_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'name', 'domain_name' - The name of the domain.
--
-- 'tags', 'domain_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'location', 'domain_location' - The AWS Region and Availability Zones where the domain recordset was
-- created.
--
-- 'domainEntries', 'domain_domainEntries' - An array of key-value pairs containing information about the domain
-- entries.
newDomain ::
  Domain
newDomain =
  Domain'
    { createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      location = Prelude.Nothing,
      domainEntries = Prelude.Nothing
    }

-- | The date when the domain recordset was created.
domain_createdAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_createdAt = Lens.lens (\Domain' {createdAt} -> createdAt) (\s@Domain' {} a -> s {createdAt = a} :: Domain) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g.,
-- @arn:aws:lightsail:global:123456789101:Domain\/824cede0-abc7-4f84-8dbc-12345EXAMPLE@).
domain_arn :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_arn = Lens.lens (\Domain' {arn} -> arn) (\s@Domain' {} a -> s {arn = a} :: Domain)

-- | The resource type.
domain_resourceType :: Lens.Lens' Domain (Prelude.Maybe ResourceType)
domain_resourceType = Lens.lens (\Domain' {resourceType} -> resourceType) (\s@Domain' {} a -> s {resourceType = a} :: Domain)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
domain_supportCode :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_supportCode = Lens.lens (\Domain' {supportCode} -> supportCode) (\s@Domain' {} a -> s {supportCode = a} :: Domain)

-- | The name of the domain.
domain_name :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_name = Lens.lens (\Domain' {name} -> name) (\s@Domain' {} a -> s {name = a} :: Domain)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
domain_tags :: Lens.Lens' Domain (Prelude.Maybe [Tag])
domain_tags = Lens.lens (\Domain' {tags} -> tags) (\s@Domain' {} a -> s {tags = a} :: Domain) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS Region and Availability Zones where the domain recordset was
-- created.
domain_location :: Lens.Lens' Domain (Prelude.Maybe ResourceLocation)
domain_location = Lens.lens (\Domain' {location} -> location) (\s@Domain' {} a -> s {location = a} :: Domain)

-- | An array of key-value pairs containing information about the domain
-- entries.
domain_domainEntries :: Lens.Lens' Domain (Prelude.Maybe [DomainEntry])
domain_domainEntries = Lens.lens (\Domain' {domainEntries} -> domainEntries) (\s@Domain' {} a -> s {domainEntries = a} :: Domain) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Domain where
  parseJSON =
    Prelude.withObject
      "Domain"
      ( \x ->
          Domain'
            Prelude.<$> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> ( x Prelude..:? "domainEntries"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Domain

instance Prelude.NFData Domain
