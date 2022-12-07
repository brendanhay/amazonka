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
-- Module      : Amazonka.Lightsail.Types.Domain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Domain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.DomainEntry
import Amazonka.Lightsail.Types.RegisteredDomainDelegationInfo
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a domain where you are storing recordsets.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the state of the Route 53 domain delegation to
    -- a Lightsail DNS zone.
    registeredDomainDelegationInfo :: Prelude.Maybe RegisteredDomainDelegationInfo,
    -- | The Amazon Resource Name (ARN) of the domain recordset (e.g.,
    -- @arn:aws:lightsail:global:123456789101:Domain\/824cede0-abc7-4f84-8dbc-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region and Availability Zones where the domain recordset was
    -- created.
    location :: Prelude.Maybe ResourceLocation,
    -- | An array of key-value pairs containing information about the domain
    -- entries.
    domainEntries :: Prelude.Maybe [DomainEntry],
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The date when the domain recordset was created.
    createdAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Domain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'domain_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'resourceType', 'domain_resourceType' - The resource type.
--
-- 'name', 'domain_name' - The name of the domain.
--
-- 'registeredDomainDelegationInfo', 'domain_registeredDomainDelegationInfo' - An object that describes the state of the Route 53 domain delegation to
-- a Lightsail DNS zone.
--
-- 'arn', 'domain_arn' - The Amazon Resource Name (ARN) of the domain recordset (e.g.,
-- @arn:aws:lightsail:global:123456789101:Domain\/824cede0-abc7-4f84-8dbc-12345EXAMPLE@).
--
-- 'location', 'domain_location' - The AWS Region and Availability Zones where the domain recordset was
-- created.
--
-- 'domainEntries', 'domain_domainEntries' - An array of key-value pairs containing information about the domain
-- entries.
--
-- 'supportCode', 'domain_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'createdAt', 'domain_createdAt' - The date when the domain recordset was created.
newDomain ::
  Domain
newDomain =
  Domain'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      registeredDomainDelegationInfo = Prelude.Nothing,
      arn = Prelude.Nothing,
      location = Prelude.Nothing,
      domainEntries = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
domain_tags :: Lens.Lens' Domain (Prelude.Maybe [Tag])
domain_tags = Lens.lens (\Domain' {tags} -> tags) (\s@Domain' {} a -> s {tags = a} :: Domain) Prelude.. Lens.mapping Lens.coerced

-- | The resource type.
domain_resourceType :: Lens.Lens' Domain (Prelude.Maybe ResourceType)
domain_resourceType = Lens.lens (\Domain' {resourceType} -> resourceType) (\s@Domain' {} a -> s {resourceType = a} :: Domain)

-- | The name of the domain.
domain_name :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_name = Lens.lens (\Domain' {name} -> name) (\s@Domain' {} a -> s {name = a} :: Domain)

-- | An object that describes the state of the Route 53 domain delegation to
-- a Lightsail DNS zone.
domain_registeredDomainDelegationInfo :: Lens.Lens' Domain (Prelude.Maybe RegisteredDomainDelegationInfo)
domain_registeredDomainDelegationInfo = Lens.lens (\Domain' {registeredDomainDelegationInfo} -> registeredDomainDelegationInfo) (\s@Domain' {} a -> s {registeredDomainDelegationInfo = a} :: Domain)

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g.,
-- @arn:aws:lightsail:global:123456789101:Domain\/824cede0-abc7-4f84-8dbc-12345EXAMPLE@).
domain_arn :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_arn = Lens.lens (\Domain' {arn} -> arn) (\s@Domain' {} a -> s {arn = a} :: Domain)

-- | The AWS Region and Availability Zones where the domain recordset was
-- created.
domain_location :: Lens.Lens' Domain (Prelude.Maybe ResourceLocation)
domain_location = Lens.lens (\Domain' {location} -> location) (\s@Domain' {} a -> s {location = a} :: Domain)

-- | An array of key-value pairs containing information about the domain
-- entries.
domain_domainEntries :: Lens.Lens' Domain (Prelude.Maybe [DomainEntry])
domain_domainEntries = Lens.lens (\Domain' {domainEntries} -> domainEntries) (\s@Domain' {} a -> s {domainEntries = a} :: Domain) Prelude.. Lens.mapping Lens.coerced

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
domain_supportCode :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_supportCode = Lens.lens (\Domain' {supportCode} -> supportCode) (\s@Domain' {} a -> s {supportCode = a} :: Domain)

-- | The date when the domain recordset was created.
domain_createdAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_createdAt = Lens.lens (\Domain' {createdAt} -> createdAt) (\s@Domain' {} a -> s {createdAt = a} :: Domain) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Domain where
  parseJSON =
    Data.withObject
      "Domain"
      ( \x ->
          Domain'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "registeredDomainDelegationInfo")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "domainEntries" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "createdAt")
      )

instance Prelude.Hashable Domain where
  hashWithSalt _salt Domain' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` registeredDomainDelegationInfo
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` domainEntries
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Domain where
  rnf Domain' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf registeredDomainDelegationInfo
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf domainEntries
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf createdAt
