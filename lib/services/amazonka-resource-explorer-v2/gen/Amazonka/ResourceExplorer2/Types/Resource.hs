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
-- Module      : Amazonka.ResourceExplorer2.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceExplorer2.Types.ResourceProperty

-- | A resource in Amazon Web Services that Amazon Web Services Resource
-- Explorer has discovered, and for which it has stored information in the
-- index of the Amazon Web Services Region that contains the resource.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The type of the resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The date and time that Resource Explorer last queried this resource and
    -- updated the index with the latest information about the resource.
    lastReportedAt :: Prelude.Maybe Data.POSIX,
    -- | A structure with additional type-specific details about the resource.
    -- These properties can be added by turning on integration between Resource
    -- Explorer and other Amazon Web Services services.
    properties :: Prelude.Maybe [ResourceProperty],
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Service that owns the resource and is responsible for
    -- creating and updating it.
    service :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region in which the resource was created and
    -- exists.
    region :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account that owns the resource.
    owningAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resource_resourceType' - The type of the resource.
--
-- 'lastReportedAt', 'resource_lastReportedAt' - The date and time that Resource Explorer last queried this resource and
-- updated the index with the latest information about the resource.
--
-- 'properties', 'resource_properties' - A structure with additional type-specific details about the resource.
-- These properties can be added by turning on integration between Resource
-- Explorer and other Amazon Web Services services.
--
-- 'arn', 'resource_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the resource.
--
-- 'service', 'resource_service' - The Amazon Web Service that owns the resource and is responsible for
-- creating and updating it.
--
-- 'region', 'resource_region' - The Amazon Web Services Region in which the resource was created and
-- exists.
--
-- 'owningAccountId', 'resource_owningAccountId' - The Amazon Web Services account that owns the resource.
newResource ::
  Resource
newResource =
  Resource'
    { resourceType = Prelude.Nothing,
      lastReportedAt = Prelude.Nothing,
      properties = Prelude.Nothing,
      arn = Prelude.Nothing,
      service = Prelude.Nothing,
      region = Prelude.Nothing,
      owningAccountId = Prelude.Nothing
    }

-- | The type of the resource.
resource_resourceType :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceType = Lens.lens (\Resource' {resourceType} -> resourceType) (\s@Resource' {} a -> s {resourceType = a} :: Resource)

-- | The date and time that Resource Explorer last queried this resource and
-- updated the index with the latest information about the resource.
resource_lastReportedAt :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_lastReportedAt = Lens.lens (\Resource' {lastReportedAt} -> lastReportedAt) (\s@Resource' {} a -> s {lastReportedAt = a} :: Resource) Prelude.. Lens.mapping Data._Time

-- | A structure with additional type-specific details about the resource.
-- These properties can be added by turning on integration between Resource
-- Explorer and other Amazon Web Services services.
resource_properties :: Lens.Lens' Resource (Prelude.Maybe [ResourceProperty])
resource_properties = Lens.lens (\Resource' {properties} -> properties) (\s@Resource' {} a -> s {properties = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The Amazon Web Service that owns the resource and is responsible for
-- creating and updating it.
resource_service :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_service = Lens.lens (\Resource' {service} -> service) (\s@Resource' {} a -> s {service = a} :: Resource)

-- | The Amazon Web Services Region in which the resource was created and
-- exists.
resource_region :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_region = Lens.lens (\Resource' {region} -> region) (\s@Resource' {} a -> s {region = a} :: Resource)

-- | The Amazon Web Services account that owns the resource.
resource_owningAccountId :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_owningAccountId = Lens.lens (\Resource' {owningAccountId} -> owningAccountId) (\s@Resource' {} a -> s {owningAccountId = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "LastReportedAt")
            Prelude.<*> (x Data..:? "Properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Service")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "OwningAccountId")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` lastReportedAt
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` owningAccountId

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf lastReportedAt
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf owningAccountId
